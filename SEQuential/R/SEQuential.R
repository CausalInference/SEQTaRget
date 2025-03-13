#' SEQuential processing per (paper here?)
#'
#' @param data data.frame or data.table, if not already expanded with \code{SEQexpand}, will preform expansion according to arguments passed to either \code{params} or \code{...}
#' @param id.col String: column name of the id column
#' @param time.col String: column name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param treatment.col String: column name of the treatment column
#' @param outcome.col String: column name of the outcome column
#' @param time_varying.cols List: column names for time varying columns
#' @param fixed.cols List: column names for fixed columns
#' @param method String: method of analysis to preform
#' @param options List: optional list of parameters from \code{SEQOpts}
#'
#' @import data.table doRNG
#' @importFrom methods is
#' @importFrom future plan multisession sequential
#' @importFrom doFuture registerDoFuture
#' @importFrom stats complete.cases
#'
#' @export
SEQuential <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time_varying.cols = list(), fixed.cols = list(), method, options) {
  # Immediate error checking =================================
  if (missing(data)) stop("Data was not supplied")
  if (missing(id.col)) stop("ID column name was not supplied")
  if (missing(time.col)) stop("Time column name was not supplied")
  if (missing(eligible.col)) stop("Eligibility column was not supplied")
  if (missing(treatment.col)) stop("Treatment column was not supplied")
  if (missing(outcome.col)) stop("Outcome column was not supplied")
  if (missing(method)) stop("Method of analysis was not supplied")
  if (!method %in% c("ITT", "dose-response", "censoring")) stop("Method ", method, "is unsupported. Supported methods are:
                                                               'dose-response', 'ITT', and 'censoring'")
  if (length(time_varying.cols) < 1) warning("Time varying columns were not supplied")
  if (length(fixed.cols) < 1) warning("Fixed columns were not supplied")

  cols <- c(id.col, time.col, treatment.col, eligible.col, outcome.col, time_varying.cols, fixed.cols)
  missing.cols <- cols[!cols %in% names(data)]

  if (length(missing.cols) > 0) {
    stop(paste(missing.cols, collapse = ", "), " are missing from supplied data ")
  }
  
  if (FALSE) {
    data <- fread("SEQdata_multitreatment2.csv")
    id.col = "ID"; time.col = "time"; outcome.col = "outcome"; treatment.col = "tx_init"; eligible.col = "eligible"; method = "censoring"
    fixed.cols = "sex"; time_varying.cols = c("N", "L", "P")
    options = SEQopts(treat.level = c(1,2), multinomial = TRUE, weighted = TRUE, weight.preexpansion = TRUE)
  }

  setDT(data)
  setorderv(data, c(id.col, time.col))
  time.start <- Sys.time()

  # Parameter Setup ==================================
  if (!is(options, "SEQopts")) stop("Options should be built from SEQopts()")
  time_varying.cols <- as.list(time_varying.cols)
  fixed.cols <- as.list(fixed.cols)

  params <- parameter.setter(data,
    DT = data.table(), id.col, time.col, eligible.col, outcome.col, treatment.col,
    as.list(time_varying.cols), as.list(fixed.cols),
    method, options
  )
  params <- parameter.simplifier(params)

  if (is.na(params@covariates)) params@covariates <- create.default.covariates(params)
  if (params@weighted & params@method != "ITT") {
    if (is.na(params@numerator)) params@numerator <- create.default.weight.covariates(params, "numerator")
    if (is.na(params@denominator)) params@denominator <- create.default.weight.covariates(params, "denominator")
  }
  if (params@LTFU) {
    if (is.na(params@cense.numerator)) params@cense.numerator <- create.default.LTFU.covariates(params, "numerator")
    if (is.na(params@cense.denominator)) params@cense.denominator <- create.default.LTFU.covariates(params, "denominator")
  }

  # Parallel Setup ==================================
  if (options@parallel) {
    ncores <- NULL
    ncores <- options@ncores
    assign.global(ncores)
    evalq(
      {
        doFuture::registerDoFuture()
        doRNG::registerDoRNG()
        future::plan(future::multisession(workers = ncores), gc = TRUE)
      },
      envir = .GlobalEnv
    )
    rm(ncores, envir = .GlobalEnv)
  }

  # Expansion ==================================================
  cat("Expanding Data...\n")
  if (params@multinomial) params@data[!get(params@treatment) %in% params@treat.level, eval(params@eligible) := 0]
  params@DT <- SEQexpand(params)
  gc()
  cat("Expansion Successful\nMoving forward with", params@method, "analysis\n")

  # Switch Diagnostics (Censoring) =============================
  if (method == "censoring") {
    switch.unique <- table(data[, 'switch' := (get(params@treatment) != shift(get(params@treatment),
                                                                              fill = get(params@treatment)[1])), by = eval(params@id)]$switch)
    switch.nonunique <- table(params@DT[['switch']])
    
    params@DT <- params@DT[, "switch" := NULL]
    params@data <- params@data[get("switch"), eval(params@outcome) := NA]
  } else switch.unique <- switch.nonunique <- NA

  # Model Dispersion ===========================================
  survival.data <- survival.plot <- survival.ce <- risk <- hazard <- vcov <- outcome <- weights <- list()
  analytic <- internal.analysis(params)
  
  subgroups <- if (is.na(params@subgroup)) 1L else names(analytic[[1]]$model)
  if (!params@hazard) {
    cat(method, "model created successfully\n")

    # Survival Information =======================================
    for (i in seq_along(subgroups)) {
      label <- subgroups[[i]]
      models <- lapply(analytic, function(x) x$model[[i]])
        
      if (params@km.curves) {
        if (is.na(params@subgroup)) cat("Creating Survival curves\n") else cat("Creating Survival Curves for", label, "\n")
        survival <- internal.survival(params, models)
        survival.data[[label]] <- survival$data
        survival.ce[[label]] <- survival$ce.model
        survival.plot[[label]] <- internal.plot(survival$data, params)
        risk[[label]] <- create.risk(survival$data) 
      }
      if (params@calculate.var) vcov[[label]] <- models[[1]]$vcov
      outcome[[label]] <- lapply(models, function(x) x$model)
      weights[[label]] <- lapply(analytic, function(x) x$weighted_stats)
    }
  } else {
    for (i in seq_along(subgroups)) {
      label <- paste0(params@subgroup, "_", subgroups[[i]])
      hazard[[label]] <- hazard(analytic, params)
    }
  }
  
  outcome.unique  <- outcome.nonunique <- c()
  for (i in seq_along(subgroups)) {
    label <- subgroups[[i]]
    filter <- sort(unique(data[[params@subgroup]]))
    outcome.unique[[label]] <- if (!is.na(params@subgroup)) table(copy(data)[get(params@subgroup) == filter[[i]], ][[params@outcome]]) else table(copy(data)[[params@outcome]])
    outcome.nonunique[[label]] <- if (!is.na(params@subgroup)) table(copy(params@DT)[get(params@subgroup) == filter[[i]], ][[params@outcome]]) else table(copy(params@DT)[[params@outcome]])
  }
  
  # Output ======================================================
  info <- list(outcome.unique = outcome.unique,
               outcome.nonunique = outcome.nonunique,
               switch.unique = switch.unique,
               switch.nonunique = switch.nonunique,
               compevent.unique = if (!is.na(params@compevent)) table(data[[params@compevent]]) else NA,
               compevent.nonunique = if(!is.na(params@compevent)) table(params@DT[!is.na(get(params@outcome)), 
                                                                                  ][[params@compevent]]) else NA)
  params@DT <- params@data <- data.table()
  runtime <- format.time(round(as.numeric(difftime(Sys.time(), time.start, "secs")), 2))

  out <- prepare.output(params, outcome, weights, hazard, vcov, survival.plot, survival.data, survival.ce, risk, runtime, info)

  cat("Completed\n")
  plan(future::sequential())
  return(out)
}
