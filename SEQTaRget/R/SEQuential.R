#' @title SEQuential trial emulation
#' @description `SEQuential` is an all-in-one API to SEQuential analysis, returning a SEQoutput object of results. More specific examples can be found on pages at <https://causalinference.github.io/SEQTaRget/>
#' 
#' @details Implementation of sequential trial emulation for the analysis of observational databases. 
#' The SEQuential software accommodates time-varying treatments and confounders, as well as binary 
#' and failure time outcomes. SEQ allows to compare both static and dynamic strategies, 
#' can be used to estimate observational analogs of intention-to-treat 
#' and per-protocol effects, and can adjust for potential selection bias induced by losses-to-follow-up.
#'
#' @param data data.frame or data.table, will preform expansion according to arguments passed through the `options` argument
#' @param id.col String: column name of the id column
#' @param time.col String: column name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param treatment.col String: column name of the treatment column
#' @param outcome.col String: column name of the outcome column
#' @param time_varying.cols List: column names for time varying columns
#' @param fixed.cols List: column names for fixed columns
#' @param method String: method of analysis to preform; should be one of `"ITT"`, `"dose-response"`, or `"censoring"`
#' @param options List: optional list of parameters from [SEQopts()]
#' @param verbose Logical: if TRUE, cats progress to console, default is `TRUE`
#'
#' @import data.table doRNG
#' @importFrom methods is
#' @importFrom future plan
#' @importFrom doFuture registerDoFuture
#' @importFrom stats complete.cases
#' 
#' @returns An S4 object of class SEQoutput
#' @examples
#' \donttest{
#' data <- SEQdata
#' model <- SEQuential(data, id.col = "ID", 
#'                           time.col = "time", 
#'                           eligible.col = "eligible",
#'                           treatment.col = "tx_init",
#'                           outcome.col = "outcome",
#'                           time_varying.cols = c("N", "L", "P"),
#'                           fixed.cols = "sex",
#'                           method = "ITT", 
#'                           options = SEQopts())
#' }
#'
#' @export
SEQuential <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time_varying.cols = list(), fixed.cols = list(), method, options, verbose = TRUE) {
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
    method, options, verbose
  )
  params <- parameter.simplifier(params)

  if (is.na(params@covariates)) params@covariates <- create.default.covariates(params)
  if (params@weighted && params@method != "ITT") {
    if (is.na(params@numerator)) params@numerator <- create.default.weight.covariates(params, "numerator")
    if (is.na(params@denominator)) params@denominator <- create.default.weight.covariates(params, "denominator")
  }
  if (params@LTFU) {
    if (is.na(params@cense.numerator)) params@cense.numerator <- create.default.LTFU.covariates(params, "numerator")
    if (is.na(params@cense.denominator)) params@cense.denominator <- create.default.LTFU.covariates(params, "denominator")
  }
  if (!is.na(params@visit)) {
    # Visit uses the same params as LTFU unless otherwise specified
    if (is.na(params@visit.numerator)) params@visit.numerator <- create.default.LTFU.covariates(params, "numerator")
    if (is.na(params@visit.denominator)) params@visit.denominator <- create.default.LTFU.covariates(params, "denominator")
  }

  # Parallel Setup ==================================
  if (params@parallel) {
    registerDoFuture()
    registerDoRNG()
    plan("multisession", workers = params@ncores, gc = TRUE)
  }
  
  # Data Checking ====================================
  needed <- c(params@id, params@time, params@eligible, params@treatment, params@outcome, 
              unlist(params@time_varying), unlist(params@fixed), 
              params@cense, params@compevent, params@deviation.col, 
              unlist(params@excused.cols), params@subgroup)
  needed <- needed[!is.na(needed)]
  
  if (length(colnames(data)) > length(needed)) {
    if (verbose) cat("Non-required columns provided, pruning for efficiency\n")
    data <- data[, needed, with = FALSE]
    if (verbose) cat("Pruned\n") else warning("Non-required columns provided and pruned for efficiency\n")
  }
  
  if (nrow(data[!complete.cases(data)]) > 0) stop("Data contains NA values, please fix before modeling")
  if (nrow(copy(data)[max(get(params@time)) > .N, .SD, by = eval(params@id)]) > 0) {
    if (verbose) cat("Non zero-indexed time identified. Attempting Repair...\n")
    data[, get(params@time) := 0:(.N - 1), by = eval(params@id)]
    if (verbose) cat("Repaired\n") else warning("Non zero-indexed time identifed, Repair attempted and succeeded\n")
  }
  # Expansion ==================================================
  if (params@verbose) cat("Expanding Data...\n")
  if (params@multinomial) params@data[!get(params@treatment) %in% params@treat.level, eval(params@eligible) := 0]
  params@DT <- factorize(SEQexpand(params), params)
  params@data <- factorize(params@data, params)
  
  gc()
  if (params@verbose) cat("Expansion Successful\nMoving forward with", params@method, "analysis\n")

  # Switch Diagnostics (Censoring) =============================
  if (method == "censoring") {
    if (!params@deviation) {
      switch.unique <- data[, 'switch' := (get(params@treatment) != shift(get(params@treatment), fill = get(params@treatment)[1])), by = eval(params@id)
                            ][, list(n = .N), by = "switch"]
    } else {
      data[, 'switch' := FALSE]
      for (i in seq_along(params@treat.level)) {
        conditional <- paste0(paste0(params@treatment), "==", params@treat.level[[i]],
                              " & ", params@deviation.col, params@deviation.conditions[[i]])
        
        data[eval(parse(text = conditional)), 'switch' := TRUE]
      }
      switch.unique <- data[, list(n = .N), by = "switch"]
    }
    switch.nonunique <- params@DT[, list(n = .N), by = "switch"]
    
    params@DT[, "switch" := NULL]
  } else switch.unique <- switch.nonunique <- NA

  # Model Dispersion ===========================================
  survival.data <- survival.plot <- survival.ce <- risk <- hazard <- vcov <- outcome <- weights <- list()
  analytic <- internal.analysis(params)
  WDT <- analytic[[1]]$WDT
  
  subgroups <- if (is.na(params@subgroup)) 1L else names(analytic[[1]]$model)
  if (!params@hazard) {
    if (params@verbose) cat(method, "model created successfully\n")

    # Survival Information =======================================
    for (i in seq_along(subgroups)) {
      label <- subgroups[[i]]
      models <- lapply(analytic, function(x) x$model[[i]])
        
      if (params@km.curves) {
        if (is.na(params@subgroup) && params@verbose) cat("Creating Survival curves\n") else cat("Creating Survival Curves for", label, "\n")
        survival <- internal.survival(params, models)
        survival.data[[label]] <- survival$data
        survival.ce[[label]] <- survival$ce.model
        survival.plot[[label]] <- internal.plot(survival$data, params)
        risk[[label]] <- create.risk(survival$data, params) 
      }
      outcome[[label]] <- lapply(models, function(x) x$model)
      weights[[label]] <- lapply(analytic, function(x) x$weighted_stats)
    }
  } else {
    for (i in seq_along(subgroups)) {
      label <- subgroups[[i]]
      models <- lapply(analytic, function(x) x$model[[i]])
      hazard[[label]] <- internal.hazard(models, params)
    }
  }
  
  outcome.unique  <- outcome.nonunique <- c()
  for (i in seq_along(subgroups)) {
    label <- subgroups[[i]]
    filter <- sort(unique(data[[params@subgroup]]))
    outcome.unique[[label]] <- outcome.table(params, type = "unique", filter = filter[[i]])
    outcome.nonunique[[label]] <- outcome.table(params, type = "nonunique", filter = filter[[i]])
  }
  
  # Output ======================================================
  info <- list(outcome.unique = outcome.unique,
               outcome.nonunique = outcome.nonunique,
               switch.unique = switch.unique,
               switch.nonunique = switch.nonunique,
               compevent.unique = if (!is.na(params@compevent)) table(data[[params@compevent]]) else NA,
               compevent.nonunique = if(!is.na(params@compevent)) table(params@DT[!is.na(get(params@outcome)), 
                                                                                  ][[params@compevent]]) else NA)
  
  runtime <- format.time(round(as.numeric(difftime(Sys.time(), time.start, "secs")), 2))
  out <- prepare.output(params, WDT, outcome, weights, hazard, survival.plot, survival.data, survival.ce, risk, runtime, info)

  if (params@verbose) cat("Completed\n")
  plan("sequential")
  return(out)
}
