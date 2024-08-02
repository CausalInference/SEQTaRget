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
  if (length(time_varying.cols) < 1) warning("Time varying columns was not supplied")
  if (length(fixed.cols) < 1) warning("Fixed columns was not supplied")

  cols <- c(id.col, time.col, treatment.col, eligible.col, outcome.col, time_varying.cols, fixed.cols)
  missing.cols <- cols[!cols %in% names(data)]

  if (length(missing.cols) > 0) {
    stop(paste(missing.cols, collapse = ", "), " are missing from supplied data ")
  }

  setDT(data)
  setorderv(data, c(id.col, time.col))
  time.start <- Sys.time()

  if (FALSE) {
    # Debugging tools ==========================================
    # data <- fread("datagenExcused.csv")
    data <- SEQdata
    id.col <- "ID"
    time.col <- "time"
    eligible.col <- "eligible"
    outcome.col <- "outcome"
    treatment.col <- "tx_init"
    method <- "censoring"
    time_varying.cols <- c("N", "L", "P")
    fixed.cols <- "sex"
    options <- SEQuential::SEQopts(pre.expansion = FALSE, weighted = TRUE, excused = TRUE, excused.col1 = "excusedOne", excused.col0 = "excusedZero")
    test <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", c("N", "L", "P"), "sex", method = "censoring", options)
  }

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
  if (is.na(params@surv)) params@surv <- create.default.survival.covariates(params)
  if (params@weighted) {
    if (is.na(params@numerator)) params@numerator <- create.default.weight.covariates(params, "numerator")
    if (is.na(params@denominator)) params@denominator <- create.default.weight.covariates(params, "denominator")
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
  params@DT <- SEQexpand(params)
  cat("Expansion Successful\nMoving forward with", params@method, "analysis\n")

  # Model Dispersion ===========================================
  outcome <- internal.analysis(params)

  cat(method, "model successfully created\nCreating survival curves\n")
  survival <- internal.survival(params)
  risk <- create.risk(survival$data)

  out <- prepare.output(params, outcome, survival, risk,
    elapsed_time = paste(round(as.numeric(difftime(Sys.time(), time.start, units = "mins")), 2), "minutes")
  )

  plan(future::sequential())
  return(out)
}
