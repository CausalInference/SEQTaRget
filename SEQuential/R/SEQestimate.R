#' Estimate the (very rough) time to run SEQuential analysis on current machine
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
#' @importFrom stats lm rbinom
#' @export

SEQestimate <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time_varying.cols = list(), fixed.cols = list(), method, options) {
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

  data <- copy(setDT(data))
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

  # Model Timing =========================================
  uid <- unique(data[[id.col]])
  nid <- length(uid)
  if (is.infinite(params@max.followup)) maxtime <- max(data[[time.col]]) else maxtime <- params@max.followup
  expandedrows <- ((maxtime+1)^2 + (maxtime+1)) / 2
  maxrows <- nid * expandedrows

  samples <- c(0.05, 0.1, 0.2, 0.3, 0.5)*maxrows
  colspace <- length(c(treatment.col, time.col, time_varying.cols, fixed.cols))
  times <- c()
  for (i in seq_along(samples)) {
    X <- matrix(as.numeric(rbinom(samples[i] * colspace, 1, 0.5)), nrow = samples[i])
    y <- rbinom(samples[i], 1, 0.5)
    times[i] <- system.time({
      fastglm::fastglm(X, y, family = quasibinomial())
    })['elapsed']
  }

  fit <- lm(time ~ sampleSize, data.frame(sampleSize = samples, time = times))
  modelTime <- as.numeric(predict(fit, newdata = data.frame(sampleSize = maxrows)))

  # Expansion Timing ======================================
  data <- data[get(params@id) %in% sample(uid, nid*0.1)]
  expansionTime <- system.time({
    data <- data[get(params@eligible) == 1, list(period = Map(seq, get(params@time), table(data[[params@id]])[.GRP] - 1)), by = eval(params@id),
                 ][, cbind(.SD, trial = rowid(get(params@id)) - 1)
                   ][, list(period = unlist(.SD)), by = c(eval(params@id), "trial")]
  })['elapsed'] * 10

  # Total Time ============================================
  nmodels <- 1
  if (params@weighted) nmodels <- nmodels + 4
  if (params@LTFU) nmodels <- nmodels + 1
  if (params@km.curves) nmodels <- nmodels + 2
  modelTime <- modelTime * nmodels
  if (params@bootstrap) modelTime <- modelTime*params@nboot
  if (params@parallel) modelTime <- modelTime / params@ncores

  return(list(modelTime = format.time(modelTime),
              expansionTime = format.time(expansionTime),
              totalTime = format.time(modelTime + expansionTime)))
}
