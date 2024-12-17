#' Parameter Helper
#'
#' @importFrom methods new
#' @keywords internal
parameter.setter <- function(data, DT,
                             id.col, time.col, eligible.col, outcome.col, treatment.col,
                             time_varying.cols, fixed.cols, method,
                             opts) {
  new("SEQparams",
    data = data,
    DT = DT,
    id = id.col,
    time = time.col,
    eligible = eligible.col,
    outcome = outcome.col,
    treatment = treatment.col,
    time_varying = time_varying.cols,
    fixed = fixed.cols,
    method = method,
    parallel = opts@parallel,
    nthreads = opts@nthreads,
    ncores = opts@ncores,
    nboot = opts@nboot,
    bootstrap = opts@bootstrap,
    boot.sample = opts@boot.sample,
    seed = opts@seed,
    include.followup = opts@include.followup,
    include.trial = opts@include.trial,
    min.followup = opts@min.followup,
    max.followup = opts@max.followup,
    max.survival = opts@max.survival,
    weighted = opts@weighted,
    pre.expansion = opts@pre.expansion,
    excused = opts@excused,
    cense = opts@cense,
    hazard = opts@hazard,
    calculate.var = opts@calculate.var,
    compevent = opts@compevent,
    eligible_cense = opts@eligible_cense,
    excused.col0 = opts@excused.col0,
    excused.col1 = opts@excused.col1,
    covariates = opts@covariates,
    numerator = opts@numerator,
    denominator = opts@denominator,
    ltfu.numerator = opts@ltfu.numerator,
    ltfu.denominator = opts@ltfu.denominator,
    km.curves = opts@km.curves,
    surv = opts@surv,
    baseline.indicator = opts@baseline.indicator,
    squared.indicator = opts@squared.indicator,
    fastglm.method = opts@fastglm.method,
    multinomial = opts@multinomial,
    treat.level = opts@treat.level,
    followup.class = opts@followup.class,
    followup.spline = opts@followup.spline,
    elig.wts.1 = opts@elig.wts.1,
    elig.wts.0 = opts@elig.wts.0
  )
}

#' Simplifies parameters down for later use
#'
#' @keywords internal
parameter.simplifier <- function(params) {
  # Variable pre-definition ===================================
  tmp1 <- NULL
  tmp0 <- NULL

  if (!params@bootstrap) {
    params@nboot <- 1L
    params@boot.sample <- 1
    params@parallel <- FALSE
  }

  if (is.na(params@excused.col0) & is.na(params@excused.col1) & params@excused & params@method == "censoring") {
    warning("No excused variables provided for excused censoring, automatically changed to excused = FALSE")
    params@excused <- FALSE
  }

  if (params@km.curves & (params@calculate.var | params@hazard)) stop("Kaplan-Meier Curves and Hazard Ratio or Robust Standard Errors are not compatible. Please select one.")
  if (sum(params@include.followup, params@followup.class, params@followup.spline) > 1) stop("include.followup, followup.class, and followup.spline are exclusive. Please select one")

  if (is.na(params@excused.col0) & params@excused & params@method == "censoring") {
    params@excused.col0 <- "tmp0"
    params@data <- params@data[, tmp0 := 0]
  }
  if (is.na(params@excused.col1) & params@excused & params@method == "censoring") {
    params@excused.col1 <- "tmp1"
    params@data <- params@data[, tmp1 := 0]
  }

  if(!is.na(params@cense)) {
    params@LTFU <- TRUE
    params@weighted <- TRUE
  }

  if (params@method == "ITT" & params@weighted & !params@LTFU) {
    warning("Without LTFU, weighted ITT model is not supported, automatically changed to weighted = FALSE")
    params@weighted <- FALSE
  }
  if (params@followup.class & params@followup.spline) stop("Followup cannot be both a class and a spline, please select one.")

  return(params)
}

#' Output constructor
#'
#' @importFrom methods new
#' @keywords internal
prepare.output <- function(params, outcome_model, hazard, robustSE, survival_curve, risk, elapsed_time) {
  if (!missing(outcome_model)) {
    outcome.coefs <- lapply(1:params@nboot, function(x) coef(outcome_model[[x]]$model$model))
    weight.stats <- lapply(1:params@nboot, function(x) outcome_model[[x]]$weight_info)
  }

  new("SEQoutput",
    bootstrap = params@bootstrap,
    boot.sample = params@boot.sample,
    boot.slice = 1L,
    seed = params@seed,
    nboot = params@nboot,
    outcome = paste0(params@outcome, "~", params@covariates),
    numerator = if (!params@weighted) NA_character_ else paste0(params@treatment, "~", params@numerator),
    denominator = if (!params@weighted) NA_character_ else paste0(params@treatment, "~", params@denominator),
    outcome_model = outcome.coefs,
    hazard = if (!params@hazard) NA_real_ else hazard,
    robust_se = if (!params@calculate.var) list() else robustSE,
    weight_statistics = weight.stats,
    survival_curve = if (!params@km.curves) NA else survival_curve,
    survival_data = if (!params@km.curves) NA else survival_curve$data,
    risk_difference = if (!params@km.curves) NA_real_ else risk$rd,
    risk_ratio = if (!params@km.curves) NA_real_ else risk$rr,
    elapsed_time = elapsed_time
  )
}
