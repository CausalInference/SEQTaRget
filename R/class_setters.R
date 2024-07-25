#' Parameter Helper
#'
#' @importFrom methods new
#' @keywords internal
parameter.setter <- function(data, DT,
                             id.col, time.col, eligible.col, outcome.col, treatment.col,
                             time_varying.cols, fixed.cols, method,
                             opts){
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
      max.followup = opts@max.followup,
      max.survival = opts@max.survival,
      weighted = opts@weighted,
      pre.expansion = opts@pre.expansion,
      excused = opts@excused,
      excused.col0 = opts@excused.col0,
      excused.col1 = opts@excused.col1,
      covariates = opts@covariates,
      numerator = opts@numerator,
      denominator = opts@denominator,
      surv = opts@surv,
      baseline.indicator = opts@baseline.indicator,
      squared.indicator = opts@squared.indicator
      )
}

#' Simplifies parameters down for later use
#'
#' @keywords internal
parameter.simplifier <- function(params) {
  if(!params@bootstrap) {
    params@nboot <- 1L
    params@boot.sample <- 1
    params@parallel <- FALSE
  }

  return(params)
}

#' Output constructor
#'
#' @importFrom methods new
#' @keywords internal
prepare.output <- function(params, outcome_model, survival_curve,risk, elapsed_time) {

  if(!missing(outcome_model)){
    outcome.coefs <- lapply(1:params@nboot, function(x) outcome_model[[x]]$coefficients)
    weight.stats <- lapply(1:params@nboot, function(x) outcome_model[[x]]$weight_info)
  }

  new("SEQuential",
      bootstrap = params@bootstrap,
      boot.sample = params@boot.sample,
      seed = params@seed,
      nboot = params@nboot,
      outcome = paste0(params@outcome, "~", params@covariates),
      numerator = paste0(params@treatment, "~", params@numerator),
      denominator = paste0(params@treatment, "~", params@denominator),
      outcome_model = outcome.coefs,
      weight_statistics = weight.stats,
      survival_curve = survival_curve,
      survival_data = survival_curve$data,
      risk_difference = risk$rd,
      risk_ratio = risk$rr,
      elapsed_time = elapsed_time
      )
}

