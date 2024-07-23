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
      baseline.indicator = opts@baseline.indicator,
      squared.indicator = opts@squared.indicator
      )
}

parameter.simplifier <- function(params) {
  if(!params@bootstrap) {
    params@nboot <- 1L
    params@boot.sample <- 1
    params@parallel <- FALSE
  }

  return(params)
}
