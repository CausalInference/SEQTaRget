#' An S4 class of user options to feed into the SEQuential processes and estimates
#' This class should match \code{SEQopts} in file \code{SEQopts.R}
#' @keywords internal
setClass("SEQopts",
  slots = c(
    parallel = "logical",
    nthreads = "numeric",
    ncores = "integer",
    bootstrap.nboot = "integer",
    bootstrap = "logical",
    bootstrap.sample = "numeric",
    seed = "integer",
    followup.min = "numeric",
    followup.max = "numeric",
    survival.max = "numeric",
    trial.include = "logical",
    followup.include = "logical",
    weighted = "logical",
    weight.lower = "numeric",
    weight.upper = "numeric",
    weight.p99 = "logical",
    weight.eligible0 = "character",
    weight.eligible1 = "character",
    weight.preexpansion = "logical",
    excused = "logical",
    calculate.var = "logical",
    hazard = "logical",
    selection.random = "logical",
    selection.prob = "numeric",
    cense = "character",
    cense.eligible = "character",
    excused.col0 = "character",
    excused.col1 = "character",
    LTFU = "logical",
    covariates = "character",
    numerator = "character",
    denominator = "character",
    cense.numerator = "character",
    cense.denominator = "character",
    km.curves = "logical",
    compevent = "character",
    surv = "character",
    indicator.baseline = "character",
    indicator.squared = "character",
    fastglm.method = "integer",
    multinomial = "logical",
    treat.level = "list",
    followup.class = "logical",
    followup.spline = "logical",
    plot.title = "character",
    plot.subtitle = "character",
    plot.labels = "character",
    plot.colors = "character",
    plot.type = "character"
  ), prototype = list(
    parallel = FALSE,
    nthreads = data.table::getDTthreads(),
    ncores = parallel::detectCores(),
    bootstrap = FALSE,
    bootstrap.sample = 0.8,
    seed = 1636L,
    followup.min = -Inf,
    followup.max = Inf,
    survival.max = Inf,
    trial.include = TRUE,
    followup.include = TRUE,
    weighted = FALSE,
    weight.lower = -Inf,
    weight.upper = Inf,
    weight.p99 = FALSE,
    weight.preexpansion = TRUE,
    excused = FALSE,
    weight.eligible0 = NA_character_,
    weight.eligible1 = NA_character_,
    calculate.var = FALSE,
    hazard = FALSE,
    selection.random = FALSE,
    selection.prob = 0.8,
    cense = NA_character_,
    cense.eligible = NA_character_,
    excused.col0 = NA_character_,
    excused.col1 = NA_character_,
    LTFU = FALSE,
    km.curves = FALSE,
    covariates = NA_character_,
    numerator = NA_character_,
    denominator = NA_character_,
    cense.numerator = NA_character_,
    cense.denominator = NA_character_,
    compevent = NA_character_,
    surv = NA_character_,
    indicator.baseline = "_bas",
    indicator.squared = "_sq",
    fastglm.method = 2L,
    treat.level = list(0, 1),
    multinomial = FALSE,
    followup.class = FALSE,
    followup.spline = FALSE,
    plot.title = NA_character_,
    plot.subtitle = NA_character_,
    plot.labels = NA_character_,
    plot.colors = NA_character_,
    plot.type = NA_character_
  )
)

#' An internal S4 class to carry around parameters during the SEQuential process - inherits user facing parameters from \code{SEQopts}
#'
#' @slot data pre expansion data
#' @slot DT post expansion data
#' @slot id id column as defined by the user
#' @slot time time column as defined by the user
#' @slot eligible eligible column as defined by the user
#' @slot treatment treatment column as defined by the user
#' @slot time_varying list of time varying columns as defined by the user
#' @slot fixed list of fixed columns as defined by the user
#' @slot method method of analysis as defined by the user
#'
#' @keywords internal

setClass("SEQparams",
  contains = "SEQopts",
  slots = c(
    data = "data.table",
    DT = "data.table",
    id = "character",
    time = "character",
    eligible = "character",
    treatment = "character",
    outcome = "character",
    time_varying = "list",
    fixed = "list",
    method = "character"
  ), prototype = list(
    data = NA,
    DT = NA,
    id = NA_character_,
    time = NA_character_,
    eligible = NA_character_,
    treatment = NA_character_,
    outcome = NA_character_,
    time_varying = list(),
    fixed = list(),
    method = NA_character_
  )
)

#' An internal S4 class to help transfer weight statistics out of \code{internal_weights}
#'
#' @slot weights a data.table containing the estimated weights, either pre or post expansion
#' @slot coef.n0 coefficients from the numerator zero model
#' @slot coef.n1 coefficients from the numerator one model
#' @slot coef.d0 coefficients from the denominator zero model
#' @slot coef.d1 coefficients from the denominator one model
#'
#' @keywords internal
setClass("SEQweights",
  slots = c(
    weights = "data.table",
    coef.n0 = "numeric",
    coef.n1 = "numeric",
    coef.d0 = "numeric",
    coef.d1 = "numeric"
  ), prototype = c(
    weights = NA,
    coef.n0 = NA_real_,
    coef.n1 = NA_real_,
    coef.d0 = NA_real_,
    coef.d1 = NA_real_
  )
)

#' An S4 class used to hold the outputs for the SEQuential process
#' @slot boostrap if the process was bootstrapped
#' @slot bootstrap.sample how much of the data was sampled during bootstrapping
#' @slot boot.slice when using method 'show' defines what to print
#' @slot seed seeding any random sampling occuring in bootstrap process
#' @slot bootstrap.nboot number of bootstraps
#' @slot outcome outcome covariates
#' @slot numerator numerator covariates
#' @slot denominator denominator covariates
#' @slot outcome_model list of length \code{bootstrap.nboot} containing outcome coefficients
#' @slot hazard hazard ratio
#' @slot robust_se robust standard errors
#' @slot survival_curve ggplot object for the survival curves
#' @slot survival_data data.table of survival data
#' @slot risk_difference risk difference calculated from survival data
#' @slot risk_ratio risk ratio calculated from survival data
#' @slot elapsed_time time in minutes used for the SEQuential process
#' @slot weight_statistics information from the weighting process, containing weight coefficients and weight statistics
#'
setClass("SEQoutput",
         slots = c(
           bootstrap = "logical",
           bootstrap.sample = "numeric",
           boot.slice = "integer",
           seed = "integer",
           bootstrap.nboot = "integer",
           outcome = "character",
           numerator = "character",
           denominator = "character",
           outcome_model = "list",
           hazard = "numeric",
           robust_se = "list",
           survival_curve = "ANY",
           survival_data = "ANY",
           risk_difference = "numeric",
           risk_ratio = "numeric",
           elapsed_time = "character",
           weight_statistics = "list"
         ), prototype = c(
           bootstrap = FALSE,
           bootstrap.sample = NA_real_,
           boot.slice = 1L,
           seed = NA_integer_,
           bootstrap.nboot = NA_integer_,
           outcome = NA_character_,
           numerator = NA_character_,
           denominator = NA_character_,
           outcome_model = list(),
           hazard = NA_real_,
           robust_se = list(),
           survival_curve = NA,
           survival_data = data.table(),
           risk_difference = NA_real_,
           risk_ratio = NA_real_,
           elapsed_time = NA_character_,
           weight_statistics = list()
         )
)
