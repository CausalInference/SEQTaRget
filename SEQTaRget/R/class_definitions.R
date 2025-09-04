#' An S4 class of user options to feed into the SEQuential processes and estimates
#' This class should match \code{SEQopts} in file \code{SEQopts.R}
#' @keywords internal
setClass("SEQopts",
  slots = c(
    deviation = "logical",
    deviation.col = "character",
    deviation.conditions = "list",
    deviation.excused = "logical",
    deviation.excused_cols = "list",
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
    weight.eligible_cols = "list",
    weight.preexpansion = "logical",
    excused = "logical",
    calculate.var = "logical",
    hazard = "logical",
    hazard.fixed.dist = "list",
    hazard.time_varying.dist = "list",
    selection.random = "logical",
    selection.prob = "numeric",
    selection.first_trial = "logical",
    cense = "character",
    cense.eligible = "character",
    excused.cols = "list",
    LTFU = "logical",
    covariates = "character",
    numerator = "character",
    denominator = "character",
    cense.numerator = "character",
    cense.denominator = "character",
    km.curves = "logical",
    compevent = "character",
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
    plot.type = "character",
    subgroup = "character",
    data.return = "logical",
    weight.lag_condition = "logical"
  ), prototype = list(
    deviation = FALSE,
    deviation.col = NA_character_,
    deviation.conditions = list(),
    deviation.excused = FALSE,
    deviation.excused_cols = list(),
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
    weight.eligible_cols = list(),
    hazard = FALSE,
    hazard.fixed.dist = list(),
    hazard.time_varying.dist = list(),
    selection.random = FALSE,
    selection.prob = 0.8,
    cense = NA_character_,
    cense.eligible = NA_character_,
    excused.cols = list(),
    LTFU = FALSE,
    km.curves = FALSE,
    covariates = NA_character_,
    numerator = NA_character_,
    denominator = NA_character_,
    cense.numerator = NA_character_,
    cense.denominator = NA_character_,
    compevent = NA_character_,
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
    plot.type = NA_character_,
    subgroup = NA_character_,
    data.return = FALSE,
    weight.lag_condition = TRUE,
    selection.first_trial = FALSE
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
    method = "character",
    verbose = "logical"
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
    method = NA_character_,
    verbose = TRUE
  )
)

#' An internal S4 class to help transfer weight statistics out of \code{internal_weights}
#'
#' @slot weights a data.table containing the estimated weights, either pre or post expansion
#' @slot coef.n0 numerator zero model
#' @slot coef.n1 numerator one model
#' @slot coef.d0 denominator zero model
#' @slot coef.d1 denominator one model
#' @slot coef.ncense numerator censoring model
#' @slot coef.dcense denominator censoring model
#'
#' @keywords internal
setClass("SEQweights",
  slots = c(
    weights = "data.table",
    coef.numerator = "ANY",
    coef.denominator = "ANY",
    coef.ncense = "ANY",
    coef.dcense = "ANY"
  ), prototype = c(
    weights = NA,
    coef.numerator = NA,
    coef.denominator = NA,
    coef.ncense = NA,
    coef.dcense = NA
  )
)

#' An S4 class used to hold the outputs for the SEQuential process
#' @slot params SEQparams object
#' @slot outcome outcome covariates
#' @slot numerator numerator covariates
#' @slot denominator denominator covariates
#' @slot outcome.model list of length \code{bootstrap.nboot} containing outcome coefficients
#' @slot hazard hazard ratio
#' @slot survival.curve ggplot object for the survival curves
#' @slot survival.data data.table of survival data
#' @slot risk.difference risk difference calculated from survival data
#' @slot risk.ratio risk ratio calculated from survival data
#' @slot time time in minutes used for the SEQuential process
#' @slot weight.statistics information from the weighting process, containing weight coefficients and weight statistics
#' @slot info list of outcome and switch information (if applicable)
#' @slot ce.model list of competing event models if \code{compevent} is specified, NA otherwise
#'
setClass("SEQoutput",
         slots = c(
           params = "SEQparams",
           DT = "data.table",
           outcome = "character",
           numerator = "character",
           denominator = "character",
           outcome.model = "list",
           hazard = "list",
           survival.curve = "list",
           survival.data = "list",
           risk.comparison = "list",
           risk.data = "list",
           time = "character",
           weight.statistics = "list",
           ce.model = "list",
           info = "list"
         ), prototype = c(
           params = new("SEQparams"),
           DT = data.table(),
           outcome = NA_character_,
           numerator = NA_character_,
           denominator = NA_character_,
           outcome_model = list(),
           hazard = list(),
           survival.curve = list(),
           survival.data = data.table(),
           risk.comparison = list(),
           risk.data = list(),
           time = NA_character_,
           weight.statistics = list(),
           ce.model = list(),
           info = list()
         )
)
