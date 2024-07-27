#' An S4 class of user options to feed into the SEQuential process
#'
#' @slot parallel Logical: to run in parallel
#' @slot nthreads Numeric: number of data.table threads to use
#' @slot ncores Integer: number of cores to use if parallelized
#' @slot bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @slot nboot Integer: number of bootstraps
#' @slot boot.sample Numeric: percentage of data to use when bootstrapping, should in [0, 1], default is 0.8
#' @slot seed Integer: starting seed
#' @slot max.followup Numeric: maximum time to expand about, default is Inf (no maximum)
#' @slot max.survival Numeric: maximum time for survival curves, default is Inf (no maximum)
#' @slot include.period Logical: whether or not to include 'period' and 'period_squared' in the outcome model
#' @slot include.trial Logical: whether or not to include 'trial' and 'trial_squared' in the outcome model
#' @slot covariates String: covariates to coerce into a formula object, eg. "A+B*C"
#' @slot numerator String: numerator covariates to coerce to formula object
#' @slot denominator String: denominator covariates to coerce to formula object
#' @slot surv String: survival covariates to coerce to formula object
#' @slot weighted Logical: whether or not to preform weighted analysis, default is FALSE
#' @slot pre.expansion Logical: whether weighting should be done on pre-expanded data
#' @slot excused Logical: in the case of censoring, whether there is an excused condition
#' @slot excused.col1 String: in the case of \code{excused = TRUE} the column name for Excused1
#' @slot excused.col0 String: in the case of \code{excused = TRUE} the column name for Excused0
#' @slot baseline.indicator String: identifier for baseline variables in \code{covariates, numerator, denominator} - intended as an override
#' @slot squared.indicator String: identifier for squared variables in \code{covariates, numerator, denominator} - intended as an override

setClass("SEQopts",
         slots = c(
           parallel = "logical",
           nthreads = "numeric",
           ncores = "integer",
           nboot = "integer",
           bootstrap = "logical",
           boot.sample = "numeric",
           seed = "integer",
           max.followup = "numeric",
           max.survival = "numeric",
           include.trial = "logical",
           include.period = "logical",
           weighted = "logical",
           pre.expansion = "logical",
           excused = "logical",
           excused.col0 = "character",
           excused.col1 = "character",
           covariates = "character",
           numerator = "character",
           denominator = "character",
           surv = "character",
           baseline.indicator = "character",
           squared.indicator = "character"
         ), prototype = list(
           parallel = FALSE,
           nthreads = data.table::getDTthreads(),
           ncores = parallel::detectCores(),
           bootstrap = FALSE,
           boot.sample = 0.8,
           seed = 1636L,
           max.followup = Inf,
           max.survival = Inf,
           include.trial = TRUE,
           include.period = TRUE,
           weighted = FALSE,
           pre.expansion = TRUE,
           excused = FALSE,
           excused.col0 = NA_character_,
           excused.col1 = NA_character_,
           covariates = NA_character_,
           numerator = NA_character_,
           denominator = NA_character_,
           surv = NA_character_,
           baseline.indicator = "_bas",
           squared.indicator = "_sq"
         ))

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
#' @importFrom data.table data.table

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
         ))

#' An internal S4 class to help transfer weight statistics out of \code{internal_weights}
#'
#' @slot weights a data.table containing the estimated weights, either pre or post expansion
#' @slot coef.n0 coefficients from the numerator zero model
#' @slot coef.n1 coefficients from the numerator one model
#' @slot coef.d0 coefficients from the denominator zero model
#' @slot coef.d1 coefficients from the denominator one model
#' @importFrom data.table data.table
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
         ))
