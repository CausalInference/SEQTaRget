#' An S4 class of user options to feed into the SEQuential process
#'
#' @slot parallel Logical: define if the SEQuential process is run in parallel, default is FALSE
#' @slot nthreads Integer: number of threads to use for data.table processing
#' @slot ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @slot bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @slot nboot Integer: number of bootstraps
#' @slot boot.sample Numeric: percentage of data to use when bootstrapping, should in [0, 1], default is 0.8
#' @slot seed Integer: starting seed
#' @slot min.followup Numeric: minimum time to expand aboud, default is -Inf (no minimum)
#' @slot max.followup Numeric: maximum time to expand about, default is Inf (no maximum)
#' @slot max.survival Numeric: maximum time for survival curves, default is Inf (no maximum)
#' @slot include.period Logical: whether or not to include 'period' and 'period_squared' in the outcome model
#' @slot include.trial Logical: whether or not to include 'trial' and 'trial_squared' in the outcome model
#' @slot covariates String: covariates to coerce into a formula object, eg. "A+B*C"
#' @slot numerator String: numerator covariates to coerce to formula object
#' @slot denominator String: denominator covariates to coerce to formula object
#' @slot ltfu.numerator String: TODO
#' @slot ltfu.denominator String: TODO
#' @slot surv String: survival covariates to coerce to formula object
#' @slot weighted Logical: whether or not to preform weighted analysis, default is FALSE
#' @slot lower.weight Numeric: weights truncated at lower end at this weight
#' @slot upper.weight Numeric: weights truncated at upper end at this weight
#' @slot p99.weight Logical: forces weight truncation at 1st and 99th percentile weights, will override provided \code{upper.weight} and \code{lower.weight}
#' @slot elig.wts.0 String: TODO
#' @slot elig.wts.1 String: TODO
#' @slot pre.expansion Logical: whether weighting should be done on pre-expanded data
#' @slot calculate.var Logical: TODO
#' @slot hazard Logical: TODO
#' @slot random.selection Logical: TODO
#' @slot selection.prob Numeric: TODO
#' @slot excused Logical: in the case of censoring, whether there is an excused condition
#' @slot cense String: TODO
#' @slot eligible_cense String: TODO
#' @slot multinomial Logical: whether or not to expect multinomial models
#' @slot treat.level List: which treatment levels to compare through survival curves
#' @slot compevent String: TODO
#' @slot excused.col1 String: in the case of \code{excused = TRUE} the column name for Excused1
#' @slot excused.col0 String: in the case of \code{excused = TRUE} the column name for Excused0
#' @slot km.curves Logical: Kaplan-Meier survival curve creation and data return
#' @slot baseline.indicator String: identifier for baseline variables in \code{covariates, numerator, denominator} - intended as an override
#' @slot squared.indicator String: identifier for squared variables in \code{covariates, numerator, denominator} - intended as an override
#' @slot fastglm.method Integer: decomposition method for fastglm (1-QR, 2-Cholesky, 3-LDLT, 4-QR.FPIV)
#' @slot followup.class Logical: TODO
#' @slot followup.spline Logical: TODO
setClass("SEQopts",
  slots = c(
    parallel = "logical",
    nthreads = "numeric",
    ncores = "integer",
    nboot = "integer",
    bootstrap = "logical",
    boot.sample = "numeric",
    seed = "integer",
    min.followup = "numeric",
    max.followup = "numeric",
    max.survival = "numeric",
    include.trial = "logical",
    include.followup = "logical",
    weighted = "logical",
    lower.weight = "numeric",
    upper.weight = "numeric",
    p99.weight = "logical",
    elig.wts.0 = "character",
    elig.wts.1 = "character",
    pre.expansion = "logical",
    excused = "logical",
    calculate.var = "logical",
    hazard = "logical",
    random.selection = "logical",
    selection.prob = "numeric",
    cense = "character",
    eligible_cense = "character",
    excused.col0 = "character",
    excused.col1 = "character",
    LTFU = "logical",
    covariates = "character",
    numerator = "character",
    denominator = "character",
    ltfu.numerator = "character",
    ltfu.denominator = "character",
    km.curves = "logical",
    compevent = "character",
    surv = "character",
    baseline.indicator = "character",
    squared.indicator = "character",
    fastglm.method = "integer",
    multinomial = "logical",
    treat.level = "list",
    followup.class = "logical",
    followup.spline = "logical"
  ), prototype = list(
    parallel = FALSE,
    nthreads = data.table::getDTthreads(),
    ncores = parallel::detectCores(),
    bootstrap = FALSE,
    boot.sample = 0.8,
    seed = 1636L,
    min.followup = -Inf,
    max.followup = Inf,
    max.survival = Inf,
    include.trial = TRUE,
    include.followup = TRUE,
    weighted = FALSE,
    lower.weight = -Inf,
    upper.weight = Inf,
    p99.weight = FALSE,
    pre.expansion = TRUE,
    excused = FALSE,
    elig.wts.0 = NA_character_,
    elig.wts.1 = NA_character_,
    calculate.var = FALSE,
    hazard = FALSE,
    random.selection = FALSE,
    selection.prob = 0.8,
    cense = NA_character_,
    eligible_cense = NA_character_,
    excused.col0 = NA_character_,
    excused.col1 = NA_character_,
    LTFU = FALSE,
    km.curves = FALSE,
    covariates = NA_character_,
    numerator = NA_character_,
    denominator = NA_character_,
    ltfu.numerator = NA_character_,
    ltfu.denominator = NA_character_,
    compevent = NA_character_,
    surv = NA_character_,
    baseline.indicator = "_bas",
    squared.indicator = "_sq",
    fastglm.method = 2L,
    treat.level = list(0, 1),
    multinomial = FALSE,
    followup.class = FALSE,
    followup.spline = FALSE
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
