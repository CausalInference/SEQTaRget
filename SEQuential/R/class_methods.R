#' An S4 class of user options to feed into the SEQuential processes and estimates
#'
#' @slot parallel Logical: define if the SEQuential process is run in parallel, default is FALSE
#' @slot nthreads Integer: number of threads to use for data.table processing
#' @slot ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @slot bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @slot bootstrap.nboot Integer: number of bootstraps
#' @slot bootstrap.sample Numeric: percentage of data to use when bootstrapping, should in [0, 1], default is 0.8
#' @slot seed Integer: starting seed
#' @slot followup.min Numeric: minimum time to expand aboud, default is -Inf (no minimum)
#' @slot followup.max Numeric: maximum time to expand about, default is Inf (no maximum)
#' @slot survival.max Numeric: maximum time for survival curves, default is Inf (no maximum)
#' @slot followup.include Logical: whether or not to include 'period' and 'period_squared' in the outcome model
#' @slot trial.include Logical: whether or not to include 'trial' and 'trial_squared' in the outcome model
#' @slot covariates String: covariates to coerce into a formula object, eg. "A+B*C"
#' @slot numerator String: numerator covariates to coerce to formula object for default weight models
#' @slot denominator String: denominator covariates to coerce to formula object for default weight models
#' @slot cense.numerator String: numerator covariates to coerce to formula object for censoring weight models
#' @slot cense.denominator String: denominator covariates to coerce to formula object for censoring weight models
#' @slot surv String: survival covariates to coerce to formula object
#' @slot weighted Logical: whether or not to preform weighted analysis, default is FALSE
#' @slot weight.lower Numeric: weights truncated at lower end at this weight
#' @slot weight.upper Numeric: weights truncated at upper end at this weight
#' @slot weight.p99 Logical: forces weight truncation at 1st and 99th percentile weights, will override provided \code{weight.upper} and \code{weight.lower}
#' @slot weight.eligible0 String: Column name for
#' @slot weight.eligible1 String: TODO
#' @slot weight.preexpansion Logical: whether weighting should be done on pre-expanded data
#' @slot calculate.var Logical: TODO
#' @slot hazard Logical: TODO
#' @slot selection.random Logical: TODO
#' @slot selection.prob Numeric: TODO
#' @slot excused Logical: in the case of censoring, whether there is an excused condition
#' @slot cense String: Column name for an additional censoring variable, for example, loss-to-followup indication
#' @slot cense.eligible String: TODO
#' @slot multinomial Logical: whether or not to expect multinomial models
#' @slot treat.level List: which treatment levels to compare through survival curves
#' @slot compevent String: TODO
#' @slot excused.col1 String: Column name for Excused 1 in the case of \code{excused = TRUE}
#' @slot excused.col0 String: Column name for Excused 0 in the case of \code{excused = TRUE}
#' @slot km.curves Logical: Kaplan-Meier survival curve creation and data return
#' @slot indicator.baseline String: identifier for baseline variables in \code{covariates, numerator, denominator} - intended as an override
#' @slot indicator.squared String: identifier for squared variables in \code{covariates, numerator, denominator} - intended as an override
#' @slot fastglm.method Integer: decomposition method for fastglm (1-QR, 2-Cholesky, 3-LDLT, 4-QR.FPIV)
#' @slot followup.class Logical: TODO
#' @slot followup.spline Logical: TODO
#' @slot plot.title Character: Title for output plot if \code{km.curves = TRUE}
#' @slot plot.subtitle Character: Subtitle for output plot if \code{km.curves = TRUE}
#' @slot plot.labels Character: Color labels for output plot if \code{km.curves = TRUE} in order e.g. \code{c("risk.0", "risk.1")}
#' @slot plot.colors Character: Colors for output plot if \code{km.curves = TRUE}, defaulted to ggplot2 defaults
#' @slot plot.type Character: Type of plot to create if \code{km.curves = TRUE}
#'
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
