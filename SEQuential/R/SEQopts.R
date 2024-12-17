#' Parameter Builder - passing defaults to \code{SEQuential}
#'
#' @param parallel Logical: define if the SEQuential process is run in parallel, default is FALSE
#' @param nthreads Integer: number of threads to use for data.table processing
#' @param ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @param bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @param nboot Integer: number of bootstraps
#' @param boot.sample Numeric: percentage of data to use when bootstrapping, should in [0, 1], default is 0.8
#' @param seed Integer: starting seed
#' @param min.followup Numeric: minimum time to expand aboud, default is -Inf (no minimum)
#' @param max.followup Numeric: maximum time to expand about, default is Inf (no maximum)
#' @param max.survival Numeric: maximum time for survival curves, default is Inf (no maximum)
#' @param include.followup Logical: whether or not to include 'followup' and 'followup_squared' in the outcome model
#' @param include.trial Logical: whether or not to include 'trial' and 'trial_squared' in the outcome model
#' @param covariates String: covariates to coerce into a formula object, eg. "A+B*C"
#' @param numerator String: numerator covariates to coerce to formula object
#' @param denominator String: denominator covariates to coerce to formula object
#' @param ltfu.numerator String: TODO
#' @param ltfu.denominator String: TODO
#' @param surv String: survival covariates to coerce to formula object
#' @param weighted Logical: whether or not to preform weighted analysis, default is FALSE
#' @param lower.weight Numeric: weights truncated at lower end at this weight
#' @param upper.weight Numeric: weights truncated at upper end at this weight
#' @param p99.weight Logical: forces weight truncation at 1st and 99th percentile weights, will override provided \code{upper.weight} and \code{lower.weight}
#' @param elig.wts.0 String: TODO
#' @param elig.wts.1 String: TODO
#' @param pre.expansion Logical: whether weighting should be done on pre-expanded data
#' @param calculate.var Logical: TODO
#' @param hazard Logical: TODO
#' @param random.selection Logical: TODO
#' @param selection.prob Numeric: TODO
#' @param excused Logical: in the case of censoring, whether there is an excused condition
#' @param cense String: TODO
#' @param eligible_cense String: TODO
#' @param multinomial Logical: whether or not to expect multinomial models
#' @param treat.level List: which treatment levels to compare through survival curves
#' @param compevent String: TODO
#' @param excused.col1 String: in the case of \code{excused = TRUE} the column name for Excused1
#' @param excused.col0 String: in the case of \code{excused = TRUE} the column name for Excused0
#' @param km.curves Logical: Kaplan-Meier survival curve creation and data return
#' @param baseline.indicator String: identifier for baseline variables in \code{covariates, numerator, denominator} - intended as an override
#' @param squared.indicator String: identifier for squared variables in \code{covariates, numerator, denominator} - intended as an override
#' @param fastglm.method Integer: decomposition method for fastglm (1-QR, 2-Cholesky, 3-LDLT, 4-QR.FPIV)
#' @param followup.class Logical: TODO
#' @param followup.spline Logical: TODO
#'
#' @export
#' @returns An object of class 'SEQOpts'
SEQopts <- function(parallel = FALSE, nthreads = data.table::getDTthreads(), ncores = parallel::detectCores() - 1,
                    bootstrap = FALSE, nboot = 100, boot.sample = 0.8, seed = 1636,
                    min.followup = -Inf, max.followup = Inf, max.survival = Inf, include.followup = TRUE, include.trial = TRUE,
                    covariates = NA, weighted = FALSE, upper.weight = Inf, lower.weight = -Inf, p99.weight = FALSE,
                    numerator = NA, denominator = NA, surv = NA, pre.expansion = TRUE, elig.wts.0 = NA, elig.wts.1 = NA, hazard = FALSE, calculate.var = FALSE,
                    random.selection = FALSE, selection.prob = 0.8, followup.class = FALSE, followup.spline = FALSE,
                    ltfu.numerator = NA, ltfu.denominator = NA, cense = NA, eligible_cense = NA,
                    compevent = NA, multinomial = FALSE, treat.level = c(0, 1),
                    excused = FALSE, excused.col1 = NA, excused.col0 = NA, km.curves = FALSE,
                    baseline.indicator = "_bas", squared.indicator = "_sq",
                    fastglm.method = 2L) {
  # Standardization =============================================================
  parallel <- as.logical(parallel)
  nthreads <- as.integer(nthreads)
  ncores <- as.integer(ncores)
  bootstrap <- as.logical(bootstrap)
  nboot <- as.integer(nboot)
  seed <- as.integer(seed)
  min.followup <- as.numeric(min.followup)
  max.followup <- as.numeric(max.followup)
  max.survival <- as.numeric(max.survival)
  lower.weight <- as.numeric(lower.weight)
  upper.weight <- as.numeric(upper.weight)
  elig.wts.0 <- as.character(elig.wts.0)
  elig.wts.1 <- as.character(elig.wts.1)

  hazard <- as.logical(hazard)
  calculate.var <- as.logical(calculate.var)

  selection.prob <- as.logical(selection.prob)
  random.selection <- as.logical(random.selection)

  include.trial <- as.logical(include.trial)
  include.followup <- as.logical(include.followup)

  covariates <- gsub("\\s", "", covariates)
  numerator <- gsub("\\s", "", numerator)
  denominator <- gsub("\\s", "", denominator)
  ltfu.numerator <- gsub("\\s", "", ltfu.numerator)
  ltfu.denominator <- gsub("\\s", "", ltfu.denominator)
  surv <- gsub("\\s", "", surv)

  weighted <- as.logical(weighted)
  pre.expansion <- as.logical(pre.expansion)

  excused <- as.logical(excused)
  excused.col1 <- as.character(excused.col1)
  excused.col0 <- as.character(excused.col0)

  cense <- as.character(cense)
  eligible_cense <- as.character(eligible_cense)
  compevent <- as.character(compevent)
  treat.level <- as.list(treat.level)

  baseline.indicator <- as.character(baseline.indicator)
  squared.indicator <- as.character(squared.indicator)

  fastglm.method <- as.integer(fastglm.method)

  new("SEQopts",
    parallel = parallel,
    nthreads = nthreads,
    ncores = ncores,
    bootstrap = bootstrap,
    nboot = nboot,
    boot.sample = boot.sample,
    seed = seed,
    min.followup = min.followup,
    max.followup = max.followup,
    max.survival = max.survival,
    include.trial = include.trial,
    include.followup = include.followup,
    weighted = weighted,
    lower.weight = lower.weight,
    upper.weight = upper.weight,
    p99.weight = p99.weight,
    pre.expansion = pre.expansion,
    excused = excused,
    cense = cense,
    compevent = compevent,
    eligible_cense = eligible_cense,
    excused.col1 = excused.col1,
    excused.col0 = excused.col0,
    km.curves = km.curves,
    covariates = covariates,
    numerator = numerator,
    denominator = denominator,
    surv = surv,
    baseline.indicator = baseline.indicator,
    squared.indicator = squared.indicator,
    fastglm.method = fastglm.method,
    treat.level = treat.level,
    multinomial = multinomial,
    hazard = hazard,
    calculate.var = calculate.var,
    elig.wts.1 = elig.wts.1,
    elig.wts.0 = elig.wts.0,
    followup.class = followup.class,
    followup.spline = followup.spline
  )
}
