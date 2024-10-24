#' Parameter Builder - passing defaults to \code{SEQuential}
#'
#' @param parallel Logical: define if the SEQuential process is run in parallel, default is FALSE
#' @param nthreads Integer: number of threads to use for data.table processing
#' @param ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @param bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @param nboot Integer: number of bootstraps
#' @param boot.sample Numeric: percentage of data to use when bootstrapping, should in [0, 1], default is 0.8
#' @param seed Integer: starting seed
#' @param max.followup Numeric: maximum time to expand about, default is Inf (no maximum)
#' @param max.survival Numeric: maximum time for survival curves, default is Inf (no maximum)
#' @param include.period Logical: whether or not to include 'period' and 'period_squared' in the outcome model
#' @param include.trial Logical: whether or not to include 'trial' and 'trial_squared' in the outcome model
#' @param covariates String: covariates to coerce into a formula object, eg. "A+B*C"
#' @param numerator String: numerator covariates to coerce to formula object
#' @param denominator String: denominator covariates to coerce to formula object
#' @param ltfu.numerator String: TODO
#' @param ltfu.denominator String: TODO
#' @param surv String: survival covariates to coerce to formula object
#' @param weighted Logical: whether or not to preform weighted analysis, default is FALSE
#' @param pre.expansion Logical: whether weighting should be done on pre-expanded data
#' @param excused Logical: in the case of censoring, whether there is an excused condition
#' @param cense String: TODO
#' @param cense2 String: TODO
#' @param eligible_cense String: TODO
#' @param eligible_cense2 String: TODO
#' @param compevent String: TODO
#' @param excused.col1 String: in the case of \code{excused = TRUE} the column name for Excused1
#' @param excused.col0 String: in the case of \code{excused = TRUE} the column name for Excused0
#' @param km.curves Logical: Kaplan-Meier survival curve creation and data return
#' @param baseline.indicator String: identifier for baseline variables in \code{covariates, numerator, denominator} - intended as an override
#' @param squared.indicator String: identifier for squared variables in \code{covariates, numerator, denominator} - intended as an override
#'
#'
#' @export
#' @returns An object of class 'SEQOpts'
SEQopts <- function(parallel = FALSE, nthreads = data.table::getDTthreads(), ncores = parallel::detectCores() - 1,
                    bootstrap = FALSE, nboot = 100, boot.sample = 0.8, seed = 1636,
                    max.followup = Inf, max.survival = Inf, include.period = TRUE, include.trial = TRUE,
                    covariates = NA, weighted = FALSE,
                    numerator = NA, denominator = NA, surv = NA, pre.expansion = TRUE,
                    ltfu.numerator = NA, ltfu.denominator = NA, cense = NA, cense2 = NA, eligible_cense = NA, eligible_cense2 = NA,
                    compevent = NA,
                    excused = FALSE, excused.col1 = NA, excused.col0 = NA, km.curves = FALSE,
                    baseline.indicator = "_bas", squared.indicator = "_sq") {
  # Standardization =============================================================
  parallel <- as.logical(parallel)
  nthreads <- as.integer(nthreads)
  ncores <- as.integer(ncores)
  bootstrap <- as.logical(bootstrap)
  nboot <- as.integer(nboot)
  seed <- as.integer(seed)
  max.followup <- as.numeric(max.followup)
  max.survival <- as.numeric(max.survival)

  include.trial <- as.logical(include.trial)
  include.period <- as.logical(include.period)

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
  cense2 <- as.character(cense2)
  eligible_cense <- as.character(eligible_cense)
  eligible_cense2 <- as.character(eligible_cense2)
  compevent <- as.character(compevent)

  baseline.indicator <- as.character(baseline.indicator)
  squared.indicator <- as.character(squared.indicator)

  new("SEQopts",
    parallel = parallel,
    nthreads = nthreads,
    ncores = ncores,
    bootstrap = bootstrap,
    nboot = nboot,
    boot.sample = boot.sample,
    seed = seed,
    max.followup = max.followup,
    max.survival = max.survival,
    include.trial = include.trial,
    include.period = include.period,
    weighted = weighted,
    pre.expansion = pre.expansion,
    excused = excused,
    cense = cense,
    cense2 = cense2,
    compevent = compevent,
    eligible_cense = eligible_cense,
    eligible_cense2 = eligible_cense2,
    excused.col1 = excused.col1,
    excused.col0 = excused.col0,
    km.curves = km.curves,
    covariates = covariates,
    numerator = numerator,
    denominator = denominator,
    surv = surv,
    baseline.indicator = baseline.indicator,
    squared.indicator = squared.indicator
  )
}
