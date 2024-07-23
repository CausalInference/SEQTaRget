#' Parameter Builder - passing defaults to \code{SEQuential}
#'
#' @param parallel Logical: define if the SEQuential process is run in parallel, default is FALSE
#' @param nthreads Integer: number of threads to use for data.table processing
#' @param ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @param bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @param nboot Integer: number of bootstraps
#' @param boot.sample Numeric: percentage of data to use when bootstrapping, should in [0, 1], default is 0.8
#' @param seed Integer: starting seed
#' @param max.period Integer: maximum time to expand about
#' @param max.survival Integer: maximum time for survival curves, default is "max"
#' @param expand Logical: defines if the data should be expanded, default is TRUE
#' @param covariates String: covariates to coerce into a formula object, eg. "A+B*C"
#' @param weighted Logical: whether or not to preform weighted analysis, default is FALSE
#' @param weight.time String: "pre" or "post", whether to construct weights on data that has been pre-expanded or post-expanded
#' @param stabilized Logical: if the weights should be stabilized, default is FALSE
#' @param baseline.indicator String: identifier for baseline variables in \code{covariates} - intended as an override
#' @param sq.indicator String: identifier for squared variables in \code{covariates} - intended as an override
#'
#' @export
SEQopts <- function(parallel = FALSE, nthreads = data.table::getDTthreads(), ncores = parallel::detectCores() - 1,
                    bootstrap = FALSE, nboot = 100, boot.sample = 0.8, boot.return = "coef", seed = 1636,
                    max.followup = Inf, max.survival = Inf, expand = TRUE, covariates = NA, weighted = FALSE,
                    numerator = NA, denominator = NA, pre.expansion = TRUE, weight.covariates = NA, excused = FALSE, excused.col1 = NA, excused.col0 = NA,
                    baseline.indicator = "_bas", squared.indicator = "_sq") {

  #Standardization =============================================================
  parallel <- as.logical(parallel)
  nthreads <- as.integer(nthreads)
  ncores <- as.integer(ncores)
  bootstrap <- as.logical(bootstrap)
  nboot <- as.integer(nboot)
  seed <- as.integer(seed)
  max.followup <- as.numeric(max.followup)
  max.survival <- as.numeric(max.survival)

  covariates <- gsub("\\s", "", covariates)
  numerator <- gsub("\\s", "", numerator)
  denominator <- gsub("\\s", "", denominator)

  weighted <- as.logical(weighted)
  pre.expansion <- as.logical(pre.expansion)

  excused <- as.logical(excused)
  excused.col1 <- as.character(excused.col1)
  excused.col0 <- as.character(excused.col0)

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
      weighted = weighted,
      pre.expansion = pre.expansion,
      excused = excused,
      excused.col1 = excused.col1,
      excused.col0 = excused.col0,
      covariates = covariates,
      numerator = numerator,
      denominator = denominator,
      baseline.indicator = baseline.indicator,
      squared.indicator = squared.indicator)
}
