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
                    max.followup = Inf, max.survival = "max", expand = TRUE, covariates = NA, weighted = FALSE,
                    numerator = NA, denominator = NA, pre.expansion = TRUE, weight.covariates = NA,
                    baseline.indicator = "_bas", sq.indicator = "_sq"){

  #Standardization =============================================================
  covariates <- gsub("\\s", "", covariates)
  numerator <- gsub("\\s", "", numerator)
  denominator <- gsub("\\s", "", denominator)

  params <- list(parallel = parallel, nthreads = nthreads, ncores = ncores, bootstrap = bootstrap,
                 nboot = nboot, boot.sample = boot.sample, seed = seed, max.followup = max.followup, max.survival = max.survival,
                 expand = expand, covariates = covariates, boot.return = boot.return, weight.covariates = weight.covariates,
                 weighted = weighted, pre.expansion = pre.expansion, numerator = numerator, denominator = denominator,
                 baseline.indicator = baseline.indicator, sq.indicator = sq.indicator)

  return(params)
}
