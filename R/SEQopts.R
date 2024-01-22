#' Parameter Builder - passing defaults to \code{SEQuential}
#'
#' @param parallel Logical: define if the SEQuential process is run in parallel, default is FALSE
#' @param nthreads Integer: number of threads to use for data.table processing
#' @param ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @param bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @param nboot Integer: number of bootstraps
#' @param seed Integer: starting seed
#' @param max.period Integer: maximum time to expand about
#' @param max.survival Integer: maximum time for survival curves, default is "max"
#' @param expand Logical: defines if the data should be expanded, default is TRUE
#' @param covariates String: covariates to coerce into a formula object, eg. "A+B*C"
#' @param weighted Logical: whether or not to preform weighted analysis, default is FALSE
#' @param weight.time String: "pre" or "post", whether to construct weights on data that has been pre-expanded or post-expanded
#' @param stabilized Logical: if the weights should be stabilized, default is FALSE
#' @param weight.covariates String: covariates to coerce into a formula object used in weight creation, eg. "A+B*C"
#' @param baseline.indicator String: identifier for baseline variables in \code{covariates} - intended as an override
#' @param sq.indicator String: identifier for squared variables in \code{covariates} - intended as an override
#'
#' @export
SEQopts <- function(parallel = FALSE, nthreads = data.table::getDTthreads(), ncores = parallel::detectCores() - 1,
                    bootstrap = FALSE, nboot = 100, seed = 1636,
<<<<<<< HEAD
                    max.followup = Inf, max.survival = "max", expand = TRUE, covariates = NA, weighted = FALSE, stabilized = FALSE, weight.covariates = NA,
                    pre_expansion = FALSE, baseline.indicator = "_bas", sq.indicator = "_sq"){
=======
                    max.period = Inf, max.survival = "max", expand = TRUE, covariates = NA, weighted = FALSE, stabilized = FALSE,
                    weight.time = "post", baseline.indicator = "_bas", sq.indicator = "_sq"){
>>>>>>> parent of 876bf0f (max.period -> max.followup)

  #Standardization =============================================================
  covariates <- gsub("\\s", "", covariates)

  params <- list(parallel = parallel, nthreads = nthreads, ncores = ncores, bootstrap = bootstrap,
                 nboot = nboot, seed = seed, max.followup = max.followup, max.survival = max.survival,
                 expand = expand, covariates = covariates,
                 weighted = weighted, stabilized = stabilized, weight.covariates = weight.covariates, pre_expansion = pre_expansion,
                 baseline.indicator = baseline.indicator, sq.indicator = sq.indicator)

  return(params)
}

#add weight parameters to opts as an override
