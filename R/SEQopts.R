#' Parameter Builder - passing defaults to \code{SEQuential}
#'
#' @param parallel Logical: define if the SEQuential process is run in parallel, default is FALSE
#' @param ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @param bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @param nboot Integer: number of bootstraps
#' @param seed Integer: starting seed
#' @param memory Integer: number of bytes for in-memory use, default is 1GB less than free system max
#' @param max Integer: maximum time to expand about
#' @param expand Logical: defines if the data should be expanded, default is TRUE
#' @param covariates String: covariates to coerce into a formula object, eg. "A+B*C"
#' @param weighted Logical:
#' @param stabilized Logical: if the weights should be stabilized, default is FALSE
#'
#' @export
SEQopts <- function(parallel = FALSE, nthreads = data.table::getDTthreads(), ncores = parallel::detectCores() - 1,
                    bootstrap = FALSE, nboot = 100, seed = 1636, memory = get_ram() - 2^20,
                    max = Inf, expand = TRUE, covariates = NA, weighted = FALSE, stabilized = FALSE,
                    weight.time = "post"){

  params <- list(parallel = parallel, nthreads = nthreads, ncores = ncores, bootstrap = bootstrap,
                 nboot = nboot, seed = seed, memory = memory, max = max, expand = expand, covariates = covariates,
                 weighted = weighted, stabilized = stabilized, weight.time = weight.time)

  return(params)
}


