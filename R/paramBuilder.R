#' Internal Parameter Builder - passing defaults to \code{SEQuential}
#' @keywords internal
buildParam <- function(){
  SEQopts.expansion() |>
    SEQopts.computation() |>
    SEQopts.covariates() |>
    SEQopts.weight()
}

#' User-facing helper function to create a parameter list for \code{SEQuential}
#' @param x List: list of other parameters to concatenate
#' @param parallel Logical: define if the SEQuential process is run in parallel, default is FALSE
#' @param ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @param bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @param nboot Integer: number of bootstrap
#' @param seed Integer: starting seed
#' @param memory Integer: number of bytes for in-memory use, default is 1GB less than free system max
#'
#' @export
SEQopts.computation <- function(x = NULL, parallel = FALSE,
                           ncores = parallel::detectCores() - 1,
                           bootstrap = FALSE,
                           nboot = 100,
                           seed = 1636,
                           memory = get_ram() - 2^20){

  params <- list(parallel = parallel, ncores = ncores, bootstrap = bootstrap,
                 nboot = nboot, seed = seed, memory = memory)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

#' User-facing helper function to create a parameter list for \code{SEQuential}
#' @param x List: list of other parameters to concatenate
#' @param max Integer: maximum time to expand about
#' @param expansion Logical: defines if the data has already been expanded, default is FALSE
#'
#' @export
SEQopts.expansion <- function(x = NULL, max = Inf, expansion = FALSE){
  params <- list(max = max, expansion = expansion)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

#' User facing helper function to create a parameter list for \code{SEQuential}
#' @param x List: list of other parameters to concatenate
#' @param covariates String: covariates to coerce into a formula object, eg. "A+B*C"
#'
#' @export
SEQopts.covariates <- function(x = NULL, covariates = NA){
  params <- list(covariates = covariates)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

#' User facing helper function to create a parameter list for \code{SEQuential}
#' @param x List: list of other parameters to concatenate
#' @param method String: type of analysis to preform
#'
#' @export
SEQopts.analysis <- function(x = NULL, method = "ITT"){
  params <- list(method = method)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

SEQopts.weight <- function(x = NULL, weighted = FALSE){
  params <- list(weighted = weighted)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

