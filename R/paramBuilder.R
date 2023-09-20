#' Internal Parameter Builder - passing defaults to \code{SEQuential}
#' @keywords internal
buildParam <- function(){
  SEQopts.expansion() |>
    SEQopts.computation() |>
    SEQopts.covariates()
}

#' User-facing helper function to create a parameter list for \code{SEQuential}
#' @param x List: list of other parameters to concatenate
#' @param parallel Logical: define if the SEQuential process is run in parallel, default is TRUE
#' @param ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @param bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @param nboot Integer: number of bootstrap
#' @param seed Integer: starting seed
#'
#' @export
#'

SEQopts.computation <- function(x = NULL, parallel = TRUE,
                           ncores = parallel::detectCores() - 1,
                           bootstrap = FALSE,
                           nboot = 100,
                           seed = 1636){
  params <- list(parallel = parallel, ncores = ncores, bootstrap = bootstrap, nboot = nboot, seed = seed)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

#' User-facing helper function to create a parameter list for \code{SEQuential}
#' @param x List: list of other parameters to concatenate
#' @param min Integer: minimum time to expand about
#' @param max Integer: maximim time to expand about
#'
#' @export

SEQopts.expansion <- function(x = NULL, min = NULL, max = NULL){
  params <- list(min = min, max = max)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

SEQopts.covariates <- function(x = NULL, covariates = NA){
  params <- list(covariates = covariates)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

SEQopts.analysis <- function(x = NULL, type = "unweighted"){
  if(!type %in% c("unweighted", "weighted", "p99")) stop("Analysis type is limited to 'unweighted', 'weighted', or 'p99'")
}

SEQopts.weight <- function(x = NULL){

}
