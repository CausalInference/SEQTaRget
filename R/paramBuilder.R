#' Internal Parameter Builder - passing defaults to \code{SEQuential}
#'
buildParam <- function(){
  SEQcomputation() |>
    SEQcovariates()
}

#' User-facing helper function to create a 'SEQparam' object
#' @param parallel Logical: define if the SEQuential process is run in parallel, default is TRUE
#' @param ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @param bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#'
#' @export
SEQcomputation <- function(x = NULL, parallel = TRUE,
                           ncores = parallel::detectCores() - 1,
                           bootstrap = FALSE,
                           seed = 1636){
  params <- list(parallel = parallel, ncores = ncores, bootstrap = bootstrap, seed = seed)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

SEQcovariates <- function(x = NULL, covariates = NA){
  params <- list(covariates = covariates)

  if(!is.null(x)) return(c(x, params)) else return(params)
}
