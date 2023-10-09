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
#' @param memory Integer: number of bytes for in-memory use, default is 1GB less than system max
#' @param spark Logical: defines if spark should be used for out-of-memory processing
#'
#' @export
SEQopts.computation <- function(x = NULL, parallel = TRUE,
                           ncores = parallel::detectCores() - 1,
                           bootstrap = FALSE,
                           nboot = 100,
                           seed = 1636,
                           memory = get_ram() - 2^30,
                           spark = FALSE,
                           spark.connection = "local"){

  params <- list(parallel = parallel, ncores = ncores, bootstrap = bootstrap,
                 nboot = nboot, seed = seed, memory = memory, spark = spark,
                 spark.connection = spark.connection)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

#' User-facing helper function to create a parameter list for \code{SEQuential}
#' @param x List: list of other parameters to concatenate
#' @param max Integer: maximum time to expand about
#'
#' @export
SEQopts.expansion <- function(x = NULL, max = NULL){
  params <- list(min = min, max = max)

  if(!is.null(x)) return(c(x, params)) else return(params)
}

#' User facing helper function to create a parameter list for \code{SEQuential}
#' @param x List: list of other parameters to concatenate
#' @param covariates String: covariates to coerce into a formula object, eg. "A+B*C"
SEQopts.covariates <- function(x = NULL, covariates = NA){
  params <- list(covariates = covariates)


  if(!is.null(x)) return(c(x, params)) else return(params)
}

SEQopts.analysis <- function(x = NULL, type = "unweighted"){
  if(!type %in% c("unweighted", "weighted", "p99")) stop("Analysis type is limited to 'unweighted', 'weighted', or 'p99'")
}

SEQopts.weight <- function(x = NULL){

}

