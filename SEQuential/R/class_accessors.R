#' Function to explore bootstrapped results of a SEQuential object
#'
#' @param obj SEQuential object to explore
#' @param n bootstrap iteration to return
#'
#' @returns a SEQuential object where outcome_model and weight_statistics are limited to the bootstrap \code{n}
#'
#' @importFrom methods is slot slot<-
#'
#' @export
explore <- function(obj, n){
  if(!is(obj, "SEQoutput")) stop("Object is not of class SEQoutput")
  if(n > obj@nboot & obj@bootstrap) stop("Out of bounds bootstrap iteration")

  coef_iteration <- slot(obj, "outcome_model")[n]
  weight_iteration <- slot(obj, "weight_statistics")[n]

  slot(obj, "outcome_model") <- list(coef_iteration)
  slot(obj, "weight_statistics") <- list(weight_iteration)
  slot(obj, "boot.slice") <- as.integer(n)

  return(obj)
}
