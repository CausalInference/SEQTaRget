#' Internal Function to translate \code{x} (covariates) and \code{y} to a formula object
#'
#' @keywords internal
create.formula <- function(y, x){
  return(as.formula(paste0(y, "~", x)))
}
#' Internal Function to create 'default' formula
#' Assumes every column not explicitly given in \code{SEQuential} is a covariate, concatenating them with '+'
#'
#' @keywords internal
create.default.formula <- function(data){
  cols <- paste(names(data)[!names(data) %in% outcome.col], collapse = "+")
  return(as.formula(paste0(outcome.col, "~", cols)))
}
