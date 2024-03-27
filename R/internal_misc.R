#' Internal Function to translate \code{x} (covariates) and \code{y} to a formula object
#'
#' @keywords internal
create.formula <- function(y, x){
  paste0(y, "~", x)
}
#' Internal Function to create 'default' formula
#' Assumes every column not explicitly given in \code{SEQuential} is a covariate, concatenating them with '+'
#'
#' @keywords internal
create.default.covariates <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time.cols, fixed.cols, method){
  if(method == "ITT"){
    baseline.cols <- paste0(time.cols, "_bas", collapse = "+")
    fixed.cols <- paste0(fixed.cols, collapse = "+")
    cols <- paste0(fixed.cols, "+", baseline.cols)
    interactions <- paste0(treatment.col, "_bas*", "followup", collapse = "+")

    string <- paste0(interactions, "+", cols, "+", "followup+followup_sq")
  }
  return(string)
}

create.default.weight.covariates <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time.cols, fixed.cols, type, method){
  if(opts$pre.expansion){
    if(type == "numerator"){
      string <- paste0(paste0(fixed.cols, collapse = "+"), "+", time.col, "+", time.col, "_sq")
    } else {
      string <- paste0(paste0(c(fixed.cols, time.cols), collapse = "+"), "+", time.col, "+", time.col, "_sq")
    }
  } else {
    if(type == "numerator"){
      baseline.cols <- paste0(time.cols, "_bas", collapse = "+")
      fixed.cols <- paste0(fixed.cols, collapse = "+")
      string <- paste0(fixed.cols, "+", baseline.cols, "+followup+followup_sq")
    } else {
      baseline.cols <- paste0(time.cols, "_bas", collapse = "+")
      fixed.cols <- paste0(fixed.cols, collapse = "+")
      tv.cols <- paste0(time.cols, collapse = "+")
      string <- paste0(tv.cols, "+", fixed.cols, "+", baseline.cols, "+followup+followup_sq")
    }
  }
  return(string)
}
