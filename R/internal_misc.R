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
create.default.covariates <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, method){
  if(method == "ITT"){
    cols <- paste0(names(data)[!names(data) %in% c(id.col, eligible.col, outcome.col, time.col)], "_bas", collapse = "+")
    interactions <- paste0(treatment.col, "_bas*", "followup", collapse = "+")

    string <- paste0(interactions, "+", cols, "+", "followup+followup_sq")
  }
  return(string)
}

create.default.weight.covariates <- function(DT, data, id.col, time.col, eligible.col, treatment.col, opts){
  cols <- paste0(names(data)[!names(data) %in% c(id.col, eligible.col, treatment.col, time.col)])
  string <- paste0(cols, collapse = "+")

  return(string)
}
