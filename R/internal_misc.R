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
create.default.formula <- function(data, id.col, eligible.col, treatment.col, outcome.col, method){
  if(method == "ITT"){
    cols <- paste0(names(data)[!names(data) %in% c(id.col, eligible.col, outcome.col)], "_bas", collapse = "+")
    interactions <- paste0(treatment.col, "_bas*", c("followup", "followup_sq"), collapse = "+")
    string <- paste0(outcome.col, "~", paste0(interactions, cols, "followup", "followup_sq", collapse = "+"))
    formula <- as.formula(paste0(outcome.col, "~", paste0(interactions, cols, "followup", "followup_sq", collapse = "+")))

  }
  cols <- paste(names(data)[!names(data) %in% c(outcome.col, id.col, eligible.col)], collapse = "+")
  return(as.formula(paste0(outcome.col, "~", cols)))
}
