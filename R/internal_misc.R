#' Internal Function to create 'default' formula
#' Assumes every column not explicitly given in \code{SEQuential} is a covariate, concatenating them with '+'
#'
#' @keywords internal
create.default.covariates <- function(params){
  if(params@method == "ITT"){
    baseline.cols <- paste0(params@time_varying, params@baseline.indicator, collapse = "+")
    fixed.cols <- paste0(params@fixed, collapse = "+")
    cols <- paste0(fixed.cols, "+", baseline.cols)
    interactions <- paste0(params@treatment, "_bas*", "followup", collapse = "+")

    string <- paste0(interactions, "+", cols, "+", "followup+followup_sq")

  } else if(params@method %in% c("dose-response", "censoring")){
    if(params@pre.expansion){
      if(params@excused){
        cols <- NULL
      } else {
        cols <- paste0(paste0(params@fixed, collapse="+"), "+")
      }
    } else {
      baseline.cols <- paste0(params@time_varying, params@baseline.indicator, collapse = "+")
      fixed.cols <- paste0(params@fixed, collapse = "+")
      cols <- paste0(fixed.cols, "+", baseline.cols)
    }
    string <- paste0(cols, "+", paste0(c("followup", "trial", paste0(c("followup", "trial"), params@squared.indicator, collapse = "+")), collapse = "+"))

    if(params@method == "dose-resonse") string <- paste0(string, "+dose+dose_sq")
    if(params@method == "censoring"){
      if(params@excused) tx <- paste0(params@treatment, params@baseline.indicator) else tx <- params@treatment
      string <- paste0(tx, "+", string, "+", paste0(tx, "*followup"))
    }
  }
  return(string)
}

create.default.weight.covariates <- function(params, type){
  if(params@method == "ITT"){
    if(type == "numerator"){
      string <- paste0(paste0(params@fixed, collapse = "+"), "+", params@time, "+", params@time, params@squared.indicator)
    } else {
      string <- paste0(paste0(c(params@fixed, params@time_varying), collapse = "+"), "+", params@time, "+", params@time, params@squared.indicator)
    }
  } else {
    if(type == "numerator"){
      baseline.cols <- paste0(params@time_varying, params@baseline.indicator, collapse = "+")
      fixed.cols <- paste0(params@fixed, collapse = "+")
      string <- paste0(fixed.cols, "+", baseline.cols, "+followup+followup_sq+trial+trial_sq")
    } else {
      baseline.cols <- paste0(params@time_varying, "_bas", collapse = "+")
      fixed.cols <- paste0(params@fixed, collapse = "+")
      tv.cols <- paste0(params@time_varying, collapse = "+")
      string <- paste0(tv.cols, "+", fixed.cols, "+", baseline.cols, "+followup+followup_sq+trial+trial_sq")
    }
  }
  return(string)
}

create.risk <- function(data){
  table <- data[, .SD[.N], by = variable]
  rd <- round(as.numeric(table[2, 3] - table[1, 3]), 4)
  rr <- round(as.numeric(table[2, 3]/table[1, 3]), 4)

  return(list(rd = rd,
              rr = rr))
}
