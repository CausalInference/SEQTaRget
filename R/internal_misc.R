#' Internal Function to create 'default' formula
#' Assumes every column not explicitly given in \code{SEQuential} is a covariate, concatenating them with '+'
#'
#' @keywords internal
create.default.covariates <- function(params){
  timeVarying <- ""
  timeVarying_bas <- ""
  fixed <- ""
  trial <- ""
  tx_bas <- paste0(params@treatment, params@baseline.indicator)
  tx <- paste0(params@treatment, "+")
  followup <- paste0(paste0("followup", c("", params@squared.indicator), collapse = "+"), "+")
  dose <- paste0("dose", c("", params@squared.indicator), collapse = "+")
  interaction <- paste0(params@treatment, "*", "followup")

  if(length(params@time_varying) > 0) {
    timeVarying <- paste0(paste0(params@time_varying, collapse = "+"), "+")
    timeVarying_bas <- paste0(paste0(params@time_varying, params@baseline.indicator, collapse = "+"), "+")
  }

  if(length(params@fixed) > 0){
    fixed <- paste0(paste0(params@fixed, collapse = "+"), "+")
  }
  if(params@include.trial) trial <- paste0(paste0("trial", c("", params@squared.indicator), collapse = "+"), "+")
  if(params@include.period) period <- paste0(paste0("period", c("", params@squared.indicator), collapse = "+"), "+")

  if(params@method == "ITT") {
    out <- paste0(fixed, timeVarying_bas, paste0(tx_bas, "*", "followup"))
    return(out)
  }

  if(params@weighted){
    if(params@pre.expansion){
      if(params@method == "dose-response") out <- paste0(fixed, followup, period, dose)
      if(params@method == "censoring" & !params@excused) out <- paste0(fixed, tx, followup, trial, interaction)
      if(params@method == "censoring" & params@excused) out <- paste0(tx, followup, trial, interaction)
    } else if(!params@pre.expansion){
      if(params@method == "dose-response") out <- paste0(fixed, timeVarying_bas, followup, period, dose)
      if(params@method == "censoring" & !params@excused) out <- paste0(fixed, timeVarying_bas, tx, followup, trial, interaction)
      if(params@method == "censoring" & params@excused) out <- paste0(fixed, timeVarying_bas, followup, trial, interaction)
    }
    return(out)
  }
}

create.default.weight.covariates <- function(params, type){
  timeVarying <- ""
  timeVarying_bas <- ""
  fixed <- ""
  trial <- paste0("trial", c("", params@squared.indicator), collapse = "+")
  followup <- paste0(paste0("followup", c("", params@squared.indicator), collapse = "+"), "+")
  time <- paste0(params@time, c("", params@squared.indicator), collapse = "+")

  if(length(params@time_varying) > 0) {
    timeVarying <- paste0(paste0(params@time_varying, collapse = "+"), "+")
    timeVarying_bas <- paste0(paste0(params@time_varying, params@baseline.indicator, collapse = "+"), "+")
  }

  if(length(params@fixed) > 0){
    fixed <- paste0(paste0(params@fixed, collapse = "+"), "+")
  }

  if(type == "numerator"){
    if(params@pre.expansion){
      if(params@method == "dose-response") out <- paste0(fixed, time)
      if(params@method == "censoring" & !params@excused) out <- paste0(fixed, time)
    } else if(!params@pre.expansion){
      if(params@method == "dose-response") out <- paste0(fixed, timeVarying_bas, followup, trial)
      if(params@method == "censoring" & !params@excused) out <- paste0(fixed, timeVarying_bas, followup, trial)
      if(params@method == "censoring" & params@excused) out <- paste0(fixed, timeVarying_bas, followup, trial)
    }
  } else if(type == "denominator"){
    if(params@pre.expansion){
      if(params@method == "dose-response") out <- paste0(fixed, timeVarying, time)
      if(params@method == "censoring" & !params@excused) out <- paste0(fixed, timeVarying, time)
      if(params@method == "censoring" & params@excused) out <- paste0(fixed, timeVarying, time)
    } else if(!params@pre.expansion){
      if(params@method == "dose-response") out <- paste0(fixed, timeVarying, timeVarying_bas, followup, trial)
      if(params@method == "censoring" & !params@excused) out <- paste0(fixed, timeVarying, timeVarying_bas, followup, trial)
      if(params@method == "censoring" & params@excused) out <- paste0(fixed, timeVarying, timeVarying_bas, followup, trial)
    }
  }
  return(out)
}

create.risk <- function(data){
  table <- data[, .SD[.N], by = variable]
  rd <- round(as.numeric(table[2, 3] - table[1, 3]), 4)
  rr <- round(as.numeric(table[2, 3]/table[1, 3]), 4)

  return(list(rd = rd,
              rr = rr))
}
