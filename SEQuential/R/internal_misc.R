#' Internal Function to create 'default' formula
#' Assumes every column not explicitly given in \code{SEQuential} is a covariate, concatenating them with '+'
#'
#' @keywords internal
create.default.covariates <- function(params) {
  timeVarying <- ""
  timeVarying_bas <- ""
  fixed <- ""
  trial <- ""
  tx_bas <- paste0(params@treatment, params@baseline.indicator, "+")
  followup <- paste0(paste0("followup", c("", params@squared.indicator), collapse = "+"), "+")
  dose <- paste0("dose", c("", params@squared.indicator), collapse = "+")
  interaction <- paste0(params@treatment, params@baseline.indicator, "*", "followup")

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(paste0(params@time_varying, collapse = "+"), "+")
    timeVarying_bas <- paste0(paste0(params@time_varying, params@baseline.indicator, collapse = "+"), "+")
  }

  if (length(params@fixed) > 0) {
    fixed <- paste0(paste0(params@fixed, collapse = "+"), "+")
  }
  if (params@include.trial) trial <- paste0(paste0("trial", c("", params@squared.indicator), collapse = "+"), "+")
  if (params@include.period) period <- paste0(paste0("period", c("", params@squared.indicator), collapse = "+"), "+")

  if (params@method == "ITT") {
    out <- paste0(fixed, timeVarying_bas, interaction)
    return(out)
  }

  if (params@weighted) {
    if (params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(fixed, followup, period, dose)
      if (params@method == "censoring" & !params@excused) out <- paste0(fixed, tx_bas, followup, trial, interaction)
      if (params@method == "censoring" & params@excused) out <- paste0(tx_bas, followup, trial, interaction)
    } else if (!params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(fixed, timeVarying_bas, followup, period, dose)
      if (params@method == "censoring" & !params@excused) out <- paste0(fixed, timeVarying_bas, followup, trial, interaction)
      if (params@method == "censoring" & params@excused) out <- paste0(fixed, timeVarying_bas, followup, trial, interaction)
    }
    return(out)
  }
}

create.default.weight.covariates <- function(params, type) {
  timeVarying <- ""
  timeVarying_bas <- ""
  fixed <- ""
  trial <- paste0("trial", c("", params@squared.indicator), collapse = "+")
  followup <- paste0(paste0("followup", c("", params@squared.indicator), collapse = "+"), "+")
  time <- paste0(params@time, c("", params@squared.indicator), collapse = "+")

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(paste0(params@time_varying, collapse = "+"), "+")
    timeVarying_bas <- paste0(paste0(params@time_varying, params@baseline.indicator, collapse = "+"), "+")
  }

  if (length(params@fixed) > 0) {
    fixed <- paste0(paste0(params@fixed, collapse = "+"), "+")
  }

  if (type == "numerator") {
    if (params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(fixed, time)
      if (params@method == "censoring" & !params@excused) out <- paste0(fixed, time)
      if (params@method == "censoring" & params@excused) out <- NA_character_
    } else if (!params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(fixed, timeVarying_bas, followup, trial)
      if (params@method == "censoring" & !params@excused) out <- paste0(fixed, timeVarying_bas, followup, trial)
      if (params@method == "censoring" & params@excused) out <- paste0(fixed, timeVarying_bas, followup, trial)
    }
  } else if (type == "denominator") {
    if (params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(fixed, timeVarying, time)
      if (params@method == "censoring" & !params@excused) out <- paste0(fixed, timeVarying, time)
      if (params@method == "censoring" & params@excused) out <- paste0(fixed, timeVarying, time)
    } else if (!params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(fixed, timeVarying, timeVarying_bas, followup, trial)
      if (params@method == "censoring" & !params@excused) out <- paste0(fixed, timeVarying, timeVarying_bas, followup, trial)
      if (params@method == "censoring" & params@excused) out <- paste0(fixed, timeVarying, timeVarying_bas, followup, trial)
    }
  }
  return(out)
}

create.default.LTFU.covariates <- function(params){
  fixed <- ""
  timeVarying <- ""

  fixed <- paste0(params@fixed, collapse = "+")
  timeVarying <- paste0(params@time_varying, collapse = "+")

  out <- paste0(c(fixed, timeVarying), collapse = "+")

  return(out)
}

create.default.survival.covariates <- function(params) {
  if (params@method == "ITT") out <- paste0(params@covariates, "+", paste0(params@treatment, "*", c("followup", "followup_sq"), collapse = "+"))
  if (params@method == "dose-response") out <- paste0(params@covariates, "+", paste0("followup", "*", c("dose", "dose_sq"), collapse = "+"))
  if (params@method == "censoring") out <- params@covariates

  return(out)
}

create.risk <- function(data) {
  table <- data[, .SD[.N], by = "variable"]
  rd <- round(as.numeric(table[2, 3] - table[1, 3]), 4)
  rr <- round(as.numeric(table[2, 3] / table[1, 3]), 4)

  return(list(
    rd = rd,
    rr = rr
  ))
}

#' function loading ncores in global environment
#' @param pos defaults to 1 which equals an assignment to global environment
#' @param ncores ncores to assign to global
#'
#' @keywords internal
assign.global <- function(ncores, pos = 1) {
  assign("ncores", ncores, envir = as.environment(pos))
}

#' Helper Function to inline predict a fastglm object
#' @param model a fastglm object
#' @param newdata filler for a .SD from data.table
#' @param params parameter from SEQuential
#' @param type type of prediction
#'
#' @keywords internal

inline.pred <- function(model, newdata, params, type){
  if(type == "numerator") cols <- unlist(strsplit(params@numerator, "\\+"))
  if(type == "denominator") cols <- unlist(strsplit(params@denominator, "\\+"))
  data <- newdata[, cols, with = FALSE]

  X <- as.matrix(data)
  X <- cbind(X, rep(1, nrow(X)))
  pred <- predict(model, X, "response")
  return(pred)
}

#' Helper function to prepare data for fastglm
#' @param weight data after undergoing preparation
#' @param params parameter from SEQuential
#' @param type type of model, e.g. d0 = "denominator"
#' @param model model number, e.g. d0 = "zero model"
#'
#' @keywords internal

prepare.data <- function(weight, params, type, model, case){
  weight <- weight[!is.na(get(params@outcome))]
  if (case == "default") {
    if (type == "numerator") {
      cols <- unlist(strsplit(params@numerator, "\\+"))
      if(!params@excused) {
        if(model == 0) weight <- weight[tx_lag == 0, ]
        if(model == 1) weight <- weight[tx_lag == 1, ]

      } else {
        if (model == 0) weight <- weight[get(paste0(params@treatment, params@baseline.indicator)) == 0 &
                                           get(params@excused.col0) == 0 &
                                           isExcused < 1 &
                                           followup != 0, ]
        if(model == 1) weight <- weight[get(paste0(params@treatment, params@baseline.indicator)) == 1 &
                                          get(params@excused.col1) == 0 &
                                          isExcused < 1 &
                                          followup != 0, ]
      }
    } else if (type == "denominator"){
      cols <- unlist(strsplit(params@denominator, "\\+"))
      if(!params@excused) {
        if (model == 0) weight <- weight[tx_lag == 0, ]
        if (model == 1) weight <- weight[tx_lag == 1, ]
      } else {
        if(!params@pre.expansion){
          if (model == 0) weight <- weight[tx_lag == 0 &
                                             get(params@excused.col0) == 0 &
                                             isExcused < 1 &
                                             followup != 0, ]
          if (model == 1) weight <- weight[tx_lag == 1 &
                                             get(params@excused.col1) == 0 &
                                             isExcused < 1 &
                                             followup != 0, ]
        } else {
          if (model == 0) weight <- weight[tx_lag == 0 & get(params@excused.col0) == 0, ]
          if (model == 1) weight <- weight[tx_lag == 1 & get(params@excused.col1) == 0, ]
        }
      }
    }
    y <- weight[[params@treatment]]
    X <- as.matrix(weight[, cols, with = FALSE])
    X <- cbind(X, rep(1, nrow(X)))

  } else if (case == "LTFU") {
    cols <- unlist(strsplit(params@LTFU.covs, "\\+"))

    y <- weight[[params@cense]]
    X <- as.matrix(weight[, cols, with = FALSE])
    X <- cbind(X, rep(1, nrow(X)))
  }

  return(list(y = y, X = X))
}
