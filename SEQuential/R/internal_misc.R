#' Internal Function to create 'default' formula
#' Assumes every column not explicitly given in \code{SEQuential} is a covariate, concatenating them with '+'
#'
#' @keywords internal
create.default.covariates <- function(params) {
  timeVarying <- NULL
  timeVarying_bas <- NULL
  fixed <- NULL
  trial <- NULL
  tx_bas <- paste0(params@treatment, params@baseline.indicator)
  followup <- paste0("followup", c("", params@squared.indicator), collapse = "+")
  dose <- paste0("dose", c("", params@squared.indicator), collapse = "+")
  interaction <- paste0(params@treatment, params@baseline.indicator, "*", "followup")

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(params@time_varying, collapse = "+")
    timeVarying_bas <- paste0(params@time_varying, params@baseline.indicator, collapse = "+")
  }

  if (length(params@fixed) > 0) {
    fixed <- paste0(params@fixed, collapse = "+")
  }
  if (params@include.trial) trial <- paste0("trial", c("", params@squared.indicator), collapse = "+")
  if (params@include.period) period <- paste0("period", c("", params@squared.indicator), collapse = "+")

  if (params@method == "ITT") {
    if (params@pre.expansion) out <- paste0(c(fixed, timeVarying_bas, interaction), collapse = "+")
    if (!params@pre.expansion) out <- paste0(c(fixed, timeVarying_bas, trial, interaction), collapse = "+")
    return(out)
  }

  if (params@weighted) {
    if (params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(c(fixed, followup, period, dose), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(fixed, tx_bas, followup, trial, interaction), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- paste0(c(tx_bas, followup, trial, interaction), collapse = "+")
    } else if (!params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(c(fixed, timeVarying_bas, followup, period, dose), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(fixed, timeVarying_bas, followup, trial, interaction), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- paste0(c(fixed, timeVarying_bas, followup, trial, interaction), collapse = "+")
    }
    return(out)
  }
}

# TODO - in weight covariates, does include.trial and include.period affect time?
create.default.weight.covariates <- function(params, type) {
  timeVarying <- NULL
  timeVarying_bas <- NULL
  fixed <- NULL
  trial <- paste0("trial", c("", params@squared.indicator), collapse = "+")
  followup <- paste0("followup", c("", params@squared.indicator), collapse = "+")
  time <- paste0(params@time, c("", params@squared.indicator), collapse = "+")

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(params@time_varying, collapse = "+")
    timeVarying_bas <- paste0(params@time_varying, params@baseline.indicator, collapse = "+")
  }

  if (length(params@fixed) > 0) {
    fixed <- paste0(params@fixed, collapse = "+")
  }

  if (type == "numerator") {
    if (params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(c(fixed, time), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(fixed, time), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- NA_character_
    } else if (!params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(c(fixed, timeVarying_bas, followup, trial), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(fixed, timeVarying_bas, followup, trial), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- paste0(c(fixed, timeVarying_bas, followup, trial), collapse = "+")
    }
  } else if (type == "denominator") {
    if (params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(c(fixed, timeVarying, time), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(fixed, timeVarying, time), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- paste0(c(fixed, timeVarying, time), collapse = "+")
    } else if (!params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(c(fixed, timeVarying, timeVarying_bas, followup, trial), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(fixed, timeVarying, timeVarying_bas, followup, trial), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- paste0(c(fixed, timeVarying, timeVarying_bas, followup, trial), collapse = "+")
    }
  }
  return(out)
}

create.default.LTFU.covariates <- function(params, type){
  timeVarying <- NULL
  timeVarying_bas <- NULL
  fixed <- NULL
  trial <- NULL
  period <- NULL
  followup <- paste0("followup", c("", params@squared.indicator), collapse = "+")
  time <- paste0(params@time, c("", params@squared.indicator), collapse = "+")
#  tx_bas <- paste0(params@treatment, params@baseline.indicator)

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(params@time_varying, collapse = "+")
    timeVarying_bas <- paste0(params@time_varying, params@baseline.indicator, collapse = "+")
  }

  if (length(params@fixed) > 0) {
    fixed <- paste0(params@fixed, collapse = "+")
  }

  if (params@include.trial) trial <- paste0("trial", c("", params@squared.indicator), collapse = "+")
  if (params@include.period) period <- paste0("period", c("", params@squared.indicator), collapse = "+")

  if (type == "numerator") {
    if (params@pre.expansion) out <- paste0(c("tx_lag", time, fixed), collapse = "+")
    if (!params@pre.expansion) out <- paste0(c("tx_lag", trial, followup, fixed, timeVarying_bas), collapse = "+")
  } else if (type == "denominator") {
    if (params@pre.expansion) out <- paste0(c("tx_lag", time, fixed, timeVarying), collapse = "+")
    if (!params@pre.expansion) out <- paste0(c("tx_lag", trial, followup, fixed, timeVarying, timeVarying_bas), collapse = "+")
  }

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

inline.pred <- function(model, newdata, params, type, case = "default"){
  if (case == "default") {
    if(type == "numerator") cols <- unlist(strsplit(params@numerator, "\\+"))
    if(type == "denominator") cols <- unlist(strsplit(params@denominator, "\\+"))
  }
  if(case == "LTFU") {
    if (type == "numerator") cols <- unlist(strsplit(params@ltfu.numerator, "\\+"))
    if (type == "denominator") cols <- unlist(strsplit(params@ltfu.denominator, "\\+"))
  }
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
  followup <- NULL
  isExcused <- NULL
  tx_lag <- NULL

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
    weight <- weight[!is.na(get(params@cense)), ]
    if (type == "numerator") cols <- unlist(strsplit(params@ltfu.numerator, "\\+"))
    if (type == "denominator") cols <- unlist(strsplit(params@ltfu.denominator, "\\+"))
    ykept <- c(params@cense)

    y <- abs(weight[[params@cense]] - 1)
    X <- as.matrix(weight[, paste0(params@time, params@squared.indicator) := get(params@time)^2
                          ][, cols, with = FALSE])
    X <- cbind(X, rep(1, nrow(X)))
  }
  return(list(y = y, X = X))
}
