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
  interaction <- paste0(tx_bas, "*", "followup")
  interaction.dose <- paste0("followup*", c("dose", "dose_sq"), collapse = "+")
  if (params@hazard) interaction <- interaction.dose <- NULL

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
    out <- paste0(c(tx_bas, followup, trial, fixed, timeVarying_bas, interaction), collapse = "+")
    return(out)
  }

  if (params@weighted) {
    if (params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(c(dose, followup, trial, fixed, interaction.dose), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(tx_bas, followup, trial, fixed, interaction), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- paste0(c(tx_bas, followup, trial, interaction), collapse = "+")
    } else if (!params@pre.expansion) {
      if (params@method == "dose-response") out <- paste0(c(dose, followup, trial, fixed, timeVarying_bas, interaction.dose), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(tx_bas, followup, trial, fixed, timeVarying_bas, interaction), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- paste0(c(tx_bas, followup, trial, fixed, timeVarying_bas, interaction), collapse = "+")
    }
    return(out)
  } else {
    #TODO - confirm with alejandro about unweighted model covariates
    if (params@method == "dose-response") out <- paste0(c(dose, followup, trial, fixed, timeVarying_bas, interaction.dose), collapse = "+")
    if (params@method == "censoring" & !params@excused) out <- paste0(c(tx_bas, followup, trial, fixed, timeVarying_bas, interaction), collapse = "+")
    if (params@method == "censoring" & params@excused) out <- paste0(c(tx_bas, followup, trial, fixed, timeVarying_bas, interaction), collapse = "+")
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
  timeVarying <- NULL
  timeVarying_bas <- NULL
  fixed <- NULL
  tx_bas <- paste0(params@treatment, params@baseline.indicator)
  followup <- paste0("followup", c("", params@squared.indicator), collapse = "+")
  period <- paste0("period", c("", params@squared.indicator), collapse = "+")
  trial <- paste0("trial", c("", params@squared.indicator), collapse = "+")
  dose <- paste0("dose", c("", params@squared.indicator), collapse = "+")
  interaction <- paste0(tx_bas, "*", "followup")

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(params@time_varying, collapse = "+")
    timeVarying_bas <- paste0(params@time_varying, params@baseline.indicator, collapse = "+")
  }

  if (length(params@fixed) > 0) {
    fixed <- paste0(params@fixed, collapse = "+")
  }

  if (params@method == "ITT") out <- paste0(c(tx_bas, interaction, followup, trial, timeVarying_bas, fixed), collapse = "+")
  if (params@method == "censoring") {
    if (!params@pre.expansion) out <- paste0(c(tx_bas, interaction, followup, trial, timeVarying_bas, fixed), collapse = "+")
    if (params@pre.expansion) out <- paste0(c(fixed, tx_bas, followup, trial, interaction), collapse = "+")
  }
  if (params@method == "dose-response") {
    if (!params@pre.expansion) out <- paste0(c(fixed, timeVarying, timeVarying_bas, followup, trial), collapse = "+")
    if (params@pre.expansion) out <- paste0(c(fixed, followup, period, dose), collapse = "+")
  }

  return(out)
}
