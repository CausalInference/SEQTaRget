#' Internal Function to create 'default' formula
#' Assumes every column not explicitly given in \code{SEQuential} is a covariate, concatenating them with '+'
#'
#' @keywords internal
create.default.covariates <- function(params) {
  timeVarying <- NULL
  timeVarying_bas <- NULL
  fixed <- NULL
  trial <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  dose <- paste0("dose", c("", params@indicator.squared), collapse = "+")
  interaction <- paste0(tx_bas, "*", "followup")
  interaction.dose <- paste0("followup*", c("dose", "dose_sq"), collapse = "+")
  if (params@hazard) interaction <- interaction.dose <- NULL

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(params@time_varying, collapse = "+")
    timeVarying_bas <- paste0(params@time_varying, params@indicator.baseline, collapse = "+")
  }

  if (length(params@fixed) > 0) {
    fixed <- paste0(params@fixed, collapse = "+")
  }
  if (params@trial.include) trial <- paste0("trial", c("", params@indicator.squared), collapse = "+")
  if (params@followup.include) followup <- paste0("followup", c("", params@indicator.squared)) else if (params@followup.spline | params@followup.class) followup <- "followup"

  if (params@method == "ITT") {
    out <- paste0(c(tx_bas, followup, trial, fixed, timeVarying_bas, interaction), collapse = "+")
    return(out)
  }

  if (params@weighted) {
    if (params@weight.preexpansion) {
      if (params@method == "dose-response") out <- paste0(c(dose, followup, trial, fixed, interaction.dose), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(tx_bas, followup, trial, fixed, interaction), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- paste0(c(tx_bas, followup, trial, interaction), collapse = "+")
    } else if (!params@weight.preexpansion) {
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

# TODO - in weight covariates, does trial.include and include.period affect time?
create.default.weight.covariates <- function(params, type) {
  timeVarying <- NULL
  timeVarying_bas <- NULL
  fixed <- NULL
  trial <- paste0("trial", c("", params@indicator.squared), collapse = "+")
  followup <- paste0("followup", c("", params@indicator.squared), collapse = "+")
  time <- paste0(params@time, c("", params@indicator.squared), collapse = "+")

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(params@time_varying, collapse = "+")
    timeVarying_bas <- paste0(params@time_varying, params@indicator.baseline, collapse = "+")
  }

  if (length(params@fixed) > 0) {
    fixed <- paste0(params@fixed, collapse = "+")
  }

  if (type == "numerator") {
    if (params@weight.preexpansion) {
      if (params@method == "dose-response") out <- paste0(c(fixed, time), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(fixed, time), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- NA_character_
    } else if (!params@weight.preexpansion) {
      if (params@method == "dose-response") out <- paste0(c(fixed, timeVarying_bas, followup, trial), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(fixed, timeVarying_bas, followup, trial), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- paste0(c(fixed, timeVarying_bas, followup, trial), collapse = "+")
    }
  } else if (type == "denominator") {
    if (params@weight.preexpansion) {
      if (params@method == "dose-response") out <- paste0(c(fixed, timeVarying, time), collapse = "+")
      if (params@method == "censoring" & !params@excused) out <- paste0(c(fixed, timeVarying, time), collapse = "+")
      if (params@method == "censoring" & params@excused) out <- paste0(c(fixed, timeVarying, time), collapse = "+")
    } else if (!params@weight.preexpansion) {
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
  followup <- NULL
  time <- paste0(params@time, c("", params@indicator.squared), collapse = "+")

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(params@time_varying, collapse = "+")
    timeVarying_bas <- paste0(params@time_varying, params@indicator.baseline, collapse = "+")
  }

  if (length(params@fixed) > 0) {
    fixed <- paste0(params@fixed, collapse = "+")
  }

  if (params@trial.include) trial <- paste0("trial", c("", params@indicator.squared), collapse = "+")
  if (params@followup.include) followup <- paste0("followup", c("", params@indicator.squared), collapse = "+")

  if (type == "numerator") {
    if (params@weight.preexpansion) out <- paste0(c("tx_lag", time, fixed), collapse = "+")
    if (!params@weight.preexpansion) out <- paste0(c("tx_lag", trial, followup, fixed, timeVarying_bas), collapse = "+")
  } else if (type == "denominator") {
    if (params@weight.preexpansion) out <- paste0(c("tx_lag", time, fixed, timeVarying), collapse = "+")
    if (!params@weight.preexpansion) out <- paste0(c("tx_lag", trial, followup, fixed, timeVarying, timeVarying_bas), collapse = "+")
  }

  return(out)
}

create.default.survival.covariates <- function(params) {
  timeVarying <- NULL
  timeVarying_bas <- NULL
  fixed <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  followup <- paste0("followup", c("", params@indicator.squared), collapse = "+")
  period <- paste0("period", c("", params@indicator.squared), collapse = "+")
  trial <- paste0("trial", c("", params@indicator.squared), collapse = "+")
  dose <- paste0("dose", c("", params@indicator.squared), collapse = "+")
  interaction <- paste0(tx_bas, "*", "followup")

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(params@time_varying, collapse = "+")
    timeVarying_bas <- paste0(params@time_varying, params@indicator.baseline, collapse = "+")
  }

  if (length(params@fixed) > 0) {
    fixed <- paste0(params@fixed, collapse = "+")
  }

  if (params@method == "ITT") out <- paste0(c(tx_bas, interaction, followup, trial, timeVarying_bas, fixed), collapse = "+")
  if (params@method == "censoring") {
    if (!params@weight.preexpansion) out <- paste0(c(tx_bas, interaction, followup, trial, timeVarying_bas, fixed), collapse = "+")
    if (params@weight.preexpansion) out <- paste0(c(fixed, tx_bas, followup, trial, interaction), collapse = "+")
  }
  if (params@method == "dose-response") {
    if (!params@weight.preexpansion) out <- paste0(c(fixed, timeVarying, timeVarying_bas, followup, trial), collapse = "+")
    if (params@weight.preexpansion) out <- paste0(c(fixed, followup, period, dose), collapse = "+")
  }

  return(out)
}
