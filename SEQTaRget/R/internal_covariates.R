#' Internal Function to create 'default' formula
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
  if (!params@km.curves) interaction <- interaction.dose <- NULL

  if (length(params@time_varying) > 0) {
    timeVarying <- paste0(params@time_varying, collapse = "+")
    timeVarying_bas <- paste0(params@time_varying, params@indicator.baseline, collapse = "+")
  }

  if (length(params@fixed) > 0) {
    if (!is.na(params@subgroup)) fixed <- params@fixed[!params@subgroup %in% params@fixed] else fixed <- params@fixed
    fixed <- if(length(fixed) > 0) paste0(fixed, collapse = "+") else NULL
  }
  if (params@trial.include) trial <- paste0("trial", c("", params@indicator.squared), collapse = "+")
  if (params@followup.include) followup <- paste0("followup", c("", params@indicator.squared)) else followup <- NULL
  if ((params@followup.spline | params@followup.class) & !params@followup.include) followup <- "followup" 

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
    if (params@method == "dose-response") out <- paste0(c(dose, followup, trial, fixed, timeVarying_bas, interaction.dose), collapse = "+")
    if (params@method == "censoring" & !params@excused) out <- paste0(c(tx_bas, followup, trial, fixed, timeVarying_bas, interaction), collapse = "+")
    if (params@method == "censoring" & params@excused) out <- paste0(c(tx_bas, followup, trial, fixed, timeVarying_bas, interaction), collapse = "+")
    return(out)
    }
}

#' Internal Function to create 'default' weighting formula
#'
#' @keywords internal
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

#' Internal Function to create 'default' loss-to-followup formula
#'
#' @keywords internal
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
