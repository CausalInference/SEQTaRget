#' An S4 class used to hold the outputs for the SEQuential process
#' @slot boostrap if the process was bootstrapped
#' @slot boot.sample how much of the data was sampled during bootstrapping
#' @slot boot.slice when using method 'show' defines what to print
#' @slot seed seeding any random sampling occuring in bootstrap process
#' @slot nboot number of bootstraps
#' @slot outcome outcome covariates
#' @slot numerator numerator covariates
#' @slot denominator denominator covariates
#' @slot outcome_model list of length \code{nboot} containing outcome coefficients
#' @slot hazard hazard ratio
#' @slot robust_se robust standard errors
#' @slot survival_curve ggplot object for the survival curves
#' @slot survival_data data.table of survival data
#' @slot risk_difference risk difference calculated from survival data
#' @slot risk_ratio risk ratio calculated from survival data
#' @slot elapsed_time time in minutes used for the SEQuential process
#' @slot weight_statistics information from the weighting process, containing weight coefficients and weight statistics
#'
setClass("SEQoutput",
  slots = c(
    bootstrap = "logical",
    boot.sample = "numeric",
    boot.slice = "integer",
    seed = "integer",
    nboot = "integer",
    outcome = "character",
    numerator = "character",
    denominator = "character",
    outcome_model = "list",
    hazard = "numeric",
    robust_se = "list",
    survival_curve = "ANY",
    survival_data = "ANY",
    risk_difference = "numeric",
    risk_ratio = "numeric",
    elapsed_time = "character",
    weight_statistics = "list"
  ), prototype = c(
    bootstrap = FALSE,
    boot.sample = NA_real_,
    boot.slice = 1L,
    seed = NA_integer_,
    nboot = NA_integer_,
    outcome = NA_character_,
    numerator = NA_character_,
    denominator = NA_character_,
    outcome_model = list(),
    hazard = NA_real_,
    robust_se = list(),
    survival_curve = NA,
    survival_data = data.table(),
    risk_difference = NA_real_,
    risk_ratio = NA_real_,
    elapsed_time = NA_character_,
    weight_statistics = list()
  )
)

setMethod("show", "SEQoutput", function(object) {
  elapsed_time <- slot(object, "elapsed_time")
  outcome <- slot(object, "outcome")
  numerator <- slot(object, "numerator")
  denominator <- slot(object, "denominator")
  bootstrap <- slot(object, "bootstrap")
  nboot <- slot(object, "nboot")
  boot_slice <- slot(object, "boot.slice")
  outcome_model <- slot(object, "outcome_model")[[1]][[1]]
  weight_statistics <- slot(object, "weight_statistics")[[1]][[1]]
  risk_ratio <- slot(object, "risk_ratio")
  risk_difference <- slot(object, "risk_difference")

  cat("SEQoutput process completed in", elapsed_time, ":\n")
  cat("Initialized with:\n")
  cat("Outcome covariates:", outcome, "\n")
  cat("Numerator covariates:", numerator, "\n")
  cat("Denominator covariates:", denominator, "\n\n")

  if (bootstrap) {
    cat("Bootstrapped", nboot, "times\n")
    cat("Coefficients and Weighting for bootstrap slice", boot_slice, ":\n")
  } else {
    cat("Coefficients and Weighting:\n")
  }

  outcome_model <- paste0(names(outcome_model), ": ", outcome_model, "\n")
  if (!is.na(weight_statistics$max)) {
    n0.coef <- paste0(names(weight_statistics$n0.coef), ": ", weight_statistics$n0.coef, "\n")
    n1.coef <- paste0(names(weight_statistics$n1.coef), ": ", weight_statistics$n1.coef, "\n")
    d0.coef <- paste0(names(weight_statistics$d0.coef), ": ", weight_statistics$d0.coef, "\n")
    d1.coef <- paste0(names(weight_statistics$d1.coef), ": ", weight_statistics$d1.coef, "\n")
  } else {
    n0.coef <- n1.coef <- d0.coef <- d1.coef <- NA
  }

  cat("\nOutcome Model: ", outcome_model, "\n")
  cat("Numerator 0 Model: ", n0.coef, "\n")
  cat("Numerator 1 Model: ", n1.coef, "\n")
  cat("Denominator 0 Model: ", d0.coef, "\n")
  cat("Denominator 1 Model: ", d1.coef, "\n\n")

  cat("Weights:\n")
  cat("Min: ", weight_statistics$min, "\n")
  cat("Max: ", weight_statistics$max, "\n")
  cat("StDev: ", weight_statistics$sd, "\n")
  cat("P25: ", weight_statistics$p25, "\n")
  cat("P50: ", weight_statistics$p50, "\n")
  cat("P75: ", weight_statistics$p75, "\n\n")

  cat("Risk Ratio:\n", risk_ratio, "\n\n")
  cat("Risk Difference:\n", risk_difference, "\n")
})
