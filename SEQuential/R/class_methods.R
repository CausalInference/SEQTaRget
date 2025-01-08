#' Function to explore bootstrapped results of a SEQuential object
#'
#' @param obj SEQuential object to explore
#' @param n bootstrap iteration to return
#'
#' @returns a SEQuential object where outcome_model and weight_statistics are limited to the bootstrap \code{n}
#'
#' @importFrom methods is slot slot<-
#'
#' @export
explore <- function(obj, n) {
  if (!is(obj, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (n > obj@bootstrap.nboot & obj@bootstrap) stop("Out of bounds bootstrap iteration")

  coef_iteration <- slot(obj, "outcome_model")[n]
  weight_iteration <- slot(obj, "weight_statistics")[n]

  slot(obj, "outcome_model") <- list(coef_iteration)
  slot(obj, "weight_statistics") <- list(weight_iteration)
  slot(obj, "boot.slice") <- as.integer(n)

  return(obj)
}

#' SEQoutput 'show' generic method
setMethod("show", "SEQoutput", function(object) {
  elapsed_time <- slot(object, "elapsed_time")
  outcome <- slot(object, "outcome")
  numerator <- slot(object, "numerator")
  denominator <- slot(object, "denominator")
  bootstrap <- slot(object, "bootstrap")
  bootstrap.nboot <- slot(object, "bootstrap.nboot")
  boot_slice <- slot(object, "boot.slice")
  outcome_model <- slot(object, "outcome_model")[[1]]
  weight_statistics <- slot(object, "weight_statistics")[[1]]
  risk_ratio <- slot(object, "risk_ratio")
  risk_difference <- slot(object, "risk_difference")

  cat("SEQuential process completed in", elapsed_time, ":\n")
  cat("Initialized with:\n")
  cat("Outcome covariates:", outcome, "\n")
  cat("Numerator covariates:", numerator, "\n")
  cat("Denominator covariates:", denominator, "\n\n")

  if (bootstrap) {
    cat("Bootstrapped", bootstrap.nboot, "times\n")
    cat("Coefficients and Weighting for bootstrap slice", boot_slice, ":\n")
  } else {
    cat("Coefficients and Weighting:\n")
  }
  cat("\nOutcome Model =================== \n")
  print(summary(outcome_model))

  if (!is.na(weight_statistics)) {
    cat("\nWeight Information ================ \n")
    n0.coef <- paste0(names(weight_statistics$n0.coef), ": ", weight_statistics$n0.coef, "\n")
    n1.coef <- paste0(names(weight_statistics$n1.coef), ": ", weight_statistics$n1.coef, "\n")
    d0.coef <- paste0(names(weight_statistics$d0.coef), ": ", weight_statistics$d0.coef, "\n")
    d1.coef <- paste0(names(weight_statistics$d1.coef), ": ", weight_statistics$d1.coef, "\n")

    cat("Numerator 0 Model: \n", n0.coef, "\n")
    cat("Numerator 1 Model: \n", n1.coef, "\n")
    cat("Denominator 0 Model: \n", d0.coef, "\n")
    cat("Denominator 1 Model: \n", d1.coef, "\n\n")

    cat("Weights:\n")
    cat("Min: ", weight_statistics$min, "\n")
    cat("Max: ", weight_statistics$max, "\n")
    cat("StDev: ", weight_statistics$sd, "\n")
    cat("P01: ", weight_statistics$p01, "\n")
    cat("P25: ", weight_statistics$p25, "\n")
    cat("P50: ", weight_statistics$p50, "\n")
    cat("P75: ", weight_statistics$p75, "\n")
    cat("P99: ", weight_statistics$p99, "\n\n")
  }
  cat("Risk Ratio:\n", risk_ratio, "\n\n")
  cat("Risk Difference:\n", risk_difference, "\n")
})

#setMethod("summary", "SEQoutput", function(object) {
#
#})
