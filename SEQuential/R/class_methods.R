setMethod("show", "SEQoutput", function(object) {
  elapsed_time <- slot(object, "time")
  params <- slot(object, "params")
  bootstrap <- slot(params, "bootstrap")
  bootstrap.nboot <- slot(params, "bootstrap.nboot")
  outcome <- slot(object, "outcome")
  numerator <- slot(object, "numerator")
  denominator <- slot(object, "denominator")
  outcome_model <- slot(object, "outcome.model")[[1]]
  weight_statistics <- slot(object, "weight.statistics")[[1]]
  risk_ratio <- slot(object, "risk.ratio")
  risk_difference <- slot(object, "risk.difference")

  cat("SEQuential process completed in", elapsed_time, ":\n")
  cat("Initialized with:\n")
  cat("Outcome covariates:", outcome, "\n")
  cat("Numerator covariates:", numerator, "\n")
  cat("Denominator covariates:", denominator, "\n\n")

  if (bootstrap) {
    cat("Bootstrapped", bootstrap.nboot, "times\n")
    cat("First Model Information ========================================== \n")
  } else {
    cat("Coefficients and Weighting:\n")
  }
  cat("\nOutcome Model ==================================================== \n")
  print(summary(outcome_model))

  if (length(weight_statistics) > 0) {
    cat("\nWeight Information ============================================= \n")
    cat("Treatment Lag = 0 Model ========================================== \n")
    cat("Numerator 0: \n")
    print(summary(weight_statistics$n0.coef))
    cat("Denominator: \n")
    print(summary(weight_statistics$d0.coef))

    cat("Treatment Lag = 1 Model ========================================== \n")
    cat("Numerator: \n")
    print(summary(weight_statistics$n1.coef))
    cat("Denominator: \n")
    print(summary(weight_statistics$d1.coef))

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
  cat("End of Followup Risk Ratio:\n", risk_ratio, "\n\n")
  cat("End of Followup Risk Difference:\n", risk_difference, "\n")
})

#' Retrieves Numerator Models from SEQuential object
#' @param object object of class SEQoutput
#'
#' @returns List of both numerator models
#' @importFrom methods is slot
#'
#' @export
numerator <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  weight_statistics <- slot(object, "weight_statistics")[[1]]
  return(list(numerator0 = weight_statistics$n0.coef,
              numerator1 = weight_statistics$n1.coef))
}

#' Retrieves Denominator Models from SEQuential object
#' @param object object of class SEQoutput
#'
#' @returns List of both numerator models
#' @importFrom methods is slot
#'
#' @export
denominator <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  weight_statistics <- slot(object, "weight_statistics")[[1]]
  return(list(denominator0 = weight_statistics$d0.coef,
              denominator1 = weight_statistics$d1.coef))
}

#' Retrieves Outcome Models from SEQuential object
#' @param object object of class SEQoutput
#'
#' @returns List of all outcome models
#' @importFrom methods is slot
#'
#' @export
outcome <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  return(object@outcome.model)
}
#' Retrieves Outcome, Numerator, and Denominator Covariates
#' @param object object of class SEQoutput
#'
#' @returns list of SEQuential covariates
#' @importFrom methods is slot
#'
#' @export
covariates <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  return(list(Outcome = object@outcome,
              Numerator = object@numerator,
              Denominator = object@denominator))
}

#' Function to print kaplan-meier curves
#'
#' @param object SEQoutput object to plot
#' @param plot.type character: type of plot to print
#' @param plot.title character: defines the title of the plot
#' @param plot.subtitle character: plot subtitle
#' @param plot.labels length 2 character: plot labels
#' @param plot.colors length 2 character: plot colors
#'
#' @importFrom methods is slot slot<-
#' @returns ggplot object of plot \code{plot.type}
#' @export
km.curve <- function(object, plot.type = c("survival", "risk", "inc"),
                     plot.title, plot.subtitle, plot.labels, plot.colors) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  params <- slot(object, "params")
  slot(params, "plot.type") <- plot.type
  slot(params, "plot.colors") <- plot.colors
  slot(params, "plot.title") <- plot.title
  slot(params, "plot.subtitle") <- plot.subtitle
  slot(params, "plot.labels") <- plot.labels

  out <- internal.plot(object@survival.data, params)
  return(out)
}

#' Function to return survival data from a SEQuential object
#'
#' @param object SEQoutput object
#'
#' @importFrom methods is slot
#' @returns dataframe of survival values
#' @export
km.data <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  return(slot(object, "survival.data"))
}

#' Function to return diagnostic tables from a SEQuential object
#'
#' @param object SEQoutput object
#'
#' @importFrom methods is slot
#' @returns list of diagnostic tables
#' @export
diagnostics <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  return(slot(object, "info"))
}
