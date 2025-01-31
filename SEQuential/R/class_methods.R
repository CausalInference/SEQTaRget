setMethod("show", "SEQoutput", function(object) {
  elapsed_time <- slot(object, "time")
  params <- slot(object, "params")
  bootstrap <- slot(params, "bootstrap")
  bootstrap.nboot <- slot(params, "bootstrap.nboot")
  outcome <- slot(object, "outcome")
  numerator <- slot(object, "numerator")
  denominator <- slot(object, "denominator")
  outcome_model <- lapply(slot(object, "outcome.model"), function(x) x[[1]])
  weight_statistics <- slot(object, "weight.statistics")[[1]][[1]]
  risk_ratio <- slot(object, "risk.ratio")
  risk_difference <- slot(object, "risk.difference")
  hazard <- slot(object, "hazard")

  cat("SEQuential process completed in", elapsed_time, ":\n")
  cat("Initialized with:\n")
  cat("Outcome covariates:", outcome, "\n")
  cat("Numerator covariates:", numerator, "\n")
  cat("Denominator covariates:", denominator, "\n\n")

  if (bootstrap) {
    cat("Bootstrapped", bootstrap.nboot, "times\n")
    cat("Full Model Information ========================================== \n")
  } else {
    cat("Coefficients and Weighting:\n")
  }
  cat("\nOutcome Model ==================================================== \n")
  for (i in seq_along(outcome_model)) {
    if (!is.na(params@subgroup)) cat("For subgroup: ", names(outcome_model)[[i]], "\n")
    print(summary(outcome_model[[1]]))
  }

  if (params@weighted) {
    cat("\nWeight Information ============================================= \n")
    if (params@method != "ITT") {
      cat("Treatment Lag = 0 Model ========================================== \n")
      if (length(weight_statistics$n0.coef) > 1) {
        cat("Numerator: \n")
        print(summary(weight_statistics$n0.coef)) 
      }
      cat("Denominator: \n")
      print(summary(weight_statistics$d0.coef))
      
      cat("Treatment Lag = 1 Model ========================================== \n")
      if (length(weight_statistics$n1.coef) > 1) {
        cat("Numerator: \n")
        print(summary(weight_statistics$n1.coef)) 
      }
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
    if (params@LTFU) {
      cat("LTFU Numerator: \n")
      print(summary(weight_statistics$ncense.coef))
      cat("LTFU Denominator: ")
      print(summary(weight_statistics$dcense.coef))
    }
  }
  if (!is.na(params@compevent)) {
    cat("Competing Event Model ============================================ \n")
    for (i in seq_along(slot(object, "ce.model"))) {
      if (!is.na(params@subgroup)) cat("For subgroup: ", names(ce.model)[[i]])
      print(summary(slot(object, "ce.model")[[i]][[1]])) 
    }
  }
  
  if (params@km.curves) {
    cat("Risk ==============================================================\n")
    for(i in seq_along(risk_difference)) {
      if (!is.na(params@subgroup)) cat("For subgroup: ", names(risk_difference)[[i]])
      cat("Followup time", params@survival.max, "Risk Ratio:\n", risk_ratio[[i]][1], "(", risk_ratio[[i]][2], ",", risk_ratio[[i]][3], ")", "\n\n")
      cat("Followup time", params@survival.max, "Risk Difference:\n", risk_difference[[i]][1], "(", risk_difference[[i]][2], ",", risk_difference[[i]][3], ")", "\n\n")
    }
  }
  
  if (params@hazard) {
    cat("Hazard ============================================================\n")
    for (i in seq_along(hazard)) {
      if (!is.na(params@subgroup)) cat("For subgroup:", names(hazard)[[i]], "\n")
      cat("Hazard Ratio: ", hazard[[i]], "\n")
    }
  }
  
  cat("\nDiagnostic Tables ================================================== \n")
  cat("Unique Outcome Table: ")
  print(slot(object, "info")$outcome.unique)
  cat("\nNon-Unique Outcome Table: ")
  print(slot(object, "info")$outcome.nonunique)
    
  if (slot(params, "method") == "censoring"){
    cat("\nUnique Switch Table: ")
    print(slot(object, "info")$switch.unique)
    cat("\nNon-Unique Switch Table: ")
    print(slot(object, "info")$switch.nonunique)
  }
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
  if (!object@params@weighted) stop("SEQuential process was not weighted")
  weight_statistics <- slot(object, "weight.statistics")
  return(list(numerator0 = lapply(object@weight.statistics, function(x) x[[1]]$n0.coef),
              numerator1 = lapply(object@weight.statistics, function(x) x[[1]]$n1.coef)))
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
  if (!object@params@weighted) stop("SEQuential process was not weighted")
  weight_statistics <- slot(object, "weight.statistics")
  return(list(denominator0 = lapply(object@weight.statistics, function(x) x[[1]]$d0.coef),
              denominator1 = lapply(object@weight.statistics, function(x) x[[1]]$d1.coef)))
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
km.curve <- function(object, plot.type = "survival",
                     plot.title, plot.subtitle, plot.labels, plot.colors) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!plot.type %in% c("survival", "risk", "inc")) stop("plot.type should be 'survival', 'risk', or 'inc'")
  if (!object@params@km.curves) stop("Survival Curves were not created as a result of 'km.curves=FALSE'")
  params <- slot(object, "params")
  
  if (!missing(plot.type)) slot(params, "plot.type") <- plot.type
  if (!missing(plot.colors)) slot(params, "plot.colors") <- plot.colors
  if (!missing(plot.title)) slot(params, "plot.title") <- plot.title
  if (!missing(plot.title)) slot(params, "plot.subtitle") <- plot.subtitle
  if (!missing(plot.labels)) slot(params, "plot.labels") <- plot.labels
  
  out <- c()
  groups <- if(!is.na(object@params@subgroup)) names(object@survival.data) else 1L
  for (i in seq_along(object@survival.data)) {
    label <- groups[[i]]
    out[[label]] <- internal.plot(object@survival.data[[i]], params)
  }
  return(out)
}

#' Function to return survival data from a SEQuential object
#'
#' @param object SEQoutput object
#'
#' @importFrom methods is slot
#' @returns list of dataframes of survival values
#' @export
km.data <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  return(slot(object, "survival.data"))
}

#' Function to return competing event models from a SEQuential object
#' 
#' @param object SEQoutput object
#' 
#' @importFrom methods is slot
#' @returns list of fastglm objects
#' @export
compevent <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (is.na(object@params@compevent)) stop("No competing event was specified during SEQuential process")
  return(slot(object, "ce.model"))
}

#' Function to return risk ratios from a SEQuential object
#' 
#' @param object SEQoutput object
#' @importFrom methods is slot
#' @returns list of risk ratios
#' @export
risk.ratio <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@km.curves) stop("Survival Data and Risks were not created through `km.curves = TRUE` in SEQuential process")
  return(slot(object, "risk.ratio"))
}

#' Function to return risk differences from a SEQuential object
#' 
#' @param object SEQoutput object
#' @importFrom methods is slot
#' @returns list of risk differences
#' @export
risk.ratio <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@km.curves) stop("Survival Data and Risks were not created through `km.curves = TRUE` in SEQuential process")
  return(slot(object, "risk.difference"))
}

#' Function to return hazard ratios from a SEQuential object
#' 
#' @param object SEQoutput object
#' @importFrom methods is slot
#' @returns list of hazard ratios
#' @export
hazard <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@hazard) stop("Hazard Ratios were not created through `hazard = TRUE` in SEQuential process")
  return(slot(object, "hazard"))
}

#' Function to return robust standard errors from a SEQuential object
#' 
#' @param object SEQoutput object
#' @importFrom methods is slot
#' @returns list of robust standard errors of outcome models
#' @export
robust <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@calculate.var) stop("Robust standard errors were not created through `calculate.var = TRUE` in SEQuential process")
  return(slot(object, "hazard"))
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
