#' Show method for S4 object - SEQoutput.
#' 
#' @param object A SEQoutput object - usually generated from \code{SEQuential}
#' @importFrom knitr kable
#' @importMethodsFrom methods show
#' 
#' @returns No return value, sends information about SEQoutput to the console
#' @exportMethod show
setMethod("show", "SEQoutput", function(object) {
  elapsed_time <- slot(object, "time")
  params <- slot(object, "params")
  bootstrap <- slot(params, "bootstrap")
  bootstrap.nboot <- slot(params, "bootstrap.nboot")
  outcome <- slot(object, "outcome")
  numerator <- slot(object, "numerator")
  denominator <- slot(object, "denominator")
  hazard <- slot(object, "hazard")
  risk.data <- slot(object, "risk.data")
  risk.comparison <- slot(object, "risk.comparison")
  if (!params@hazard) {
    outcome_model <- lapply(slot(object, "outcome.model"), function(x) x[[1]])
    weight_statistics <- slot(object, "weight.statistics")[[1]][[1]]
  }

  cat("SEQuential process completed in", elapsed_time, ":\n")
  cat("Initialized with:\n")
  cat("Outcome covariates:", outcome, "\n")
  cat("Numerator covariates:", numerator, "\n")
  cat("Denominator covariates:", denominator, "\n\n")

  if (bootstrap) {
    cat("Bootstrapped", bootstrap.nboot, "times\n")
  } 
  if (!params@hazard) {
    cat("Full Model Information ========================================== \n")
    cat("\nOutcome Model ==================================================== \n")
    cat("Coefficients and Weighting:\n")
    for (i in seq_along(outcome_model)) {
      if (!is.na(params@subgroup)) cat("For subgroup: ", names(outcome_model)[[i]], "\n")
      print(summary(outcome_model[[i]]))
    }
    
    if (params@weighted) {
      cat("\nWeight Information ============================================= \n")
      if (params@method != "ITT") {
        for (i in seq_along(params@treat.level)) {
          if (params@weight.lag_condition) {
            cat("Treatment Lag =", params@treat.level[[i]], "Treatment =", params@treat.level[[i]], "Model ====================================\n")
          } else {
            cat("Treatment =", params@treat.level[[i]], "Model ====================================\n")
          }
          if (length(weight_statistics$coef.numerator) > 1) {
            cat("Numerator ========================== \n")
            if (!params@multinomial | 
                (params@multinomial & !params@weight.preexpansion & 
                 (params@excused | params@deviation.excused))
                ) print(summary(weight_statistics$coef.numerator[[i]])) else {
              nonbaseline <- params@treat.level[-1]
              for (j in seq_along(nonbaseline)) {
                cat("Nested Model: Treatment =", nonbaseline[[j]], "=========\n")
                print(summary(weight_statistics$coef.numerator[[i]]$models[[j]]))
              }
            }
          }
            cat("Denominator ========================== \n")
            if (!params@multinomial | 
                (params@multinomial & !params@weight.preexpansion & 
                 (params@excused | params@deviation.excused))
                ) print(summary(weight_statistics$coef.denominator[[i]])) else {
              nonbaseline <- params@treat.level[-1]
              for (j in seq_along(nonbaseline)) {
                cat("Nested Model: Treatment =", nonbaseline[[j]], "=========\n")
                print(summary(weight_statistics$coef.denominator[[i]]$models[[j]]))
              }
            }
          }
        }
      
      cat("Weights:\n")
      cat("Min: ", weight_statistics$min, "\n")
      cat("Max: ", weight_statistics$max, "\n")
      cat("StDev: ", weight_statistics$sd, "\n")
      cat("P01: ", weight_statistics$p01, "\n")
      cat("P25: ", weight_statistics$p25, "\n")
      cat("P50: ", weight_statistics$p50, "\n")
      cat("P75: ", weight_statistics$p75, "\n")
      cat("P99: ", weight_statistics$p99, "\n\n")
        
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
        if (!is.na(params@subgroup)) cat("For subgroup: ", slot(object, "ce.model")[[i]], "\n")
        print(summary(slot(object, "ce.model")[[i]][[1]])) 
      }
    }
    
    if (params@km.curves) {
      cat("Risk ==============================================================\n")
      for(i in seq_along(risk.data)) {
        if (!is.na(params@subgroup)) cat("For subgroup: ", names(risk.data)[[i]], "\n")
        print(kable(risk.data[[i]]))
        print(kable(risk.comparison[[i]]))
      }
    }
  
  } else {
    cat("Hazard ============================================================\n")
    for (i in seq_along(hazard)) {
      if (!is.na(params@subgroup)) cat("For subgroup:", names(hazard)[[i]], "\n")
      cat("Hazard Ratio: ", hazard[[i]], "\n")
    }
  }
  
  cat("\nDiagnostic Tables ================================================== \n")
  outcome.unique <- slot(object, "info")$outcome.unique
  outcome.nonunique <- slot(object, "info")$outcome.nonunique
  for (i in seq_along(outcome.unique)) {
    if (!is.na(params@subgroup)) cat("For subgroup: ", names(outcome.unique)[[i]], "\n")
    cat("Unique Outcome Table: ")
    print(kable(outcome.unique[[i]]))
    cat("\nNon-Unique Outcome Table: ")
    print(kable(outcome.nonunique[[i]]))
  }
    
  if (slot(params, "method") == "censoring"){
    cat("\nUnique Switch Table: ")
    print(kable(slot(object, "info")$switch.unique))
    cat("\nNon-Unique Switch Table: ")
    print(kable(slot(object, "info")$switch.nonunique))
  }
  
  if (!is.na(slot(params, "compevent"))) {
    cat("\nUnique Competing Event Table: ")
    print(slot(object, "info")$compevent.unique)
    cat("\nNon-Unique Competing Event Table: ")
    print(slot(object, "info")$compevent.nonunique)
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
km_curve <- function(object, plot.type = "survival",
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
km_data <- function(object) {
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

#' Function to return risk information from a SEQuential object
#' 
#' @param object SEQoutput object
#' @importFrom methods is slot
#' @returns a data table of risk information at every followup
#' @export
risk_data <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@km.curves) stop("Survival Data and Risks were not created through `km.curves = TRUE` in SEQuential process")
  return(slot(object, "risk.data"))
}

#' Function to return risk information from a SEQuential object
#' 
#' @param object SEQoutput object
#' @importFrom methods is slot
#' @returns a data frame of risk information at end of followup (risk ratios, risk differences and confidence intervals, if bootstrapped)
#' @export
risk_comparison <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@km.curves) stop("Survival Data and Risks were not created through `km.curves = TRUE` in SEQuential process")
  return(slot(object, "risk.comparison"))
}

#' Function to return hazard ratios from a SEQuential object
#' 
#' @param object SEQoutput object
#' @importFrom methods is slot
#' @returns list of hazard ratios
#' @export
hazard_ratio <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@hazard) stop("Hazard Ratios were not created through `hazard = TRUE` in SEQuential process")
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

#' Function to return the internal data from a SEQuential object
#' 
#' @param object SEQoutput object
#' 
#' @importFrom methods is slot
#' @returns data.table
#' @export
SEQ_data <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@data.return) stop ("Data was not attached through `data.return = TRUE` in SEQuential process")
  return(slot(object, "DT"))
}
