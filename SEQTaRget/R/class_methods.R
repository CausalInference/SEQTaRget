#' Print a coefficient table from a cleaned fastglm object
#' @param model a fastglm object (may have row-level vectors stripped)
#' @keywords internal
.coef_table <- function(model) {
  if (is.null(model) || !is.list(model)) return(invisible(NULL))
  coefs <- model$coefficients
  if (is.null(coefs)) return(invisible(NULL))
  se <- model$se
  if (!is.null(se) && length(se) == length(coefs)) {
    data.frame(Estimate = coefs, `Std. Error` = se, check.names = FALSE)
  } else {
    data.frame(Estimate = coefs)
  }
}

#' Show method for S4 object - SEQoutput.
#'
#' @param object A SEQoutput object - usually generated from [SEQuential()]
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
    # No outcome model is fit for an end-of-follow-up outcome
    if (!params@end_of_fup) outcome_model <- lapply(slot(object, "outcome.model"), function(x) x[[1]])
    weight_statistics <- slot(object, "weight.statistics")[[1]][[1]]
  }

  cat("SEQuential process completed in", elapsed_time, ":\n")
  cat("Initialized with:\n")
  if (params@end_of_fup) {
    cat("End-of-follow-up outcome:", params@outcome,
        paste0("(", params@end_of_fup.type, ")"), "at follow-up time", params@end_of_fup.time,
        if (params@end_of_fup.window > 0) paste0("+/- ", params@end_of_fup.window) else "", "\n")
  } else cat("Outcome covariates:", outcome, "\n")
  cat("Numerator covariates:", paste(numerator, collapse = " | "), "\n")
  cat("Denominator covariates:", paste(denominator, collapse = " | "), "\n\n")

  if (bootstrap) {
    cat("Bootstrapped", bootstrap.nboot, "times\n")
  } 
  if (!params@hazard) {
    cat("Full Model Information ========================================== \n")
    if (!params@end_of_fup) {
      cat("\nOutcome Model ==================================================== \n")
      cat("Coefficients and Weighting:\n")
      for (i in seq_along(outcome_model)) {
        if (!is.na(params@subgroup)) cat("For subgroup: ", names(outcome_model)[[i]], "\n")
        print(.coef_table(outcome_model[[i]]))
      }
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
                ) print(.coef_table(weight_statistics$coef.numerator[[i]])) else {
              nonbaseline <- params@treat.level[-1]
              for (j in seq_along(nonbaseline)) {
                cat("Nested Model: Treatment =", nonbaseline[[j]], "=========\n")
                print(.coef_table(weight_statistics$coef.numerator[[i]]$models[[j]]))
              }
            }
          }
            cat("Denominator ========================== \n")
            if (!params@multinomial |
                (params@multinomial & !params@weight.preexpansion &
                 (params@excused | params@deviation.excused))
                ) print(.coef_table(weight_statistics$coef.denominator[[i]])) else {
              nonbaseline <- params@treat.level[-1]
              for (j in seq_along(nonbaseline)) {
                cat("Nested Model: Treatment =", nonbaseline[[j]], "=========\n")
                print(.coef_table(weight_statistics$coef.denominator[[i]]$models[[j]]))
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
        print(.coef_table(weight_statistics$ncense.coef))
        cat("LTFU Denominator: ")
        print(.coef_table(weight_statistics$dcense.coef))
      }
    }
    if (!is.na(params@compevent)) {
      cat("Competing Event Model ============================================ \n")
      ce.models <- slot(object, "ce.model")
      for (i in seq_along(ce.models)) {
        if (is.null(ce.models[[i]]) || length(ce.models[[i]]) == 0) next
        if (!is.na(params@subgroup)) cat("For subgroup: ", names(ce.models)[[i]], "\n")
        print(.coef_table(ce.models[[i]][[1]]))
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

    if (params@end_of_fup) {
      eof.data <- slot(object, "eof.data")
      eof.comparison <- slot(object, "eof.comparison")
      cat("End-of-Follow-up Outcome ==========================================\n")
      for (i in seq_along(eof.data)) {
        if (!is.na(params@subgroup)) cat("For subgroup: ", names(eof.data)[[i]], "\n")
        print(kable(eof.data[[i]]))
        print(kable(eof.comparison[[i]]))
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
  followup.unique <- slot(object, "info")$followup.unique
  followup.nonunique <- slot(object, "info")$followup.nonunique
  # A continuous end-of-follow-up outcome has no event counts, so the outcome
  # tables are NA there; drive the loop off the follow-up tables, which are
  # always present, so their subgroups still print.
  has_outcome_tables <- is.list(outcome.unique)
  labelled <- if (has_outcome_tables) outcome.unique else followup.unique
  for (i in seq_along(labelled)) {
    if (!is.na(params@subgroup)) cat("For subgroup: ", names(labelled)[[i]], "\n")
    if (has_outcome_tables) {
      cat("Unique Outcome Table (distinct subjects who had the outcome): ")
      print(kable(outcome.unique[[i]]))
      cat("\nNon-Unique Outcome Table (total outcome events): ")
      print(kable(outcome.nonunique[[i]]))
    }
    if (!is.null(followup.unique)) {
      cat("\nUnique Follow-up Table (distinct subjects contributing follow-up): ")
      print(kable(followup.unique[[i]]))
    }
    if (!is.null(followup.nonunique)) {
      cat("\nNon-Unique Follow-up Table (person-time intervals): ")
      print(kable(followup.nonunique[[i]]))
    }
  }
    
  if (params@end_of_fup) {
    eof.unique <- slot(object, "info")$eof.unique
    eof.nonunique <- slot(object, "info")$eof.nonunique
    for (i in seq_along(eof.nonunique)) {
      if (!is.na(params@subgroup)) cat("For subgroup: ", names(eof.nonunique)[[i]], "\n")
      cat("\nEnd-of-Follow-up Table (trial-periods; categories sum to Eligible): ")
      print(kable(eof.nonunique[[i]]))
      cat("\nEnd-of-Follow-up Table (distinct subjects; categories may overlap): ")
      print(kable(eof.unique[[i]]))
    }
  }

  if (slot(params, "method") == "censoring") {
    cat("\nUnique Switch Table: ")
    print(kable(slot(object, "info")$switch.unique))
    cat("\nNon-Unique Switch Table: ")
    print(kable(slot(object, "info")$switch.nonunique))
  }
  
  if (!is.na(slot(params, "compevent"))) {
    compevent.unique <- slot(object, "info")$compevent.unique
    compevent.nonunique <- slot(object, "info")$compevent.nonunique
    for (i in seq_along(compevent.unique)) {
      if (!is.na(params@subgroup)) cat("For subgroup: ", names(compevent.unique)[[i]], "\n")
      cat("\nUnique Competing Event Table: ")
      print(kable(compevent.unique[[i]]))
      cat("\nNon-Unique Competing Event Table: ")
      print(kable(compevent.nonunique[[i]]))
    }
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
  arm <- function(i) lapply(object@weight.statistics, function(x) {
    coefs <- x[[1]]$coef.numerator
    if (length(coefs) >= i) coefs[[i]] else NULL
  })
  return(list(numerator0 = arm(1L), numerator1 = arm(2L)))
}

#' Retrieves Denominator Models from SEQuential object
#' @param object object of class SEQoutput
#'
#' @returns List of both denominator models
#' @importFrom methods is slot
#'
#' @export
denominator <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@weighted) stop("SEQuential process was not weighted")
  arm <- function(i) lapply(object@weight.statistics, function(x) {
    coefs <- x[[1]]$coef.denominator
    if (length(coefs) >= i) coefs[[i]] else NULL
  })
  return(list(denominator0 = arm(1L), denominator1 = arm(2L)))
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
#' @returns List of SEQuential covariates
#' @importFrom methods is slot
#'
#' @export
covariates <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  format_formula <- function(x) {
    if (all(is.na(x))) return(NA_character_)
    gsub("~", " ~ ", gsub("\\+", " + ", x))
  }
  return(list(Outcome = format_formula(object@outcome),
              Numerator = format_formula(object@numerator),
              Denominator = format_formula(object@denominator)))
}

#' Extract the end-of-follow-up outcome estimates
#'
#' Available when [SEQuential()] was run with \code{end_of_fup = TRUE}. The
#' estimate in each arm is the weighted average of the outcome measured at
#' \code{end_of_fup.time}, weighted by the period-trial-specific weight at the
#' time the measurement was taken.
#'
#' @param object SEQoutput object
#'
#' @returns A named list, one element per subgroup, each a list of two
#'   data.tables:
#'   \itemize{
#'     \item \code{estimates}: the weighted proportion (binary) or mean
#'       (continuous) in each baseline treatment arm, with its bootstrap
#'       confidence interval, and the number of contributing trial-periods and
#'       distinct subjects.
#'     \item \code{comparison}: the pairwise between-arm difference - in
#'       proportions for a binary outcome, in means for a continuous one - with
#'       its bootstrap standard error and confidence interval, paired by
#'       iteration so the interval accounts for the correlation between arms.
#'       For a binary outcome the ratio of proportions is also given, with a
#'       confidence interval computed on the log scale and a \code{log(Ratio) SE}
#'       for inverse-variance pooling. A ratio is not reported for a continuous
#'       outcome, where the outcome need not be bounded away from zero.
#'   }
#' @importFrom methods is slot
#' @export
end_of_fup <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@end_of_fup) stop("End-of-follow-up estimates were not created as a result of 'end_of_fup = FALSE'")
  data <- slot(object, "eof.data")
  comparison <- slot(object, "eof.comparison")
  out <- lapply(seq_along(data), function(i) list(estimates = data[[i]], comparison = comparison[[i]]))
  names(out) <- names(data)
  return(out)
}

#' Function to print Kaplan-Meier curves
#'
#' @param object SEQoutput object to plot
#' @param plot.type character: type of plot to print; one of: `"survival"` (default), `"risk"`, `"inc"`
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
  if (!missing(plot.subtitle)) slot(params, "plot.subtitle") <- plot.subtitle
  if (!missing(plot.labels)) slot(params, "plot.labels") <- plot.labels
  
  groups <- if(!is.na(object@params@subgroup)) names(object@survival.data) else 1L
  out <- vector("list", length(groups))
  if (length(groups) > 0) names(out) <- groups
  for (i in seq_along(object@survival.data)) {
    label <- groups[[i]]
    out[[label]] <- internal.plot(object@survival.data[[i]], params)
  }
  if (is.na(object@params@subgroup)) return(out[[1]])
  return(out)
}

#' Function to return survival data from a SEQuential object
#'
#' @param object SEQoutput object
#'
#' @importFrom methods is slot
#' @returns A data frame of survival values, or a named list of data frames when subgroups are specified
#' @export
km_data <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (is.na(object@params@subgroup)) return(slot(object, "survival.data")[[1]])
  return(slot(object, "survival.data"))
}

#' Function to return competing event models from a SEQuential object
#' 
#' @param object SEQoutput object
#' 
#' @importFrom methods is slot
#' @returns A fastglm object, or a named list of fastglm objects when subgroups are specified
#' @export
compevent <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (is.na(object@params@compevent)) stop("No competing event was specified during SEQuential process")
  if (is.na(object@params@subgroup)) return(slot(object, "ce.model")[[1]])
  return(slot(object, "ce.model"))
}

#' Function to return risk information from a SEQuential object
#' 
#' @param object SEQoutput object
#' @importFrom methods is slot
#' @returns A data table of risk information at the end of followup
#' @export
risk_data <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@km.curves) stop("Survival Data and Risks were not created through `km.curves = TRUE` in SEQuential process")
  if (is.na(object@params@subgroup)) return(slot(object, "risk.data")[[1]])
  return(slot(object, "risk.data"))
}

#' Function to return risk information from a SEQuential object
#'
#' @param object SEQoutput object
#' @importFrom methods is slot
#' @returns A data frame of risk comparison information at the reported follow-up time(s):
#'   risk ratios and risk differences, and when bootstrapped their confidence
#'   intervals and standard errors. The bootstrap standard errors are reported
#'   regardless of `bootstrap.CI_method`: `RD SE` is the standard error of the
#'   risk difference (natural scale) and `log(RR) SE` is the standard error of
#'   the log risk ratio. For an inverse-variance-weighted meta-analysis across
#'   samples, pool `Risk Difference` with `RD SE`, and `log(`Risk Ratio`)` with
#'   `log(RR) SE` (then exponentiate the pooled ratio).
#' @export
risk_comparison <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@km.curves) stop("Survival Data and Risks were not created through `km.curves = TRUE` in SEQuential process")
  if (is.na(object@params@subgroup)) return(slot(object, "risk.comparison")[[1]])
  return(slot(object, "risk.comparison"))
}

#' Function to return hazard ratios from a SEQuential object
#' 
#' @param object SEQoutput object
#' @importFrom methods is slot
#' @returns A named vector of hazard ratios, or a named list of vectors when subgroups are specified
#' @export
hazard_ratio <- function(object) {
  if (!is(object, "SEQoutput")) stop("Object is not of class SEQoutput")
  if (!object@params@hazard) stop("Hazard Ratios were not created through `hazard = TRUE` in SEQuential process")
  if (is.na(object@params@subgroup)) return(slot(object, "hazard")[[1]])
  return(slot(object, "hazard"))
}

#' Function to return diagnostic tables from a SEQuential object
#'
#' @param object SEQoutput object
#'
#' @importFrom methods is slot
#' @returns A named list of diagnostic tables, each broken down by baseline treatment arm.
#'   The "unique" and "non-unique" variants count different things:
#'   \itemize{
#'     \item \code{outcome.unique} / \code{outcome.nonunique}: distinct subjects who had the
#'       outcome vs. the total number of outcome events. These coincide for a one-time
#'       (terminal) outcome, since each subject contributes at most one event row. Both are
#'       \code{NA} for a continuous end-of-follow-up outcome, which has no events to count.
#'     \item \code{eof.unique} / \code{eof.nonunique}: present only when
#'       \code{end_of_fup = TRUE}, accounting for every trial-period at the
#'       end-of-follow-up time across four mutually exclusive categories -
#'       measured \code{At k}, measured \code{In window}, \code{Excluded
#'       (outside window)} and \code{Excluded (no measurement)} - against the
#'       \code{Eligible} total. The non-unique (trial-period) counts partition
#'       \code{Eligible}, and \code{At k} plus \code{In window} equals the
#'       trial-periods contributing to the estimate. The unique (subject) counts
#'       need not sum to \code{Eligible}, since one subject can fall into
#'       different categories for different trials.
#'     \item \code{followup.unique} / \code{followup.nonunique}: distinct subjects contributing
#'       follow-up vs. the total number of person-time intervals (expanded rows). The
#'       non-unique count is much larger because each subject contributes one row per
#'       follow-up period; it is the denominator that, with \code{outcome.nonunique}, gives
#'       the per-arm event rate.
#'   }
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
