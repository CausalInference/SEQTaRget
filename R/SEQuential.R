#' SEQuential processing per (paper here?)
#'
#' @param data data.frame or data.table, if not already expanded with \code{SEQexpand}, will preform expansion according to arguments passed to either \code{params} or \code{...}
#' @param id.col String: column name of the id column
#' @param time.col String: column name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param treatment.col String: column name of the treatment column
#' @param outcome.col String: column name of the outcome column
#' @param causal_contrast String: causal_contrast to preform
#' @param params List: optional list of parameters from \code{SEQOpts}
#' @param ... another option for passing parameters from \code{SEQOpts}
#'
#' @import data.table
#'
#' @export
SEQuential <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, causal_contrast, params, ...){
  # Error Throwing ============================================
  errorData(data, id.col, time.col, eligible.col, treatment.col, outcome.col, causal_contrast)

  # Parameter Space building ==================================
  opts <- SEQopts(); dots <- list(...)
  if(!missing(params)) errorParams(params, dots)
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots
  errorOpts(opts)

  if(is.na(opts$covariates)){
    opts$covariates <- create.default.covariates(data, id.col, time.col, eligible.col, treatment.col, outcome.col, causal_contrast, opts)
  }

  # Expansion ==================================================
  if(opts$expand){
    cat("Expanding Data...\n")
    DT <- SEQexpand(data, id.col, time.col, eligible.col, outcome.col, opts)

    if(causal_contrast == "none"){
      cat("Returning expanded data per 'causal_contrast = 'none''")
      return(DT)
    }

    cat(paste("Expansion Successful\nMoving forward with", causal_contrast, "analysis"))
  } else if(!opts$expand){
    cat("Skipping expansion per 'expand = FALSE'\n")
    cat(paste("Moving forward with", causal_contrast, "analysis\n"))
    DT <- as.data.table(data)
  }


  #Model Dispersion ===========================================
  if(!opts$weighted){
    model <- internal.model(DT, causal_contrast, outcome.col, opts)
  } else if (opts$weighted){
    cat("Weighting...")
    WT <- internal.weights(DT, data, id.col, time.col, outcome.col, treatment.col, opts)
    cat(paste0(ifelse(opts$stabilized, "Stabilized ", "Non-Stabilized "),
               ifelse(opts$pre_expansion, "pre-expansion ", "post-expansion "),
        "weight creation successful\nMoving to Modeling..."))
  }
  cat(paste0("\n", causal_contrast, " model successfully created\nCreating survival curves"))
  surv <- internal.survival(DT, id.col, time.col, outcome.col, treatment.col, opts)

  return(list(model, surv))
}
