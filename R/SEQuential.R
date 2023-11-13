#' SEQuential processing per (paper here?)
#'
#' @param data data.frame or data.table, if not already expanded with \code{SEQexpand}, will preform expansion according to arguments passed to either \code{params} or \code{...}
#' @param id.col String: column name of the id column
#' @param time.col String: column name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param treatment.col String: column name of the treatment column
#' @param outcome.col String: column name of the outcome column
#' @param method String: method of analysis to preform
#' @param params List: optional list of parameters from \code{SEQOpts}
#' @param ... another option for passing parameters from \code{SEQOpts}
#'
#' @import data.table
#'
#' @export
SEQuential <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, method, params, ...){
  # Error Throwing ============================================
  errorData(data, id.col, time.col, eligible.col, treatment.col, outcome.col, method)

  # Parameter Space building ==================================
  opts <- SEQopts(); dots <- list(...)
  if(!missing(params)) errorParams(params, dots)
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots
  errorOpts(opts)

  if(is.na(opts$covariates)){
    formula <- create.default.formula(DT, id.col, time.col, eligible.col, treatment.col, outcome.col, method,)
  } else formula <- create.formula(outcome.col, opts$covariates)

  # Expansion ==================================================
  if(opts$expand == TRUE){
    cat("Expanding Data...\n")
    DT <- SEQexpand(data, id.col, time.col, eligible.col, outcome.col, params = opts)

    if(method == "none"){
      cat("Returning expanded data per 'method = none'")
      return(DT)
    }

    cat(paste("Expansion Successful\nMoving forward with", method, "analysis"))
  } else if(opts$expand == FALSE){
    cat("Skipping expansion per 'expand = FALSE'\n")
    cat(paste("Moving forward with", method, "analysis\n"))
    DT <- as.data.table(data)
  }

  #Model Dispersion ===========================================
  if(opts$weighted == FALSE){
    model <- internal.model(DT, method, formula, opts)
  } else if (opts$weighted == TRUE){
    if(opts$stabilized == TRUE && opts$weight.time == "pre"){
      if(opts$weight.time == "pre"){
        weightModel <- 'MODEL USING WEIGHTS FIT PRE-EXPANSION'
      }
    } else if(opts$stabilized == FALSE || weight.time == "post"){
      weightModel <- 'MODEL USING WEIGHTS FIT POST-EXPANSION'
    }
  }
  cat(paste0("\n", method, " model successfully created\nCreating survival curves"))

  surv <- internal.survival(DT, id.col, time.col, outcome.col, treatment.col, opts = opts)

  return(list(model, surv))
}
