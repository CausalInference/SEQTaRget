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
#' @importFrom survival survfit Surv
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

  # Coercion ==================================================
  if(is.null(attr(data, "SEQexpanded"))){
    cat("Expanding Data...\nIf expansion has already occurred, please set 'expand = FALSE'\n")
    DT <- SEQexpand(data, id.col, time.col, eligible.col, outcome.col, params = opts)
    cat(paste("Expansion Successful\nMoving forward with", method, "analysis"))

  } else if(attr(data, "SEQexpanded") == TRUE || opts$expand == FALSE){
    if(attr(data, "SEQexpanded") == TRUE) cat("Previous expansion detected\n")
    else cat("Skipping expansion per 'expand = FALSE'\n")
    cat(paste("Moving forward with", method, "analysis"))
    DT <- as.data.table(data)
  }

  if(is.na(opts$covariates)){
    formula <- create.default.formula(DT, outcome.col, id.col, eligible.col)
    } else formula <- create.formula(outcome.col, opts$covariates)
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
#  surv <- internal.survival(DT, time.col, outcome.col, treatment.col, opts)
  return(model)
}
