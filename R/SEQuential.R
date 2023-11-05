#' SEQuential processing per (paper here?)
#'
#' @param data data.frame or data.table, if not already expanded with \code{SEQexpand}, will preform expansion according to arguments passed to either \code{params} or \code{...}
#' @param id.col String: column name of the id column
#' @param time.col String: colum name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param method String: method of analysis to preform
#' @param params List: optional list of parameters from \code{SEQOpts}
#' @param ... another option for passing parameters from \code{SEQOpts}
#'
#' @import data.table
#'
#' @export
SEQuential <- function(data, id.col, time.col, eligible.col, outcome.col, method, params, ...){
  # Error Throwing ============================================
  if(!missing(params)) errorParams(params, dots)
  errorData(data, id.col, time.col, eligible.col, outcome.col, method)

  # Parameter Space building ==================================
  opts <- SEQopts(); dots <- list(...)
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots
  errorOpts(opts)

  if(is.na(opts$covariates)) formula <- create.default.formula(data)
  else formula <- create.formula(outcome.col, opts$covariates)

  # Coercion ==================================================
  if(is.null(attr(data, "SEQexpanded"))){
    cat("Expanding Data...\nIf expansion has already occurred, please set 'expanded = TRUE'")
    DT <- SEQexpand(data, id.col, time.col, eligible.col, params = opts)
    cat(paste("Expansion Successful\nMoving forward with", method, "analysis"))

  } else if(attr(data, "SEQexpanded") == TRUE || opts$expand == FALSE){
    if(attr(data, "SEQexpanded") == TRUE) cat("Previous expansion detected")
    else cat("Skipping expansion per 'expand = FALSE'")
    cat(paste("Moving forward with", method, "analysis"))
    DT <- as.data.table(data)
  }

  #Model Dispersion ===========================================
  if(opts$weighted == FALSE){
    model <- internal.model(DT, formula, method, opts)
  } else if (opts$weighted == TRUE){
    if(opts$stabilized == TRUE && opts$weight.time == "pre"){
      if(opts$weight.time == "pre"){
        weightModel <- 'MODEL USING WEIGHTS FIT PRE-EXPANSION'
      }
    } else if(opts$stabilized == FALSE || weight.time == "post"){
      weightModel <- 'MODEL USING WEIGHTS FIT POST-EXPANSION'
    }
  }
}
