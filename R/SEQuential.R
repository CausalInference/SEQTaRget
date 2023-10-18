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
  # Parameter Space building ==================================
  opts <- buildParam(); dots <- list(...)
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots

  # Error Throwing ============================================
  if(!missing(params)) errorParams(params, dots)
  errorData(data, id, time, treatment, eligible)
  errorParallelSpark(opts)

  # Coercion ==================================================
  if(tryCatch(attr(data, "SEQexpanded"), error = function(e) NULL) == TRUE){
    data <- as.data.table(data)
    data <- SEQexpand(data, id, time, eligible, params = opts)
  }

  if(is.na(opts$covariates)){
    formula <- create.default.formula()}
  else {formula <- create.formula(outcome.col, opts$covariates)}

  #Model Dispersion ===========================================
  if(opts$weighted == FALSE){
    if(method == "ITT") model <- itt_model()
  }

}
