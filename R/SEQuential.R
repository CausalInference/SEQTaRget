#'
#'
#'
#'
#' @import data.table
#'
#' @export
SEQuential <- function(data, id, time, treatment, params, ...){
  # Coercion ==================================================
  data <- as.data.table(data)
  opts <- buildParam(); dots <- list(...)

  # Error Throwing ============================================
  if(!missing(params)) errorParams(params, dots)
  errorData(data, id, time, treatment)

  # Parameter Space building ==================================
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots



}
