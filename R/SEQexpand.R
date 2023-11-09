#' Creates an expanded dataset for use with \code{SEQuential}
#'
#' @param data Dataframe or DataTable: data to expand
#' @param id.col String: column name of the id column
#' @param time.col String: colum name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param params List: optional list of parameters from \code{SEQOpts}
#' @param ... Other parameters, as passed to \code{SEQOpts.expansion}
#'
#' @import data.table
#'
#' @export

SEQexpand <- function(data, id.col, time.col, eligible.col, outcome.col, params, ...) {
  # Coercion ==================================================
  DT <- expansion.preprocess(as.data.table(data), id.col, eligible.col)
  opts <- SEQopts(); dots <- list(...)

  #Parameter Space ============================================
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots

  result <- internal.expansion(DT, id.col, time.col, eligible.col, outcome.col, opts)
  return(result)
}
