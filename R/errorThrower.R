#' Internal Error Thrower - when identical names are passed to \code{...} and \code{params} and their values differ
#' @keywords internal

errorParams <- function(params, dots){
  for (name in intersect(names(params), names(dots))){
    if(!identical(params[[name]], dots[[name]]))
      stop(sprintf("Conflicting values for parameter '%s' in params and ..."))
  }
}

#' Internal Error Thrower - when \code{data}, \code{id}, \code{time}, \code{treatment} are missing.
#' Or, in the case of the latter three, if they are not found in \code{names(data)}
#' @keywords internal
errorData <- function(data, id, time, treatment){
  if(missing(data)) stop("Data is missing")
  if(missing(id)) stop("ID column name is missing") else if(!id %in% names(data)) stop(sprintf("The 'id' argument '%s' is not found in the data column names"))
  if(missing(time)) stop("Time column name is missing")  else if(!time %in% names(data)) stop(sprintf("The 'time' argument '%s' is not found in the data column names"))
  if(missing(treatment)) stop("Treatment column name is missing")  else if(!treatment %in% names(data)) stop(sprintf("The 'treatment' argument '%s' is not found in the data column names"))
}
