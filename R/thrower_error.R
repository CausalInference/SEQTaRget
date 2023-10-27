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
errorData <- function(data, id.col, time.col, eligible.col, outcome.col, method){
  if(missing(data)) stop("Data is missing")
  if(missing(id.col)) stop("ID column name is missing") else if(!id.col %in% names(data)) stop(sprintf("The 'id' argument '%s' is not found in the data column names"))
  if(missing(time.col)) stop("Time column name is missing")  else if(!time.col %in% names(data)) stop(sprintf("The 'time' argument '%s' is not found in the data column names"))
  if(missing(outcome.col)) stop("Outcome column name is missing")  else if(!outcome.col %in% names(data)) stop(sprintf("The 'outomce' argument '%s' is not found in the data column names"))
  if(missing(eligible.col)) stop("Eligible column name is missing")  else if(!eligible.col %in% names(data)) stop(sprintf("The 'eligible' argument '%s' is not found in the data column names"))
  if(missing(method)) stop("Analysis method must be specified")
  }

#' Internal Error Thrower - when the expected bytes used for data expansion exceed given memory
#' @keywords internal
errorMemory <- function(data, id.col, eligible.col, time.col, opts){
  bytes <- translate_memory(memory)

  dt <- data[, .(
    nrow_eligible = sum(get(eligible.col) == 1),
    max_time = pmin(max(get(time.col)), max.time),
    ncol = ncol(data)
  ), by = id.col]


  dt[, dim_id := nrow_eligible*max_time*ncol]
  exp_memory <- sum(dt$dim_id)*8

  if(exp_memory > bytes){
    stop(paste("Expected result exceeds memory of", memory))
  }
}

errorOpts <- function(){
  if(opts$stabilized == FALSE && opts$weight.time == "pre") stop("Non-stabilized weights can only be fit post-expansion")
}
