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
errorData <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time.cols, fixed.cols, method){
  if(missing(data)) stop("Data is missing")

  requested.cols <- unique(c(id.col, time.col, eligible.col, treatment.col, outcome.col, time.cols, fixed.cols))
  for(i in seq_along(requested.cols)){
    if(!requested.cols[[i]] %in% names(data)) stop(paste0(requested.cols[[i]], "not found in provided data"))
      }

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

errorOpts <- function(opts){
}
