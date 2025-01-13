#' Internal function to pull Risk Ratio and Risk Difference from data when \code{km.curves = TRUE}
#'
#' @keywords internal
create.risk <- function(data) {
  variable <- NULL
  table <- data[, .SD[.N], by = "variable"
                ][variable %like% "risk", ]
  rd <- round(as.numeric(table[2, 3] - table[1, 3]), 4)
  rr <- round(as.numeric(table[2, 3] / table[1, 3]), 4)
  
  if ("lb" %in% names(table) & "ub" %in% names(table)) {
    rd.ub <- round(as.numeric(table[2, 4] - table[1, 4]), 4)
    rd.lb <- round(as.numeric(table[2, 5] - table[1, 5]), 4)
    
    rr.ub <- round(as.numeric(table[2, 4] / table[1, 4]), 4)
    rr.lb <- round(as.numeric(table[2, 5] / table[1, 5]), 4)
  } else rd.lb <- rd.ub <- rr.lb <- rr.ub <- NA_real_

  return(list(
    difference = c(difference = rd, LCI = rd.lb, UCI = rd.ub),
    ratio = c(ratio = rr, LCI = rr.lb, UCI = rr.ub)
  ))
}

#' Internal function loading ncores in global environment
#' @param pos defaults to 1 which equals an assignment to global environment
#' @param ncores ncores to assign to global
#'
#' @keywords internal
assign.global <- function(ncores, pos = 1) {
  assign("ncores", ncores, envir = as.environment(pos))
}

#' Nicely cleans time for readability
#'
#' @keywords internal
format.time <- function(seconds) {
  if (seconds < 60) {
    paste0(round(seconds, 2), " seconds")
  } else if (seconds < 3600) {
    minutes <- floor(seconds / 60)
    remaining_seconds <- seconds %% 60
    paste0(minutes, " minute", ifelse(minutes > 1, "s", ""),
           " ", round(remaining_seconds, 2), " second", ifelse(remaining_seconds > 1, "s", ""))
  } else {
    hours <- floor(seconds / 3600)
    remaining_seconds <- seconds %% 3600
    minutes <- floor(remaining_seconds / 60)
    seconds <- remaining_seconds %% 60
    paste0(hours, " hour", ifelse(hours > 1, "s", ""),
           " ", minutes, " minute", ifelse(minutes > 1, "s", ""),
           " ", round(seconds, 2), " second", ifelse(seconds > 1, "s", ""))
  }
}
