create.risk <- function(data) {
  variable <- NULL
  table <- data[, .SD[.N], by = "variable"
                ][variable %like% "risk", ]
  rd <- round(as.numeric(table[2, 3] - table[1, 3]), 4)
  rr <- round(as.numeric(table[2, 3] / table[1, 3]), 4)

  return(list(
    rd = rd,
    rr = rr
  ))
}

#' function loading ncores in global environment
#' @param pos defaults to 1 which equals an assignment to global environment
#' @param ncores ncores to assign to global
#'
#' @keywords internal
assign.global <- function(ncores, pos = 1) {
  assign("ncores", ncores, envir = as.environment(pos))
}

#' Passes data to correct model
#'
#' @importFrom fastglm fastglm
#' @keywords internal
model.passer <- function(X, y, params) {
  if (params@multinomial) model <- multinomial(X, y, method = params@fastglm.method) else model <- fastglm(X, y, family = quasibinomial(), method = params@fastglm.method)

  return(model)
}

#' Passes model to correct prediction method
#'
#' @keywords internal
prediction.passer <- function(model, X, params, type) {
  if (params@multinomial) pred <- multinomial.predict(model, X) else pred <- inline.pred(model, X, params, type)

  return(pred)
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
