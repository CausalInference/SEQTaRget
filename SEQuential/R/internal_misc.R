create.risk <- function(data) {
  table <- data[, .SD[.N], by = "variable"]
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
  if (params@multinomial) model <- multinomial(X, y) else model <- fastglm(X, y, family = quasibinomial(), method = params@fastglm.method)

  return(model)
}

#' Passes model to correct prediction method
#'
#' @keywords internal
prediction.passer <- function(model, X, params, type) {
  if (params@multinomial) pred <- multinomial.predict(model, X) else pred <- inline.pred(model, X, params, type)

  return(pred)
}
