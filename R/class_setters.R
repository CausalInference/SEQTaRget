parameter.setter <- function(data, DT,
                             id.col, time.col, eligible.col, outcome.col,
                             time_varying.cols, fixed.cols, method,
                             opts){
  new("SEQparams",
      data = data,
      DT = DT,
      id = id.col,
      time = time.col,
      eligible = eligible.col,
      outcome = outcome.col,
      time_varying = time_varying.cols,
      fixed = fixed.cols,
      method = method,
      opts = opts)
}
