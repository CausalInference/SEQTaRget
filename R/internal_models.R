#' Internal function for fitting ITT model on in-memory data
#'
#' @importFrom speedglm speedglm
#'
#' @keywords internal
itt.model <- function(formula, data){
  data <- as.data.frame(data)
  model <- speedglm::speedglm(formula,
                              data,
                              family = binomial("logit"))
  return(model)
}

create.numerator <- function(DT, opts){
  if(opts$weighted == 0){
    DT[, numerator := 1]
  }
}
