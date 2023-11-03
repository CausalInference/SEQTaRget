#' Internal function for fitting ITT model on in-memory data
#'
#' @importFrom speedglm speedglm
#'
#' @keywords internal
internal.model <- function(data, method, formula, opts){
  data <- as.data.frame(data)
  if(method == "ITT"){
    model <- speedglm::speedglm(formula,
                                data,
                                family = binomial("logit"))
    return(model)
  }
}

create.numerator <- function(DT, opts){
  if(opts$weighted == 0){
    DT[, numerator := 1]
  }
}
