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
  }
  return(model)
}

internal.survival <- function(data, time.col, outcome.col, treatment.col, opts){
  if(opts$expand == TRUE) time.col <- "period"
  set(data, j = time.col, value = as.numeric(data[[time.col]]))
  data <- as.data.frame(data)

  survformula <- as.formula(paste0("Surv(", time.col,",", outcome.col, ")~", treatment.col))
  surv <- survival::survfit(survformula, data)
  return(surv)
}

create.numerator <- function(DT, opts){
  if(opts$weighted == 0){
    DT[, numerator := 1]
  }
}
