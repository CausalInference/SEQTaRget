#' Internal function for fitting outcome models
#'
#' @importFrom fastglm fastglm
#' @importFrom stats as.formula model.matrix
#'
#' @keywords internal
internal.model <- function(data, params) {
  data <- data[!is.na(get(params@outcome)), ]
  X <- model.matrix(as.formula(paste0(params@outcome, "~", params@covariates)), data)
  y <- data[[params@outcome]]

  if(!params@weighted) {
    if (!params@multinomial){
      model <- fastglm::fastglm(X, y, family = quasibinomial(), method = params@fastglm.method)
    } else {
      model <- multinomial(X, y, family = quasibinomial(), method = params@fastglm.method)
    }
  } else {
      weight <- data[['weight']]
      model <- fastglm::fastglm(X, y, family = quasibinomial(), weights = weight, method = params@fastglm.method)
    }
  return(model)
}
