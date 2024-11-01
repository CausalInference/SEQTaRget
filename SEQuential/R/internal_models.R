#' Internal function for fitting outcome models
#'
#' @importFrom fastglm fastglm
#' @importFrom stats as.formula model.matrix
#' @importFrom speedglm speedglm
#'
#' @keywords internal
internal.model <- function(data, params) {
  obj <- paste0(params@outcome, "~", params@covariates)
  if (params@glm.fitter == "speedglm") {
    if (!params@weighted) {
      model <- speedglm::speedglm(obj, data = data, family = quasibinomial())
    } else {
      model <- speedglm::speedglm(obj, data = data, weights = data$weight, family = quasibinomial())
    }
  } else {
    data <- data[!is.na(get(params@outcome)), ]
    X <- model.matrix(as.formula(paste0(params@outcome, "~", params@covariates)), data)
    y <- data[[params@outcome]]

    if(!params@weighted) {
      model <- fastglm::fastglm(X, y, family = quasibinomial(), method = params@fastglm.method)
    } else {
      weight <- data[['weight']]
      model <- fastglm::fastglm(X, y, family = quasibinomial(), weights = weight, method = params@fastglm.method)
    }
  }
  return(model)
}
