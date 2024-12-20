#' Internal function for fitting outcome models
#'
#' @importFrom fastglm fastglm
#' @importFrom stats as.formula model.matrix
#' @importFrom splines ns
#'
#' @keywords internal
internal.model <- function(data, params) {
  data <- data[!is.na(get(params@outcome)), ]

  if (params@followup.class) data <- data[, "followup" := as.factor(get("followup"))]
  if (params@followup.spline) data <- data[, "followup" := splines::ns(get("followup"))]

  X <- model.matrix(as.formula(paste0(params@outcome, "~", params@covariates)), data)
  y <- data[[params@outcome]]

  if(!params@weighted) {
      model <- fastglm::fastglm(X, y, family = quasibinomial(), method = params@fastglm.method)
      weight <- NULL
    } else {
      weight <- data[weight < params@weight.lower, weight := params@weight.lower
                     ][weight > params@weight.upper, weight := params@weight.upper][['weight']]
      model <- fastglm::fastglm(X, y, family = quasibinomial(), weights = weight, method = params@fastglm.method)
    }
  if (params@calculate.var) vcov <- fastglm.robust(model, X, y, weight) else vcov <- NA
  return(list(model = model,
              vcov = vcov))
}
