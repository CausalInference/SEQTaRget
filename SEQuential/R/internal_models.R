#' Internal function for fitting outcome models
#'
#' @importFrom fastglm fastglm
#' @importFrom stats as.formula model.matrix
#'
#' @keywords internal
internal.model <- function(data, params) {
  data <- data[!is.na(get(params@outcome)), ]

  if (params@multinomial) data <- data[get(paste0(params@treatment, params@baseline.indicator)) %in% params@treat.level]
  X <- model.matrix(as.formula(paste0(params@outcome, "~", params@covariates)), data)
  y <- data[[params@outcome]]

  if(!params@weighted) {
      model <- fastglm::fastglm(X, y, family = quasibinomial(), method = params@fastglm.method)
    } else {
      weight <- data[weight < params@lower.weight, weight := params@lower.weight
                     ][weight > params@upper.weight, weight := params@upper.weight][['weight']]
      model <- fastglm::fastglm(X, y, family = quasibinomial(), weights = weight, method = params@fastglm.method)
    }
  return(model)
}
