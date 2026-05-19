#' Internal function for fitting outcome models
#'
#' @importFrom stats as.formula model.matrix
#' @importFrom splines ns
#' @import data.table
#'
#' @keywords internal
internal.model <- function(data, params, start = NULL) {
  data <- data[!is.na(get(params@outcome)), ]

  if (params@followup.class) {
    fup_levels <- 0L:max(params@DT$followup, na.rm = TRUE)
    data <- data[, "followup" := factor(get("followup"), levels = fup_levels)]
  }

  handler <- function(data, params, start = NULL) {
    X <- model.matrix(as.formula(paste0("~", params@covariates)), data)
    y <- data[[params@outcome]]

    if(!params@weighted) {
        model <- fit_glm(X, y, family = quasibinomial(), params = params, start = start)
        weight <- NULL
      } else {
        weight <- data[weight < params@weight.lower, weight := params@weight.lower
                       ][weight > params@weight.upper, weight := params@weight.upper][['weight']]
        model <- fit_glm(X, y, family = quasibinomial(), weights = weight, params = params, start = start)
      }
    return(list(model = model))
  }

  if (is.na(params@subgroup)) {
    model <- list(handler(data, params, start = if (is.list(start)) start[[1L]] else start))
  } else {
    model <- list()
    subgroups <- sort(unique(data[[params@subgroup]]))
    for (i in seq_along(subgroups)) {
      label <- paste0(params@subgroup, "_", subgroups[[i]])
      subDT <- data[get(params@subgroup) == subgroups[[i]], ][, eval(params@subgroup) := NULL]
      model[[label]] <- handler(subDT, params, start = if (is.list(start)) start[[label]] else start)
    }
  }

  return(model)
}
