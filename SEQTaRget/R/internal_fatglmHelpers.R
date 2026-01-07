#' Helper Function to inline predict a fastglm object
#' @param model a fastglm object
#' @param newdata filler for a .SD from data.table
#' @param params parameter from SEQuential
#' @param type type of prediction
#'
#' @keywords internal

inline.pred <- function(model, newdata, params, type, case = "default", multi = FALSE, target = NULL) {
  covs <- switch(
    case,
    "default" = switch(
      type,
      "numerator" = params@numerator,
      "denominator" = params@denominator,
      "outcome" = params@covariates
    ),
    "LTFU" = switch(
      type,
      "numerator" = params@cense.numerator,
      "denominator" = params@cense.denominator
    ),
    "visit" = switch(
      type,
      "numerator" = params@visit.numerator,
      "denominator" = params@visit.denominator
    ),
    "surv" = params@covariates
  )
  cols <- unique(unlist(strsplit(covs, "\\*|\\+")))
  X <- model.matrix(as.formula(paste0("~", covs)),
                    data = newdata[, cols, with = FALSE])
  
  pred <- if (!multi) predict(model, X, "response") else multinomial.predict(model, X, target)
  return(pred)
}

#' Helper function to prepare data for fastglm
#' @param weight data after undergoing preparation
#' @param params parameter from SEQuential
#' @param type type of model, e.g. d0 = "denominator"
#' @param model model number, e.g. d0 = "zero model"
#'
#' @keywords internal
prepare.data <- function(weight, params, type, model, case) {
  cols <- covs <- y <- X <- isExcused <- followup <- tx_lag <- NULL
  target <- match(model, unlist(params@treat.level))
  
  if (case == "default") {
    if (type %in% c("numerator", "denominator")) {
      cols <- unlist(strsplit(ifelse(type == "numerator", params@numerator, params@denominator), "\\+|\\*"))
      covs <- ifelse(type == "numerator", params@numerator, params@denominator)
      
      if (params@weight.lag_condition) {
        weight <- if (type == "numerator" && params@excused) weight[get(paste0(params@treatment, params@indicator.baseline)) == model, ] else weight[tx_lag == model, ]
      } 
      if (type == "denominator" && !params@weight.preexpansion) weight <- weight[followup != 0, ]
      if (params@excused) {
        if (!is.na(params@excused.cols[[target]])) weight <- weight[get(params@excused.cols[[target]]) == 0, ]
      }
      
      y <- if (!params@weight.preexpansion && (params@excused || params@deviation.excused)) weight[["censored"]] else weight[[params@treatment]]
      X <- model.matrix(as.formula(paste0("~", covs)), weight[, cols, with = FALSE])
    }
    
  } else if (case == "LTFU") {
    weight <- weight[!is.na(get(params@cense))]
    cols <- unlist(strsplit(ifelse(type == "numerator", params@cense.numerator, params@cense.denominator), "\\+|\\*"))
    covs <- ifelse(type == "numerator", params@cense.numerator, params@cense.denominator)
    
    weight[, paste0(params@time, params@indicator.squared) := get(params@time)^2]
    y <- abs(weight[[params@cense]] - 1)
    X <- model.matrix(as.formula(paste0("~", covs)), weight[, cols, with = FALSE])
    
  } else if (case == "visit") {
    cols <- unlist(strsplit(ifelse(type == "numerator", params@visit.numerator, params@visit.denominator), "\\+|\\*"))
    covs <- ifelse(type == "numerator", params@visit.numerator, params@visit.denominator)
    
    weight[, paste0(params@time, params@indicator.squared) := get(params@time)^2]
    y <- weight[[params@visit]]
    X <- model.matrix(as.formula(paste0("~", covs)), weight[, cols, with = FALSE])
    
  } else if (case == "surv") {
    cols <- unlist(strsplit(params@covariates, "\\+|\\*"))
    covs <- params@covariates
    
    y <- if (type == "compevent") weight[[params@compevent]] else weight[[params@outcome]]
    X <- model.matrix(as.formula(paste0("~", covs)), weight[!is.na(get(params@outcome))][, cols, with = FALSE])
    
  }
  return(list(y = y, X = X))
}

#' Function to clean out non needed elements from fastglm return
#' @param model a fastglm model
#' @keywords internal
clean_fastglm <- function(model) {
  model$x <- NULL
  model$y <- NULL
  model$model <- NULL
  return(model)
}
