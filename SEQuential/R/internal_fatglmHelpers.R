#' Helper Function to inline predict a fastglm object
#' @param model a fastglm object
#' @param newdata filler for a .SD from data.table
#' @param params parameter from SEQuential
#' @param type type of prediction
#'
#' @keywords internal

inline.pred <- function(model, newdata, params, type, case = "default"){
  if (case == "default") {
    if(type == "numerator") {
      cols <- unlist(strsplit(params@numerator, "\\+"))
      covs <- params@numerator
    }
    if(type == "denominator") {
      cols <- unlist(strsplit(params@denominator, "\\+"))
      covs <- params@denominator
    }
  }
  if(case == "LTFU") {
    if (type == "numerator") {
      cols <- unlist(strsplit(params@cense.numerator, "\\+"))
      covs <- params@cense.numerator
    }
    if (type == "denominator") {
      cols <- unlist(strsplit(params@cense.denominator, "\\+"))
      covs <- params@cense.denominator
    }
  }
  if(case == "surv") {
    cols <- unlist(strsplit(params@covariates, "\\+"))
    covs <- params@covariates
  }

  cols <- unlist(strsplit(covs, "\\*|\\+"))
  X <- model.matrix(as.formula(paste0("~", covs)), data = newdata[, cols, with = FALSE])
  pred <- predict(model, X, "response")
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
  
  if (case == "default") {
    if (type %in% c("numerator", "denominator")) {
      cols <- unlist(strsplit(ifelse(type == "numerator", params@numerator, params@denominator), "\\+|\\*"))
      covs <- ifelse(type == "numerator", params@numerator, params@denominator)
      
      if (!params@excused) {
        weight <- weight[tx_lag == model, ]
      } else {
        base_filter <- weight[tx_lag == model &
                                get(ifelse(model == 0, params@excused.col0, params@excused.col1)) == 0 &
                                isExcused < 1 &
                                followup != 0, ]
        
        if (type == "denominator" && params@weight.preexpansion) {
          weight <- weight[tx_lag == model & get(ifelse(model == 0, params@excused.col0, params@excused.col1)) == 0, ]
        } else {
          weight <- base_filter
        }
      }
      
      y <- weight[[params@treatment]]
      X <- model.matrix(as.formula(paste0("~", covs)), weight[, cols, with = FALSE])
    }
    
  } else if (case == "LTFU") {
    weight <- weight[!is.na(get(params@cense))]
    cols <- unlist(strsplit(ifelse(type == "numerator", params@cense.numerator, params@cense.denominator), "\\+|\\*"))
    covs <- ifelse(type == "numerator", params@cense.numerator, params@cense.denominator)
    
    weight[, paste0(params@time, params@indicator.squared) := get(params@time)^2]
    y <- abs(weight[[params@cense]] - 1)
    X <- model.matrix(as.formula(paste0("~", covs)), weight[, cols, with = FALSE])
    
  } else if (case == "surv") {
    cols <- unlist(strsplit(params@covariates, "\\+|\\*"))
    covs <- params@covariates
    
    y <- if (type == "compevent") weight[[params@compevent]] else weight[[params@outcome]]
    X <- model.matrix(as.formula(paste0("~", covs)), weight[!is.na(get(params@outcome))][, cols, with = FALSE])
    
  } else if (case == "multinomial") {
    covs <- ifelse(type == "numerator", params@numerator, params@denominator)
    cols <- unlist(strsplit(covs, "\\+|\\*"))
    weight <- weight[tx_lag == model, ]
    
    y <- weight[[params@treatment]]
    X <- model.matrix(as.formula(paste0("~", covs)), weight[, cols, with = FALSE])
  }
  
  return(list(y = y, X = X))
}

fastglm.robust <- function(model, X, y, weight = NULL) {
  coefs <- coef(model)
  residuals <- as.vector(y - X %*% coef(model))

  if (!is.null(weight)) {
    W <- diag(weight)
    X <- sqrt(W) %*% X
    residuals <- sqrt(weight) * residuals
  }

  n <- nrow(X)
  p <- ncol(X)

  omega <- residuals^2 * (n / (n - p))
  xtx.inv <- solve(t(X) %*% X)
  robust <- xtx.inv %*% t(X) %*% sweep(X, 1, omega, `*`) %*% xtx.inv

  return(sqrt(diag(robust)))
}

fastglm.clean <- function(model) {
  model$x <- NULL
  model$y <- NULL
  model$model <- NULL
  return(model)
}