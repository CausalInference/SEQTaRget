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
    if (type == "outcome") {
      cols <- unlist(strsplit(params@covariates, "\\+"))
      covs <- params@covariates
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

#' Function to calculate robust standard errors from a fastglm model
#' 
#' @importFrom stats var
#' @keywords internal
fastglm.robust <- function(model, X, y, weight = NULL) {
  coefs <- coef(model)
  names.var <- colnames(X)
  residuals <- as.vector(y - X %*% coefs)
  
  if (!is.null(weight)) {
    X <- X * sqrt(weight)
    residuals <- residuals * sqrt(weight)
  }
  
  constant <- apply(X, 2, function(x) var(x) == 0)
  variable <- X[, !constant, drop = FALSE]
  
  n <- nrow(X)
  p <- ncol(variable)
  
  omega <- residuals^2 * (n / (n - p))
  xtx <- t(variable) %*% variable
  
  eig <- eigen(xtx, symmetric = TRUE)
  eValues <- eig$values
  eVectors <- eig$vectors
  eValues[eValues < 1e-6] <- 1e-6

  xtx.inv <- eVectors %*% diag(1 / eValues) %*% t(eVectors)
  robust <- sqrt(diag(xtx.inv %*% (t(variable) %*% (variable * omega)) %*% xtx.inv))
  
  full <- numeric(ncol(X))
  names(full) <- names.var
  full[!constant] <- robust
  full[constant] <- 0
  
  return(full)
}

#' Function to clean out non needed elements from fastglm return
#' @param model a fastglm model
#' @keywords internal
fastglm.clean <- function(model) {
  model$x <- NULL
  model$y <- NULL
  model$model <- NULL
  return(model)
}