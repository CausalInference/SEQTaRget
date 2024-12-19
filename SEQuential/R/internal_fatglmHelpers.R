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
    cols <- unlist(strsplit(params@surv, "\\+"))
    covs <- params@surv
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

prepare.data <- function(weight, params, type, model, case){
  followup <- NULL
  isExcused <- NULL
  tx_lag <- NULL

  weight <- weight[!is.na(get(params@outcome))]
  if (case == "default") {
    if (type == "numerator") {
      cols <- unlist(strsplit(params@numerator, "\\+|\\*"))
      covs <- params@numerator
      if(!params@excused) {
        if(model == 0) weight <- weight[tx_lag == 0, ]
        if(model == 1) weight <- weight[tx_lag == 1, ]

      } else {
        if (model == 0) weight <- weight[get(paste0(params@treatment, params@indicator.baseline)) == 0 &
                                           get(params@excused.col0) == 0 &
                                           isExcused < 1 &
                                           followup != 0, ]
        if(model == 1) weight <- weight[get(paste0(params@treatment, params@indicator.baseline)) == 1 &
                                          get(params@excused.col1) == 0 &
                                          isExcused < 1 &
                                          followup != 0, ]
      }
    } else if (type == "denominator"){
      cols <- unlist(strsplit(params@denominator, "\\+|\\*"))
      covs <- params@denominator
      if(!params@excused) {
        if (model == 0) weight <- weight[tx_lag == 0, ]
        if (model == 1) weight <- weight[tx_lag == 1, ]
      } else {
        if(!params@weight.preexpansion){
          if (model == 0) weight <- weight[tx_lag == 0 &
                                             get(params@excused.col0) == 0 &
                                             isExcused < 1 &
                                             followup != 0, ]
          if (model == 1) weight <- weight[tx_lag == 1 &
                                             get(params@excused.col1) == 0 &
                                             isExcused < 1 &
                                             followup != 0, ]
        } else {
          if (model == 0) weight <- weight[tx_lag == 0 & get(params@excused.col0) == 0, ]
          if (model == 1) weight <- weight[tx_lag == 1 & get(params@excused.col1) == 0, ]
        }
      }
    }
    y <- weight[[params@treatment]]
    X <- model.matrix(as.formula(paste0("~", covs)), weight[, cols, with = FALSE])

  } else if (case == "LTFU") {
    weight <- weight[!is.na(get(params@cense)), ]
    if (type == "numerator") {
      cols <- unlist(strsplit(params@cense.numerator, "\\+|\\*"))
      covs <- params@cense.numerator
      }
    if (type == "denominator") {
      cols <- unlist(strsplit(params@cense.denominator, "\\+|\\*"))
      covs <- params@cense.denominator
      }
    ykept <- c(params@cense)

    y <- abs(weight[[params@cense]] - 1)
    X <- model.matrix(as.formula(paste0("~", covs)), weight[, paste0(params@time, params@indicator.squared) := get(params@time)^2
                                                            ][, cols, with = FALSE])
  } else if (case == "surv") {
    cols <- unlist(strsplit(params@surv, "\\+|\\*"))
    covs <- params@surv

    if (type == "compevent") y <- weight[[params@compevent]] else y <- weight[[params@outcome]]
    X <- model.matrix(as.formula(paste0("~", covs)), data = weight[!is.na(get(params@outcome))]
                      [, cols, with = FALSE])
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
