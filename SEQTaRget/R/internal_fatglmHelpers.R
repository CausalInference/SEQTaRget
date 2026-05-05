.pkg_env <- new.env(parent = emptyenv())
.pkg_env$parglm_hint_warned <- FALSE

#' Fit a GLM using the package specified in params@glm.package
#' @param X model matrix
#' @param y response vector
#' @param family family object (e.g. quasibinomial())
#' @param weights optional prior weights vector
#' @param params SEQparams object
#' @importFrom fastglm fastglm
#' @importFrom parglm parglm.fit parglm.control
#' @importFrom stats binomial
#' @keywords internal
fit_glm <- function(X, y, family, weights = NULL, params) {
  if (params@glm.package == "fastglm") {
    if (!.pkg_env$parglm_hint_warned && (nrow(X) >= 10000L || ncol(X) > 10L)) {
      .pkg_env$parglm_hint_warned <- TRUE
      warning(
        "You may find your model fits faster using the parglm backend, ",
        "specified by SEQopts(glm.package = 'parglm')",
        call. = FALSE
      )
    }
    if (is.null(weights)) {
      fastglm(X, y, family = family, method = params@fastglm.method)
    } else {
      fastglm(X, y, family = family, weights = weights, method = params@fastglm.method)
    }
  } else {
    # parglm does not support quasi-likelihood families; substitute the
    # equivalent standard family (coefficients are identical, only dispersion differs)
    if (identical(family$family, "quasibinomial")) family <- binomial(link = family$link)
    ctrl <- if (is.null(params@parglm.control)) parglm.control(method = "FAST") else params@parglm.control
    if (is.null(weights)) {
      parglm.fit(X, y, family = family, nthreads = params@nthreads, control = ctrl)
    } else {
      parglm.fit(X, y, family = family, weights = weights, nthreads = params@nthreads, control = ctrl)
    }
  }
}

#' Predict from a model fitted by fit_glm
#' @param model model object returned by fit_glm
#' @param X model matrix for prediction
#' @param type "response" or "link"
#' @keywords internal
predict_model <- function(model, X, type = "response") {
  if (inherits(model, "fastglm")) {
    predict(model, X, type)
  } else {
    eta <- drop(as.matrix(X) %*% coef(model))
    if (type == "response") model$family$linkinv(eta) else eta
  }
}

#' Helper Function to inline predict a fastglm object
#' @param model a fastglm object
#' @param newdata filler for a .SD from data.table
#' @param params parameter from SEQuential
#' @param type type of prediction
#' @param case case type: "default", "LTFU", "visit", "surv"
#' @param multi multinomial flag
#' @param target target level for multinomial
#' @param cache optional formula cache from init_formula_cache
#'
#' @keywords internal

inline.pred <- function(model, newdata, params, type, case = "default", multi = FALSE, target = NULL, cache = NULL) {
  # Use cache if provided, otherwise fall back to parsing
  if (!is.null(cache)) {
    cached <- switch(
      case,
      "default" = switch(
        type,
        "numerator" = cache$numerator,
        "denominator" = cache$denominator,
        "outcome" = cache$covariates
      ),
      "LTFU" = switch(
        type,
        "numerator" = cache$cense_numerator,
        "denominator" = cache$cense_denominator
      ),
      "visit" = switch(
        type,
        "numerator" = cache$visit_numerator,
        "denominator" = cache$visit_denominator
      ),
      "surv" = cache$covariates
    )
    
    if (!is.null(cached)) {
      X <- fast_model_matrix(cached$formula, newdata, cached$cols, is_simple = cached$is_simple)
      pred <- if (!multi) predict_model(model, X, "response") else multinomial.predict(model, X, target)
      return(pred)
    }
  }

  # Fallback to original parsing (for backwards compatibility)
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
  cols <- formula_vars(covs)
  X <- model.matrix(as.formula(paste0("~", covs)),
                    data = newdata[, cols, with = FALSE])

  pred <- if (!multi) predict_model(model, X, "response") else multinomial.predict(model, X, target)
  return(pred)
}

#' Helper function to prepare data for fastglm
#' @param weight data after undergoing preparation
#' @param params parameter from SEQuential
#' @param type type of model, e.g. d0 = "denominator"
#' @param model model number, e.g. d0 = "zero model"
#' @param case case
#' @param cache cache
#'
#' @keywords internal
# Refactored prepare.data - uses pre-computed cache
prepare.data_cached <- function(weight, params, type, model, case, cache) {
  
  # Get the right cached formula/cols based on case and type
  cached <- switch(
    case,
    "default" = if (type == "numerator") cache$numerator else cache$denominator,
    "LTFU" = if (type == "numerator") cache$cense_numerator else cache$cense_denominator,
    "visit" = if (type == "numerator") cache$visit_numerator else cache$visit_denominator,
    "surv" = cache$covariates
  )
  
  if (is.null(cached)) {
    stop("Missing formula cache for case=", case, ", type=", type)
  }
  
  formula <- cached$formula
  cols <- cached$cols
  
  # ----- Case: default -----
  if (case == "default") {
    if (params@weight.lag_condition) {
      weight <- if (type == "numerator" && params@excused) {
        weight[get(cache$tx_bas) == model]
      } else {
        weight[tx_lag == model]
      }
    }
    
    if (type == "denominator" && !params@weight.preexpansion) {
      weight <- weight[followup != 0L]
    }
    
    if (params@excused) {
      target <- match(model, unlist(params@treat.level))
      excused_col <- params@excused.cols[[target]]
      if (!is.na(excused_col)) {
        weight <- weight[get(excused_col) == 0L]
      }
    }
    
    y <- if (!params@weight.preexpansion && (params@excused || params@deviation.excused)) {
      weight[["censored"]]
    } else {
      weight[[params@treatment]]
    }
    
    # ----- Case: LTFU -----
  } else if (case == "LTFU") {
    weight <- weight[!is.na(get(params@cense))]
    # Only compute squared column if not already present
    
    sq_col <- cache$time_sq_col
    if (!sq_col %in% names(weight)) {
      weight[, (sq_col) := get(params@time)^2]
    }
    y <- abs(weight[[params@cense]] - 1L)
    
    # ----- Case: visit -----
  } else if (case == "visit") {
    sq_col <- cache$time_sq_col
    if (!sq_col %in% names(weight)) {
      weight[, (sq_col) := get(params@time)^2]
    }
    y <- weight[[params@visit]]
    
    # ----- Case: surv -----
  } else if (case == "surv") {
    weight <- weight[!is.na(get(params@outcome))]
    y <- if (type == "compevent") weight[[params@compevent]] else weight[[params@outcome]]
  }
  
  # ----- Build design matrix -----
  
  # Fast path: if formula has no interactions/special terms, build directly
  # Check if simple additive (no :, no I(), no poly(), etc.)
  X <- fast_model_matrix(formula, weight, cols, is_simple = cached$is_simple)
  return(list(y = y, X = X))
}

# Fast model matrix builder - avoids overhead for simple cases
fast_model_matrix <- function(formula, data, cols, is_simple = FALSE) {
  subset_data <- data[, ..cols]

  # Fast path for simple additive numeric-only models: no setDF needed
  if (isTRUE(is_simple) && all(vapply(subset_data, is.numeric, logical(1)))) {
    X <- as.matrix(subset_data)
    X <- cbind(Intercept = 1, X)
    return(X)
  }

  # Standard path: setDF in-place on the temp subset, then model.matrix
  X <- model.matrix(formula, data = setDF(subset_data), na.action = stats::na.pass)
  return(X)
}

#' Strip large components from a model object returned by fit_glm
#' @param model a model object (fastglm or parglm.fit)
#' @keywords internal
clean_fastglm <- function(model) {
  strip <- function(m) {
    m$x <- NULL
    m$y <- NULL
    m$model <- NULL
    m$fitted.values <- NULL
    m$residuals <- NULL
    m$linear.predictors <- NULL
    m$weights <- NULL
    m$prior.weights <- NULL
    m$qr <- NULL
    m
  }
  if (!is.null(model$models)) {
    model$models <- lapply(model$models, strip)
    return(model)
  }
  strip(model)
}
