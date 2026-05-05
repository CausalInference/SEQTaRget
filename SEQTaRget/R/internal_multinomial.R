#' Check a fitted fastglm model for signs of perfect or quasi-complete separation
#'
#' Issues a warning when separation is detected. Because \code{fastglm} uses
#' IWLS and stops at its iteration limit rather than diverging to \code{Inf},
#' separation is identified by two complementary signals:
#' \enumerate{
#'   \item Any coefficient with \code{|coef| > 25} (logit > 25 implies
#'         P > 1 - 1e-11, unreachable without separation).
#'   \item Non-finite coefficients (\code{Inf}/\code{-Inf}/\code{NaN}),
#'         which can occur with other solvers.
#' }
#'
#' @param model a fastglm model object
#' @param label a short string identifying the model (used in the warning message)
#' @keywords internal
check_separation <- function(model, label = "logistic regression") {
  coefs <- model$coefficients
  if (any(!is.finite(coefs)) || any(abs(coefs) > 25)) {
    warning(
      "Perfect or quasi-complete separation detected in the ", label, " model. ",
      "One or more coefficients are non-finite or extremely large, ",
      "which typically indicates that a predictor perfectly discriminates the outcome. ",
      "The resulting weights may be unreliable.",
      call. = FALSE
    )
  }
}

#' Helper function for nested logistic
#'
#' @importFrom stats quasibinomial
#' @keywords internal
multinomial <- function(X, y, family = quasibinomial(), params) {
  y <- as.factor(y)
  ylevels <- levels(y)

  baseline <- ylevels[1]
  models <- list()

  for (class in ylevels[-1]) {
    ybin <- ifelse(y == class, 1, 0)
    model <- fit_glm(X, ybin, family, params = params)
    check_separation(model, label = paste0("multinomial (class = ", class, ")"))
    models[[class]] <- model
  }
  return(list(models = models,
              baseline = baseline,
              levels = ylevels))
}
#' Helper to predict from the nested logistic
#'
#' @keywords internal
multinomial.predict <- function(model, X, target = NULL) {
  models <- model$models
  levels <- model$levels
  X <- as.matrix(X)
  
  coefs_matrix <- do.call(cbind, lapply(models, function(x) as.numeric(coef(x))))
  pred <- X %*% coefs_matrix
  
  exppred <- exp(cbind(0, pred))
  probabilities <- exppred / rowSums(exppred)
  
  if (!is.null(target)) {
    target <- as.character(target)
    col_idx <- match(target, levels)
    if (is.na(col_idx)) {
      stop("Target class not found.")
    }
    return(probabilities[, col_idx])
  }
  return(probabilities)
}


#' Helper function to get the summary table from multinomial
#'
#' @importFrom stats pnorm
#' @keywords internal
multinomial.summary <- function(model) {
  models <- model$models
  baseline_level <- model$baseline
  levels <- model$levels

  summaries <- lapply(names(models), function(cls) {
    coef <- coef(models[[cls]])
    se <- if (!is.null(models[[cls]]$se)) models[[cls]]$se else rep(NA_real_, length(coef))
    data.frame(
      Class = cls,
      Term = names(coef),
      Coefficient = as.numeric(coef),
      Std.Error = se,
      Z.Value = coef / se,
      P.Value = 2 * pnorm(-abs(coef / se))
    )
  })
  summarytable <- do.call(rbind, summaries)

  baseline_row <- data.frame(
    Class = baseline_level,
    Term = "(Intercept)",
    Coefficient = 0,
    Std.Error = NA,
    Z.Value = NA,
    P.Value = NA
  )
  return(rbind(baseline_row, summarytable))
}

model.passer <- function(X, y, params) {
  y <- as.numeric(as.character(y))
  multi <- if (params@multinomial &&
               !params@weight.preexpansion &&
               (params@excused || params@deviation.excused)) FALSE else params@multinomial

  model <- if (!multi) {
    m <- fit_glm(X, y, family = quasibinomial(), params = params)
    check_separation(m, label = "logistic regression")
    m
  } else  {
    multinomial(X, y, family = quasibinomial(), params = params)
  }

  return(model)
}
