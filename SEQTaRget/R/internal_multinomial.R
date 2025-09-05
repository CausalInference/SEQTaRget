#' Helper function for nested logistic
#'
#' @importFrom fastglm fastglm
#' @importFrom stats quasibinomial
#' @keywords internal
multinomial <- function(X, y, family = quasibinomial(), method) {
  y <- as.factor(y)
  ylevels <- levels(y)
  
  baseline <- ylevels[1]
  models <- list()
  
  for (class in ylevels[-1]) {
    ybin <- ifelse(y == class, 1, 0)
    model <- fastglm(X, ybin, family, method = method)
    
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
  
  pred <- sapply(models, function(x) {
    coefs <- as.numeric(coef(x))
    as.vector(X %*% coefs)
  })
  
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
#' @importFrom stats pnorm vcov
#' @keywords internal
multinomial.summary <- function(model) {
  models <- model$models
  baseline <- model$baseline
  levels <- model$levels
  
  summarytable <- data.frame()
  
  for (class in names(models)) {
    coef <- coef(models[[class]])
    se <- sqrt(diag(vcov(models[[class]])))
    
    summary <- data.frame(
      Class = class,
      Term = names(coef),
      Std.Error = se,
      Z.Value = coef / se,
      P.Value = 2 * pnorm(-abs(coef / se))
    )
    summarytable <- rbind(summarytable, summary)
  }
  
  baseline <- data.frame(
    Class = baseline,
    Term = "(Intercept)",
    Coefficient = 0,
    Std.Error = NA,
    Z.Value = NA,
    P.Value = NA
  )
  return(rbind(baseline, summarytable))
}

model.passer <- function(X, y, params) {
  y <- as.numeric(as.character(y))
  multi <- if (params@multinomial & 
               !params@weight.preexpansion & 
               (params@excused | params@deviation.excused)) FALSE else params@multinomial
      
  model <- if (!multi) {
    fastglm(X, y, family = quasibinomial(), method = params@fastglm.method)
  } else  {
    multinomial(X, y, family = quasibinomial(), method = params@fastglm.method)
  }
  
  return(model)
}
