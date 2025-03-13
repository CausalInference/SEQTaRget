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
multinomial.predict <- function(model, X) {
  models <- model$models
  baseline <- model$baseline
  levels <- model$levels
  X <- apply(as.matrix(X), 2, as.numeric)
  
  pred <- sapply(models, function(x) {
    coefs <- as.numeric(coef(x))
    as.vector(as.matrix(X) %*% coefs)
  })
  exppred <- exp(cbind(0, pred))
  
  return(exppred / rowSums(exppred))
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