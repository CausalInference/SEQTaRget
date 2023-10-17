#' Internal Function to translate \code{x} (covariates) and \code{y} to a formula object
#'
#' @keywods internal
create.formula <- function(y, x){
  as.formula(paste0(y, "~", x))
}
