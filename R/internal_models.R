#' Internal function for fitting ITT model on in-memory data
#'
#' @importFrom speedglm speedglm
#'
#' @keywords internal
itt_model <- function(DT, opts){
  model <- speedglm::speedglm(create.formula(y = "PLACEHOLDER", x = opts$covariates),
                              data = DT,
                              family = binomial("logit"))
  return(model)
}
