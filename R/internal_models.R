#' Internal function for fitting ITT model on in-memory data
#'
#' @importFrom speedglm speedglm
#'
#' @keywords internal
itt_model <- function(){
  model <- speedglm::speedglm(formula,
                              data = data,
                              family = binomial("logit"))
  return(model)
}
