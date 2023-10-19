#' Internal function for fitting ITT model on in-memory data
#'
#' @importFrom speedglm speedglm
#'
#' @keywords internal
itt_model <- function(){
  model <- speedglm::speedglm(tx_init ~ N + L + P,
                              result,
                              family = binomial("logit"))
  return(model)
}
