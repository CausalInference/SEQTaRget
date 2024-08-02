#' Internal function for fitting ITT model on in-memory data
#'
#' @importFrom speedglm speedglm
#'
#' @keywords internal
internal.model <- function(data, params) {
  if (params@method == "ITT") {
    model <- speedglm(
      formula = paste0(params@outcome, "~", params@covariates),
      data,
      family = quasibinomial("logit")
    )
  } else if (params@method %in% c("dose-response", "censoring")) {
    model <- speedglm(
      formula = paste0(params@outcome, "==1~", params@covariates),
      data,
      family = quasibinomial("logit"),
      weights = data$weight
    )
  }

  return(model)
}
