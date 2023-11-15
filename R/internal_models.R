#' Internal function for fitting ITT model on in-memory data
#'
#' @importFrom speedglm speedglm
#'
#' @keywords internal
internal.model <- function(data, method, outcome.col, covariates, opts){
  formula <- as.formula(paste0(outcome.col, "~", covariates))
  if(method == "ITT"){
    model <- speedglm::speedglm(formula,
                                data,
                                family = binomial("logit"))
    names(model$coefficients) <- gsub("_bas", "", names(model$coefficients))
  }
  return(model)
}

#' Internal function for creating survival curves
#'
#' @import ggplot2 data.table
#'
#' @keywords internal

internal.survival <- function(DT, id.col, time.col, outcome.col, treatment.col, opts){
  if(opts$expand == TRUE) time.col <- "period"
  if(opts$max.survival == "max") opts$max.survival <- max(DT[[time.col]])

  tx.col <- names(DT)[grep(treatment.col, names(DT))]

  surv.model <- speedglm::speedglm(formula = paste0(outcome.col, "==1~", opts$covariates),
                                   data = DT,
                                   family = binomial("logit"))
  DT <- DT[get(time.col) == 0,
           ][rep(1:.N, each = opts$max.survival)
             ][, `:=` (followup = seq(1:.N)-1,
                       followup_sq = (seq(1:.N)-1)^2), by = eval(id.col)
               ][, eval(tx.col) := FALSE
                 ][, predFALSE := predict(surv.model, newdata = .SD, type = "response")
                   ][, eval(tx.col) := TRUE
                     ][, predTRUE := predict(surv.model, newdata = .SD, type = "response")
                       ][, `:=` (surv0 = cumprod(1 - predFALSE),
                                 surv1 = cumprod(1 - predTRUE)), by = eval(id.col)
                         ][, `:=` (risk0 = 1 - surv0,
                                   risk1 = 1 - surv1)]

  surv <- melt(DT[, .(txFALSE = mean(surv0),
                      txTRUE = mean(surv1)), by = "followup"],
               id.vars = "followup") |>
    ggplot(aes(x = followup, y = value, col = variable)) +
    geom_line() +
    theme_classic() +
    labs(x = "Time", y = "Survival", color = "") +
    scale_color_discrete(labels = c("No Treatment", "Treatment"))

  return(surv)
}

create.numerator <- function(DT, opts){
  if(opts$weighted == 0){
    DT[, numerator := 1]
  }
}
