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

internal.survival <- function(data, id.col, time.col, outcome.col, treatment.col, opts){
  if(opts$max.survival == "max") opts$max.survival <- max(data[[time.col]])
  tx.col <- names(data)[grep(treatment.col, names(data))]

  surv.model <- speedglm::speedglm(formula = paste0(outcome.col, "==1~", opts$covariates),
                                   data = data,
                                   family = binomial("logit"))
  suppressWarnings(
  data <- data[, eval(tx.col) := FALSE
               ][, predFALSE := predict(surv.model, newdata = .SD)
                 ][, eval(tx.col) := TRUE
                   ][, predTRUE := predict(surv.model, newdata = .SD)
                     ][, `:=` (surv0 = cumprod(1 - predFALSE),
                               surv1 = cumprod(1 - predTRUE)), by = eval(id.col)
                       ][, `:=` (risk0 = 1 - surv0,
                                 risk1 = 1 - surv1)]
  )

  surv <- melt(data[, .(txFALSE = mean(surv0),
                        txTRUE = mean(surv1)), by = time.col],
               id.vars = eval(time.col)) |>
    ggplot(aes(x = get(time.col), y = value, col = variable)) +
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
