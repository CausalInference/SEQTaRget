#' Internal function for fitting ITT model on in-memory data
#'
#' @importFrom speedglm speedglm
#'
#' @keywords internal
internal.model <- function(data, causal_contrast, outcome.col, opts){
  if(causal_contrast == "ITT"){
    model <- speedglm::speedglm(formula = paste0(outcome.col, "~", opts$covariates),
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
  if(opts$expand == TRUE) time.col <- "followup"
  if(opts$max.survival == "max") opts$max.survival <- max(DT[[time.col]])

  tx.col <- names(DT)[grep(treatment.col, names(DT))]

  surv.model <- speedglm::speedglm(formula = paste0(outcome.col, "==1~", opts$covariates),
                                   data = DT,
                                   family = binomial("logit"))
  DT <- DT[, eval(id.col) := paste0(get(id.col), "_", trial)
             ][get(time.col) == 0,
               ][rep(1:.N, each = opts$max.survival + 1)
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

#' Internal function for creating weights
#' @import data.table
#' @importFrom speedglm speedglm
#'
#' @keywords internal

internal.weights <- function(DT, data, id.col, time.col, outcome.col, treatment.col, opts){
  if(!opts$stabilized_weights){
    if(opts$pre_expansion){
      data <- as.data.table(data)

      weight <- data[, `:=` (tx_lag = lag(get(treatment.col)),
                             time = get(time.col),
                             time_sq = get(time.col)^2), keyby = eval(id.col)]

      model1 <- speedglm::speedglm(formula = paste0(treatment.col, "==1~", opts$covariates, "+time+time_sq"),
                                   data = weight[tx_lag == 1, ],
                                   family = binomial("logit"))
      model0 <- speedglm::speedglm(formula = paste0(paste0(treatment.col, "==0~", opts$covariates, "+time+time_sq")),
                                   data = weight[tx_lag == 0, ],
                                   family = binomial("logit"))

      out <- weight[tx_lag == 0, pred := predict(model0, newdata = .SD, type = "response")
                    ][tx_lag == 1, pred := predict(model1, newdata = .SD, type = "response")
                      ][, cmprd := cumprod(pred), keyby = eval(id.col)
                        ][, wt := 1/cmprd]

      percentile <- quantile(out$wt, probs = c(.25, .5, .75))
      stats <- list(min = min(out$wt),
                    max = max(out$wt),
                    sd = sd(out$wt),
                    p25 = percentile[[1]],
                    p50 = percentile[[2]],
                    p75 = percentile[[3]])
      #weights merge to data on PERIOD, then the first of the ID is set to 1

    } else if(!opts$pre_expansion){
      # NON STABILIZED - POST EXPANSION
    }
  } else if(opts$stabilized_weights){
    if(opts$pre_expansion){
      #STABILIZED - PRE-EXPANSION
    } else if(!opts$pre_expansion){
      #STABILIZED - POST-EXPANSION
    }
  }
  return(list(weighted_data = out,
              weighted_stats = stats))
}







