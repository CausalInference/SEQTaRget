#' Internal function for fitting ITT model on in-memory data
#'
#' @importFrom speedglm speedglm
#'
#' @keywords internal
#' @export
internal.model <- function(DT, method, outcome.col, opts){
  if(method == "ITT"){
    model <- speedglm(formula = paste0(outcome.col, "~", opts$covariates),
                                DT,
                                family = binomial("logit"))
    names(model$coefficients) <- gsub("_bas", "", names(model$coefficients))

  }
  return(model)
}

#' Internal function for creating survival curves
#'
#' @import ggplot2 data.table parallel foreach doRNG
#' @importFrom speedglm speedglm
#'
#' @keywords internal
#' @export
internal.survival <- function(DT, id.col, time.col, outcome.col, treatment.col, opts){
  if(opts$expand) time.col <- "followup"
  if(opts$max.survival == "max") opts$max.survival <- max(DT[[time.col]])
  tx.col <- names(DT)[grep(treatment.col, names(DT))]

  handler <- function(DT, id.col, time.col, outcome.col, tx.col, opts){
    surv.model <- speedglm::speedglm(formula = paste0(outcome.col, "==1~", opts$covariates),
                                   data = DT,
                                   family = binomial("logit"))
    kept <- c("risk0", "risk1", "surv0", "surv1", time.col)

    RMDT <- copy(DT)[, eval(id.col) := paste0(get(id.col), "_", trial)
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
                                          risk1 = 1 - surv1)
                                  ][, ..kept]
    return(RMDT)
  }


  if(opts$bootstrap){
    UIDs <- unique(DT[[id.col]])
    lnID <- length(UIDs)

    if(opts$parallel){
      result <- future.apply::future_lapply(1:opts$nboot, function(x){
        id.sample <- sample(UIDs,
                            round(opts$boot.sample*lnID), replace = FALSE)

        RMDT <- DT[get(id.col) %in% id.sample, ]
        output <- handler(RMDT, id.col, time.col, outcome.col, tx.col, opts)
      }, future.seed = opts$seed)
      result <- rbindlist(result)
    } else {
      # Non Parallel Bootstrapping ===============================================
      result <- lapply(1:opts$nboot, function(x) {
        set.seed(opts$seed + x)
        id.sample <- sample(UIDs,
                            round(opts$boot.sample*lnID), replace = FALSE)

        output <- handler(DT, id.col, time.col, outcome.col, tx.col, opts)
        })
      result <- rbindlist(result)
      }
    kept <- c("surv0_mu", "surv0_lb", "surv0_ub",
              "surv1_mu", "surv1_lb", "surv1_ub",
              "followup")
    DT <- unique(result[, `:=` (surv0_mu = mean(surv0),
                                surv1_mu = mean(surv1),
                                se_surv0 = sd(surv0)/sqrt(opts$nboot),
                                se_surv1 = sd(surv1)/sqrt(opts$nboot)), by = time.col
                        ][, `:=` (surv0_lb = surv0_mu - qnorm(0.975)*se_surv0,
                                  surv0_ub = surv0_mu + qnorm(0.975)*se_surv0,
                                  surv1_lb = surv1_mu - qnorm(0.975)*se_surv1,
                                  surv1_ub = surv1_mu + qnorm(0.975)*se_surv1,
                                  followup = get(time.col))
                          ][, ..kept])

    SDT <- rbind(DT[, .(followup, mu = surv0_mu, lb = surv0_lb, ub = surv0_ub)][, type := "txFALSE"],
                 DT[, .(followup, mu = surv1_mu, lb = surv1_lb, ub = surv1_ub)][, type := "txTRUE"])
    rm(DT)

    surv <- ggplot(SDT, aes(x = followup, y = mu, fill = type)) +
      geom_line(col = "black") +
      geom_ribbon(aes(ymax = ub, ymin = lb), alpha = 0.5) +
      theme_classic() +
      labs(x = "Time", y = "Survival", fill = "") +
      scale_color_discrete(labels = c("No Treatment", "Treatment"))

  } else if(!opts$bootstrap){
    DT <- handler(DT, id.col, time.col, outcome.col, tx.col, opts)
    surv <- melt(DT[, .(txFALSE = mean(surv0),
                        txTRUE = mean(surv1)), by = "followup"],
                 id.vars = "followup") |>
      ggplot(aes(x = followup, y = value, col = variable)) +
      geom_line() +
      theme_classic() +
      labs(x = "Time", y = "Survival", color = "") +
      scale_color_discrete(labels = c("No Treatment", "Treatment"))
  }
  return(surv)
}

internal.weights <- function(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts){
  if(opts$pre.expansion){
    weight <- copy(data)[, tx_lag := shift(get(treatment.col)), by = id.col
                         ][get(time.col) == 0, tx_lag := 0]

    numerator0 <- speedglm::speedglm(paste0(treatment.col, "==1~", opts$numerator),
                                     data = weight[tx_lag == 0, ],
                                     family = binomial("logit"))

    numerator1 <- speedglm::speedglm(paste0(treatment.col, "==1~", opts$numerator),
                                     data = weight[tx_lag == 1, ],
                                     family = binomial("logit"))

    denominator0 <- speedglm::speedglm(paste0(treatment.col, "==1~", opts$denominator),
                                       data = weight[tx_lag == 0, ],
                                       family = binomial("logit"))

    denominator1 <- speedglm::speedglm(paste0(treatment.col, "==1~", opts$denominator),
                                       data = weight[tx_lag == 1, ],
                                       family = binomial("logit"))

    kept <- c("numerator", "denominator", time.col, id.col)
    out <- copy(weight)[tx_lag == 0, `:=` (numerator = predict(numerator0, newdata = .SD, type = "response"),
                                           denominator = predict(denominator0, newdata = .SD, type = "response"))
                  ][tx_lag == 0 & get(treatment.col) == 0, `:=` (numerator = 1 - numerator,
                                                                 denominator = 1 - denominator)
                    ][tx_lag == 1, `:=` (numerator = predict(numerator1, newdata = .SD, type = "response"),
                                         denominator = predict(denominator1, newdata = .SD, type = "response"))
                      ][tx_lag == 1 & get(treatment.col) == 0, `:=` (numerator = 1 - numerator,
                                                                     denominator = 1 - denominator)
                        ][, ..kept]
    setnames(out, time.col, "period")

  }
  return(list(weighted_data = out,
              coef.n0 = coef(numerator0),
              coef.n1 = coef(numerator1),
              coef.d0 = coef(denominator0),
              coef.d1 = coef(denominator1)))
}
