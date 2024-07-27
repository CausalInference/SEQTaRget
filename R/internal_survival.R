#' Internal function for creating survival curves
#'
#' @import ggplot2 data.table future doFuture doRNG future.apply
#' @importFrom speedglm speedglm
#'
#' @keywords internal
#' @export
internal.survival <- function(params){
  params@time <- "followup"
  if(is.infinite(params@max.survival)) params@max.survival <- max(params@DT[[params@time]])

    handler <- function(DT, params){
    surv.model <- speedglm::speedglm(formula = paste0(params@outcome, "==1~", params@surv),
                                   data = DT,
                                   family = binomial("logit"))
    kept <- c("risk0", "risk1", "surv0", "surv1", params@time)

    RMDT <- DT[, eval(params@id) := paste0(get(params@id), "_", trial)
                ][get(params@time) == 0,
                   ][rep(1:.N, each = params@max.survival + 1)
                     ][, `:=` (followup = seq(1:.N)-1,
                               followup_sq = (seq(1:.N)-1)^2), by = eval(params@id)
                       ][, eval(params@treatment) := FALSE
                         ][, `:=` (dose = FALSE,
                                   dose_sq = FALSE)
                           ][, predFALSE := predict(surv.model, newdata = .SD, type = "response")
                             ][, eval(params@treatment) := TRUE
                               ][, `:=` (dose = followup,
                                         dose_sq = followup_sq)
                                 ][, predTRUE := predict(surv.model, newdata = .SD, type = "response")
                                   ][, `:=` (surv0 = cumprod(1 - predFALSE),
                                            surv1 = cumprod(1 - predTRUE)), by = eval(params@id)
                                     ][, `:=` (risk0 = 1 - surv0,
                                               risk1 = 1 - surv1)
                                       ]
    return(RMDT)
  }
  UIDs <- unique(params@DT[[params@id]])
  lnID <- length(UIDs)

  if(params@parallel) {
    setDTthreads(1)

    result <- future_lapply(1:params@nboot, function(x) {
      id.sample <- sample(UIDs, round(params@boot.sample * lnID), replace = TRUE)
      RMDT <- rbindlist(lapply(id.sample, function(x) params@DT[get(params@id) == x, ]))

      out <- handler(RMDT, params)
      return(out)
    }, future.seed = params@seed)
  } else {
    result <- lapply(1:params@nboot, function(x) {
      if(params@bootstrap){
        id.sample <- sample(UIDs, round(params@boot.sample * lnID), replace = TRUE)

        RMDT <- rbindlist(lapply(id.sample, function(x) params@DT[get(params@id) == x, ]))

      } else {
        RMDT <- params@DT
      }

      out <- handler(RMDT, params)
      return(out)
    })
  }
  result <- rbindlist(result)
  if(!params@bootstrap){
    DT <- handler(params@DT, params)
    surv <- melt(DT[, .(txFALSE = mean(surv0),
                        txTRUE = mean(surv1)), by = "followup"],
                 id.vars = "followup") |>
      ggplot(aes(x = followup, y = value, col = variable)) +
      geom_line() +
      theme_classic() +
      labs(x = "Time", y = "Survival", color = "") +
      scale_color_discrete(labels = c("No Treatment", "Treatment"))
  } else {
    kept <- c("surv0_mu", "surv0_lb", "surv0_ub",
              "surv1_mu", "surv1_lb", "surv1_ub",
              "followup")
    DT <- result[, list(surv0_mu = mean(surv0),
                     surv1_mu = mean(surv1),
                     se_surv0 = sd(surv0)/sqrt(params@nboot),
                     se_surv1 = sd(surv1)/sqrt(params@nboot)), by = eval(params@time)
    ][,  `:=` (surv0_lb = surv0_mu - qnorm(0.975)*se_surv0,
               surv0_ub = surv0_mu + qnorm(0.975)*se_surv0,
               surv1_lb = surv1_mu - qnorm(0.975)*se_surv1,
               surv1_ub = surv1_mu + qnorm(0.975)*se_surv1,
               followup = get(params@time))
    ][, ..kept]

    SDT <- rbind(DT[, .(followup, mu = surv0_mu, lb = surv0_lb, ub = surv0_ub)][, variable := "txFALSE"],
                 DT[, .(followup, mu = surv1_mu, lb = surv1_lb, ub = surv1_ub)][, variable := "txTRUE"])
    rm(DT)

    surv <- ggplot(SDT, aes(x = followup, y = mu, fill = variable)) +
      geom_line(col = "black") +
      geom_ribbon(aes(ymax = ub, ymin = lb), alpha = 0.5) +
      theme_classic() +
      labs(x = "Time", y = "Survival", fill = "") +
      scale_color_discrete(labels = c("No Treatment", "Treatment"))

  }
  return(surv)
}
