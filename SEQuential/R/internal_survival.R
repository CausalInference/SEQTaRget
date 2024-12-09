#' Internal function for creating survival curves
#'
#' @import ggplot2 data.table future doFuture doRNG future.apply
#' @importFrom fastglm fastglm
#'
#' @keywords internal
internal.survival <- function(params) {
  # Variable pre-definition ===================================
  trialID <- trial <- NULL
  surv.predTRUE <- surv.predFALSE <- NULL
  ce.predTRUE <- ce.predFALSE <- NULL
  cumsurvFALSE <- cumsurvTRUE <- NULL
  followup <- followup_sq <- NULL
  surv.0 <- surv.1 <- NULL
  risk.0 <- risk.1 <- NULL
  inc.0 <- inc.1 <- NULL
  variable <- NULL
  surv0_mu <- surv1_mu <- NULL
  se_surv0 <- se_surv1 <- NULL
  surv0_lb <- surv1_lb <- NULL
  surv0_ub <- surv1_ub <- NULL
  mu <- lb <- ub <- NULL
  followup <- NULL
  numerator <- denominator <- NULL

  tx_bas <- paste0(params@treatment, params@baseline.indicator)
  if (is.infinite(params@max.survival)) params@max.survival <- max(params@DT[["followup"]])

  handler <- function(DT, params) {
    if (params@multinomial) {
      DT <- DT[get(params@treatment) %in% params@treat.level]
    }
    if (!is.na(params@compevent)) {
      ce.data <- prepare.data(DT, params, case = "surv", type = "compevent")
      ce.model <- fastglm::fastglm(ce.data$X, ce.data$y, family = quasibinomial(link = "logit"), method = params@fastglm.method)
      rm(ce.data)
    }
    surv.data <- prepare.data(DT, params, case = "surv", type = "default")
    surv.model <- fastglm::fastglm(surv.data$X, surv.data$y, family = quasibinomial(link = "logit"), method = params@fastglm.method)
    rm(surv.data)
    kept <- c("followup", "risk0", "risk1", "surv.0", "surv.1", "inc.1", "inc.0")

    RMDT <- DT[, trialID := paste0(get(params@id), "_", trial)
               ][get("followup") == 0,
                 ][rep(1:.N, each = params@max.survival + 1)
                   ][, `:=`(followup = seq(1:.N)-1,
                            followup_sq = (seq(1:.N)-1)^2), by = "trialID"
                     ][, eval(tx_bas) := params@treat.level[[1]]]

    if (params@method == "dose-response") RMDT <- RMDT[, `:=`(dose = FALSE, dose_sq = FALSE)]

    RMDT <- RMDT[, surv.predFALSE := inline.pred(surv.model, newdata = .SD, params, case = "surv")]

    if (!is.na(params@compevent)) RMDT <- RMDT[, ce.predFALSE := inline.pred(ce.model, newdata = .SD, params, case = "surv")]
    if (params@method == "dose-response") RMDT <- RMDT[, `:=`(dose = followup, dose_sq = followup_sq)]

    RMDT <- RMDT[, eval(tx_bas) := params@treat.level[[2]]][, surv.predTRUE := inline.pred(surv.model, newdata = .SD, params, case = "surv")]
    if (!is.na(params@compevent)) RMDT <- RMDT[, ce.predTRUE := inline.pred(ce.model, newdata = .SD, params, case = "surv")]

    RMDT <- RMDT[, `:=`(surv.0 = cumprod(1 - surv.predFALSE),
                        surv.1 = cumprod(1 - surv.predTRUE)), by = "trialID"
                 ][, `:=`(risk0 = 1 - surv.0,
                          risk1 = 1 - surv.1)]

    if (!is.na(params@compevent)) {
      RMDT <- RMDT[, `:=` (cumsurvTRUE = cumprod((1-surv.predTRUE)*(1-ce.predTRUE)),
                           cumsurvFALSE = cumprod((1-surv.predFALSE)*(1-ce.predFALSE))), by = "trialID"
                    ][, `:=` (inc.0 = cumsum(surv.predFALSE * (1 - ce.predFALSE) * cumsurvFALSE),
                              inc.1 = cumsum(surv.predTRUE * (1 - ce.predTRUE) * cumsurvTRUE)), by = "trialID"
                      ]

      RMDT <- RMDT[, list(
        surv.0 = mean(surv.0),
        surv.1 = mean(surv.1),
        inc.0 = mean(inc.0),
        inc.1 = mean(inc.1)), by = "followup"
        ]

      fup0 <- data.table(followup = 0, surv.0 = 1, surv.1 = 1, inc.0 = 0, inc.1 = 0)
    } else {
      RMDT <- RMDT[, list(
        surv.0 = mean(surv.0),
        surv.1 = mean(surv.1)), by = "followup"
        ]

      fup0 <- data.table(followup = 0, surv.0 = 1, surv.1 = 1)
    }

    out <- rbind(fup0, RMDT[, followup := followup+1])[, `:=` (risk.0 = 1 - surv.0,
                                                               risk.1 = 1 - surv.1)]


    return(out)
  }
  UIDs <- unique(params@DT[[params@id]])
  lnID <- length(UIDs)

  if (params@parallel) {
    setDTthreads(1)

    result <- future_lapply(1:params@nboot, function(x) {
      id.sample <- sample(UIDs, round(params@boot.sample * lnID), replace = TRUE)
      RMDT <- rbindlist(lapply(id.sample, function(x) params@DT[get(params@id) == x, ]))

      out <- handler(RMDT, params)
      rm(RMDT); gc()
      return(out)
    }, future.seed = params@seed)
  } else {
    result <- lapply(1:params@nboot, function(x) {
      if (params@bootstrap) {
        id.sample <- sample(UIDs, round(params@boot.sample * lnID), replace = TRUE)

        RMDT <- rbindlist(lapply(id.sample, function(x) params@DT[get(params@id) == x, ]))
      } else {
        RMDT <- params@DT
      }

      out <- handler(RMDT, params)
      rm(RMDT); gc()
      return(out)
    })
  }
  result <- rbindlist(result)
  if (!params@bootstrap) {
    surv.DT <- handler(params@DT, params)
    gc()
    if(is.na(params@compevent)) {
      surv <- melt(
        surv.DT[, list(surv.0 = mean(surv.0), surv.1 = mean(surv.1), risk.0 = mean(risk.0), risk.1 = mean(risk.1)), by = "followup"],
        id.vars = "followup"
      )
    } else {
      surv <- melt(
        surv.DT[, list(surv.0 = mean(surv.0), surv.1 = mean(surv.1), inc.0 = mean(inc.0), inc.1 = mean(inc.1)), by = "followup"],
        id.vars = "followup"
      )
    }
    if (TRUE) surv <- surv[!variable %in% c("risk.1", "risk.0")] #TODO
      plot <- ggplot(surv, aes(x = followup, y = value, col = variable)) +
      geom_line() +
      theme_classic() +
      labs(x = "Time", y = "Survival", color = "") +
      scale_color_discrete(labels = c("No Treatment", "Treatment"))
      #TODO - bootstrapping stuff for inc0 and inc1
  } else {
    kept <- c(
      "surv0_mu", "surv0_lb", "surv0_ub",
      "surv1_mu", "surv1_lb", "surv1_ub",
      "followup"
    )
    DT <- result[, list(
      surv0_mu = mean(surv.0),
      surv1_mu = mean(surv.1),
      se_surv0 = sd(surv.0) / sqrt(params@nboot),
      se_surv1 = sd(surv.1) / sqrt(params@nboot)
    ), by = eval(params@time)][, `:=`(
      surv0_lb = surv0_mu - qnorm(0.975) * se_surv0,
      surv0_ub = surv0_mu + qnorm(0.975) * se_surv0,
      surv1_lb = surv1_mu - qnorm(0.975) * se_surv1,
      surv1_ub = surv1_mu + qnorm(0.975) * se_surv1,
      followup = get(params@time)
    )][, kept, with = FALSE]

    SDT <- rbind(
      DT[, list(followup, mu = surv0_mu, lb = surv0_lb, ub = surv0_ub)][, variable := "txFALSE"],
      DT[, list(followup, mu = surv1_mu, lb = surv1_lb, ub = surv1_ub)][, variable := "txTRUE"]
    )
    rm(DT, result)
    gc()

    plot <- ggplot(SDT, aes(x = followup, y = mu, fill = variable)) +
      geom_line(col = "black") +
      geom_ribbon(aes(ymax = ub, ymin = lb), alpha = 0.5) +
      theme_classic() +
      labs(x = "Time", y = "Survival", fill = "") +
      scale_color_discrete(labels = c("No Treatment", "Treatment"))
  }
  return(plot)
}
