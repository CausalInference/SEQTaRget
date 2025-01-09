#' Internal function for creating survival curves
#'
#' @import ggplot2 data.table future doFuture doRNG future.apply
#' @importFrom fastglm fastglm
#'
#' @keywords internal
internal.survival <- function(params) {
  result <- local({
    on.exit({
      rm(list = setdiff(ls(), "result"))
      gc()
    }, add = TRUE)

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
    risk0_mu <- risk1_mu <- NULL
    se_risk0 <- se_risk1 <- NULL
    risk0_lb <- risk1_lb <- NULL
    risk0_ub <- risk1_ub <- NULL
    inc0_mu <- inc1_mu <- NULL
    se_inc0 <- se_inc1 <- NULL
    inc0_lb <- inc1_lb <- NULL
    inc0_ub <- inc1_ub <- NULL
    mu <- lb <- ub <- NULL
    followup <- NULL
    numerator <- denominator <- NULL

    tx_bas <- paste0(params@treatment, params@indicator.baseline)
    if (is.infinite(params@survival.max)) params@survival.max <- max(params@DT[["followup"]])

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
                   ][rep(1:.N, each = params@survival.max + 1)
                     ][, `:=`(followup = seq(1:.N) - 1,
                              followup_sq = (seq(1:.N) - 1)^2), by = "trialID"
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
        RMDT <- RMDT[, `:=` (cumsurvTRUE = cumprod((1 - surv.predTRUE) * (1 - ce.predTRUE)),
                             cumsurvFALSE = cumprod((1 - surv.predFALSE) * (1 - ce.predFALSE))), by = "trialID"
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

      out <- rbind(fup0, RMDT[, followup := followup + 1])[, `:=` (risk.0 = 1 - surv.0,
                                                                   risk.1 = 1 - surv.1)]

      return(out)
    }

    # Handling parallel or sequential execution
    UIDs <- unique(params@DT[[params@id]])
    lnID <- length(UIDs)

    if (params@parallel) {
      setDTthreads(1)

      result <- future_lapply(1:params@bootstrap.nboot, function(x) {
        id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = FALSE)
        RMDT <- rbindlist(lapply(id.sample, function(x) params@DT[get(params@id) == x, ]))

        out <- handler(RMDT, params)
        rm(RMDT); gc()
        return(out)
      }, future.seed = params@seed)
    } else {
      result <- lapply(1:params@bootstrap.nboot, function(x) {
        if (params@bootstrap) {
          id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = FALSE)
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
      if (is.na(params@compevent)) {
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
    } else {
      if (is.na(params@compevent)) {
        kept <- c(
          "surv0_mu", "surv0_lb", "surv0_ub",
          "surv1_mu", "surv1_lb", "surv1_ub",
          "risk0_mu", "risk0_lb", "risk0_ub",
          "risk1_mu", "risk1_lb", "risk1_ub",
          "followup"
        )
        DT <- result[, list(
          surv0_mu = mean(surv.0),
          surv1_mu = mean(surv.1),
          risk0_mu = mean(risk.0),
          risk1_mu = mean(risk.1),
          se_surv0 = sd(surv.0) / sqrt(params@bootstrap.nboot),
          se_surv1 = sd(surv.1) / sqrt(params@bootstrap.nboot),
          se_risk0 = sd(risk.0) / sqrt(params@bootstrap.nboot),
          se_risk1 = sd(risk.1) / sqrt(params@bootstrap.nboot)
        ), by = "followup"][, `:=`(
          surv0_lb = surv0_mu - qnorm(0.975) * se_surv0,
          surv0_ub = surv0_mu + qnorm(0.975) * se_surv0,
          surv1_lb = surv1_mu - qnorm(0.975) * se_surv1,
          surv1_ub = surv1_mu + qnorm(0.975) * se_surv1,
          risk0_lb = risk0_mu - qnorm(0.975) * se_risk0,
          risk0_ub = risk0_mu + qnorm(0.975) * se_risk0,
          risk1_lb = risk1_mu - qnorm(0.975) * se_risk1,
          risk1_ub = risk1_mu + qnorm(0.975) * se_risk1,
          followup = followup
        )][, kept, with = FALSE]

        surv <- rbind(
          DT[, list(followup, value = surv0_mu, lb = surv0_lb, ub = surv0_ub)][, variable := "survival0"],
          DT[, list(followup, value = surv1_mu, lb = surv1_lb, ub = surv1_ub)][, variable := "survival1"],
          DT[, list(followup, value = risk0_mu, lb = risk0_lb, ub = risk0_ub)][, variable := "risk0"],
          DT[, list(followup, value = risk1_mu, lb = risk1_lb, ub = risk1_ub)][, variable := "risk1"]
        )
        rm(DT, result)
        gc()
      } else {
        kept <- c(
          "surv0_mu", "surv0_lb", "surv0_ub",
          "surv1_mu", "surv1_lb", "surv1_ub",
          "inc0_mu", "inc0_lb", "inc0_ub",
          "inc1_mu", "inc1_lb", "inc1_ub",
          "followup"
        )
        DT <- result[, list(
          surv0_mu = mean(surv.0),
          surv1_mu = mean(surv.1),
          inc0_mu = mean(inc.0),
          inc1_mu = mean(inc.1),
          se_surv0 = sd(surv.0) / sqrt(params@bootstrap.nboot),
          se_surv1 = sd(surv.1) / sqrt(params@bootstrap.nboot),
          se_inc0 = sd(inc.0) / sqrt(params@bootstrap.nboot),
          se_inc1 = sd(inc.1) / sqrt(params@bootstrap.nboot)
        ), by = "followup"][, `:=`(
          surv0_lb = surv0_mu - qnorm(0.975) * se_surv0,
          surv0_ub = surv0_mu + qnorm(0.975) * se_surv0,
          surv1_lb = surv1_mu - qnorm(0.975) * se_surv1,
          surv1_ub = surv1_mu + qnorm(0.975) * se_surv1,
          inc0_lb = inc0_mu - qnorm(0.975) * se_inc0,
          inc0_ub = inc0_mu + qnorm(0.975) * se_inc0,
          inc1_lb = inc1_mu - qnorm(0.975) * se_inc1,
          inc1_lb = inc1_mu + qnorm(0.975) * se_inc1,
          followup = followup
        )][, kept, with = FALSE]

        surv <- rbind(
          DT[, list(followup, value = surv0_mu, lb = surv0_lb, ub = surv0_ub)][, variable := "survival0"],
          DT[, list(followup, value = surv1_mu, lb = surv1_lb, ub = surv1_ub)][, variable := "survival1"],
          DT[, list(followup, value = inc0_mu, lb = inc0_lb, ub = inc0_ub)][, variable := "inc0"],
          DT[, list(followup, value = inc1_mu, lb = inc1_lb, ub = inc1_ub)][, variable := "inc1"]
        )
        rm(DT, result)
        gc()
      }
    }
    return(surv)
  })

  return(result)
}

