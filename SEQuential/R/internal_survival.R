#' Internal function for creating survival curves
#'
#' @import ggplot2 data.table future doFuture doRNG future.apply
#' @importFrom fastglm fastglm
#'
#' @keywords internal
internal.survival <- function(params, outcome) {
  result <- local({
    on.exit({
      rm(list = setdiff(ls(), "result"))
      gc()
    }, add = TRUE)

    # Variable pre-definition ===================================
    trialID <- trial <- NULL
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

    handler <- function(DT, params, model) {
      DT <- DT[!is.na(get(params@outcome)), ]
      if (params@multinomial) {
        DT <- DT[get(params@treatment) %in% params@treat.level, ]
      }
      if (!is.na(params@compevent)) {
        ce.data <- prepare.data(DT, params, case = "surv", type = "compevent")
        ce.model <- fastglm(ce.data$X, ce.data$y, family = quasibinomial(link = "logit"), method = params@fastglm.method)
        rm(ce.data)
      }
      
      out_list <- c()
      for (i in seq_along(params@treat.level)) {
        psurv <- paste0("predsurv_", params@treat.level[[i]])
        csurv <- paste0("cumsurv_", params@treat.level[[i]])
        surv <- paste0("surv_", params@treat.level[[i]])
        pce <- paste0("predce_", params@treat.level[[i]])
        cce <- paste0("ce_", params@treat.level[[i]])
        inc <- paste0("inc_", params@treat.level[[i]])
        risk <- paste0("risk_", params@treat.level[[i]])
        
        
        RMDT <- copy(DT)[, trialID := paste0(get(params@id), "_", trial)
                   ][get("followup") == 0,
                     ][rep(1:.N, each = params@survival.max + 1)
                       ][, `:=`(followup = seq(1:.N) - 1,
                                followup_sq = (seq(1:.N) - 1)^2), by = "trialID"
                         ][, eval(tx_bas) := as.character(params@treat.level[[i]])]
        if (params@method == "dose-response" & i == 1) { 
          RMDT[, `:=`(dose = FALSE, dose_sq = FALSE)] 
          } else {
            RMDT[, `:=`(dose = followup, dose_sq = followup_sq)]
            }
        RMDT[, (psurv) := inline.pred(model, newdata = .SD, params, case = "surv")]
        
        if (!is.na(params@compevent)) RMDT[, eval(ce) := inline.pred(ce.model, newdata = .SD, params, case = "surv")]
        RMDT[, eval(surv) := cumprod(1 - get(psurv)), by = "trialID"]
        
        if (!is.na(params@compevent)) {
          RMDT[, eval(cce) := cumprod((1 - get(psurv)) * (1 - get(pce))), by = "trialID"
               ][, eval(inc) := cumsum(get(psurv) * (1 - get(pce)) * get(cce)), by = "trialID"]
          
          RMDT <- RMDT[, setNames(list(mean(get(csurv)), mean(get(inc))), c(surv, inc)), by = "followup"]
          fup0 <- data.table(followup = 0)[, (surv) := 1][, (inc) := 0]
          } else {
            RMDT <- RMDT[, setNames(list(mean(get(surv))), surv), by = "followup"]
            fup0 <- data.table(followup = 0)[, (surv) := 1]
            }
        keep <- list("followup", inc, surv)
        kept <- intersect(keep, names(RMDT))
        
        out_list[[i]] <- rbind(fup0, RMDT[, followup := followup + 1
                                          ][, c(unlist(kept)), with = FALSE]
                               )[, eval(risk) := 1 - get(surv)]
        rm(RMDT)
      }
      
      out <- Reduce(function(x, y) merge(x, y, by = "followup"), out_list) |>
        melt(id.vars = "followup")
      
      return(list(data = out, ce.model = if (!is.na(params@compevent)) ce.model else NA))
    }

    full <- handler(copy(params@DT), params, outcome[[1]]$model)
    
    if (params@bootstrap) {
      UIDs <- unique(params@DT[[params@id]])
      lnID <- length(UIDs)
      if (params@parallel) {
        setDTthreads(1)
        
        result <- future_lapply(2:(params@bootstrap.nboot + 1), function(x) {
          id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = FALSE)
          RMDT <- rbindlist(lapply(id.sample, function(x) params@DT[get(params@id) == x, ]))
          
          out <- handler(RMDT, params, outcome[[x]]$model)
          rm(RMDT); gc()
          return(out)
        }, future.seed = params@seed)
      } else {
        result <- lapply(2:(params@bootstrap.nboot + 1), function(x) {
          id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = FALSE)
          RMDT <- rbindlist(lapply(id.sample, function(x) params@DT[get(params@id) == x, ]))
          
          out <- handler(RMDT, params, outcome[[x]]$model)
          rm(RMDT); gc()
          return(out)
        })
      }
      data <- lapply(seq_along(result), function(x) result[[x]]$data)
      ce.models <- lapply(seq_along(result), function(x) result[[x]]$ce.model)
      
      DT <- rbindlist(data)[, `:=` (se_surv0 = sd(surv.0) / sqrt(params@bootstrap.nboot),
                                      se_surv1 = sd(surv.1) / sqrt(params@bootstrap.nboot),
                                      se_risk0 = sd(risk.0) / sqrt(params@bootstrap.nboot),
                                      se_risk1 = sd(risk.1) / sqrt(params@bootstrap.nboot)), by = "followup"
                              ][, `:=` (surv.0 = NULL, surv.1 = NULL, risk.0 = NULL, risk.1 = NULL)]
      
      if (!is.na(params@compevent)) {
        DT <- DT[, `:=` (se_inc0 = sd(inc.0) / sqrt(params@bootstrap.nboot),
                         se_inc1 = sd(inc.1) / sqrt(params@bootstrap.nboot)), by = "followup"
                 ][, `:=` (inc.0 = NULL, inc.1 = NULL)]
      } 
      surv <- unique(full$data[DT, on = "followup"
                          ][, `:=` (surv0_ub = surv.0 + qnorm(0.975) * se_surv0,
                                    surv0_lb = surv.0 - qnorm(0.975) * se_surv0,
                                    surv1_ub = surv.1 + qnorm(0.975) * se_surv1,
                                    surv1_lb = surv.1 - qnorm(0.975) * se_surv1,
                                    risk0_ub = risk.0 + qnorm(0.975) * se_risk0,
                                    risk0_lb = risk.0 - qnorm(0.975) * se_risk0,
                                    risk1_ub = risk.1 + qnorm(0.975) * se_risk1,
                                    risk1_lb = risk.1 - qnorm(0.975) * se_risk1)])
      
      if (!is.na(params@compevent)) {
        surv <- surv[, `:=` (inc0_ub = inc.0 + qnorm(0.975) * se_inc0,
                             inc0_lb = inc.0 - qnorm(0.975) * se_inc0,
                             inc1_ub = inc.1 + qnorm(0.975) * se_inc1,
                             inc1_lb = inc.1 - qnorm(0.975) * se_inc1)]
        
        surv <- rbind(surv[, list(followup, value = surv.0, lb = surv0_lb, ub = surv0_ub)][, variable := "survival0"],
                      surv[, list(followup, value = surv.1, lb = surv1_lb, ub = surv1_ub)][, variable := "survival1"],
                      surv[, list(followup, value = risk.0, lb = risk0_lb, ub = risk0_ub)][, variable := "risk0"],
                      surv[, list(followup, value = risk.1, lb = risk1_lb, ub = risk1_ub)][, variable := "risk1"],
                      surv[, list(followup, value = inc.0, lb = inc0_lb, ub = inc0_ub)][, variable := "inc0"],
                      surv[, list(followup, value = inc.1, lb = inc1_lb, ub = inc1_ub)][, variable := "inc1"])
      } else {
        surv <- rbind(surv[, list(followup, value = surv.0, lb = surv0_lb, ub = surv0_ub)][, variable := "survival0"],
                      surv[, list(followup, value = surv.1, lb = surv1_lb, ub = surv1_ub)][, variable := "survival1"],
                      surv[, list(followup, value = risk.0, lb = risk0_lb, ub = risk0_ub)][, variable := "risk0"],
                      surv[, list(followup, value = risk.1, lb = risk1_lb, ub = risk1_ub)][, variable := "risk1"])
      }
      
    } else  surv <- melt(full$data, id.vars = "followup")
    out <- list(data = surv, 
                ce.model = if (!is.na(params@compevent)) if (params@bootstrap) c(list(full$ce.model), ce.models) else list(full$ce.model) else list())
    return(out)
  })
  return(result)
}

