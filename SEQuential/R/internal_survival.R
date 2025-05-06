#' Internal function for creating survival curves
#'
#' @import data.table future doFuture doRNG future.apply
#' @importFrom fastglm fastglm
#' @importFrom stats setNames
#'
#' @keywords internal
internal.survival <- function(params, outcome) {
  result <- local({
    on.exit({
      rm(list = setdiff(ls(), "result"))
      gc()
    }, add = TRUE)

    # Variable pre-definition ===================================
    ce <- followup <- followup_sq <- se <- trial <- trialID <- NULL
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
      
      DT <- rbindlist(data)[, list(se = sd(value) / sqrt(params@bootstrap.nboot)),
                             by = c("followup", "variable")]

      surv <- full$data[DT, on = c("followup", "variable")][, `:=` (LCI = value - se, UCI = value + se)
                                                            ][, se := NULL]
      
    } else  surv <- full
    out <- list(data = surv, 
                ce.model = if (!is.na(params@compevent)) if (params@bootstrap) c(list(full$ce.model), ce.models) else list(full$ce.model) else list())
    return(out)
  })
  return(result)
}

