#' Internal function for creating survival curves
#'
#' @import data.table future doFuture doRNG future.apply
#' @importFrom fastglm fastglm
#' @importFrom stats setNames ave
#'
#' @keywords internal
internal.survival <- function(params, outcome) {
  SE <- NULL
  result <- local({
    on.exit({
      rm(list = setdiff(ls(), "result"))
    }, add = TRUE)

    # Variable pre-definition ===================================
    ce <- followup <- followup_sq <- se <- trial <- trialID <- NULL
    tx_bas <- paste0(params@treatment, params@indicator.baseline)

    # Initialize formula cache
    formula_cache <- init_formula_cache(params)

    handler <- function(DT, params, model, cache) {
      DT <- DT[!is.na(get(params@outcome)), ]
      if (params@multinomial) {
        DT <- DT[get(params@treatment) %in% params@treat.level, ]
      }
      if (!is.na(params@compevent)) {
        ce.data <- prepare.data_cached(DT, params, case = "surv", type = "compevent", model = NA, cache)
        ce.model <- clean_fastglm(fastglm(ce.data$X, ce.data$y, family = quasibinomial(link = "logit"), method = params@fastglm.method))
        rm(ce.data)
      }
      
      out_list <- c()
      for (i in seq_along(params@treat.level)) {
        psurv <- paste0("predsurv_", params@treat.level[[i]])
        surv <- paste0("surv_", params@treat.level[[i]])
        pce <- paste0("predce_", params@treat.level[[i]])
        cce <- paste0("ce_", params@treat.level[[i]])
        inc <- paste0("inc_", params@treat.level[[i]])
        risk <- paste0("risk_", params@treat.level[[i]])
        
        RMDT <- DT[rep(1:.N, each = params@survival.max + 1),
                   ][, `:=`(followup = (rowid(get(params@id))) - 1,
                            followup_sq = (rowid(get(params@id)) - 1)^2), by = "trialID"
                     ][, eval(tx_bas) := as.character(params@treat.level[[i]])]
        
        if (params@method == "dose-response" & i == 1) { 
          RMDT[, `:=`(dose = FALSE, dose_sq = FALSE)] 
          } else if (params@method == "dose-response" & i != 1) {
            RMDT[, `:=`(dose = followup, dose_sq = followup_sq)]
            }
        RMDT[, (psurv) := inline.pred(model, newdata = .SD, params, case = "surv", cache = cache)]
        
        if (!is.na(params@compevent)) RMDT[, eval(pce) := inline.pred(ce.model, newdata = .SD, params, case = "surv", cache = cache)]
        RMDT[, eval(surv) := cumprod(1 - get(psurv)), by = "trialID"]
        
        if (!is.na(params@compevent)) {
          RMDT[, eval(cce) := cumprod((1 - get(psurv)) * (1 - get(pce))), by = "trialID"
               ][, eval(inc) := cumsum(get(psurv) * (1 - get(pce)) * get(cce)), by = "trialID"]
          
          RMDT <- RMDT[, setNames(list(mean(get(surv)), mean(get(inc))), c(surv, inc)), by = "followup"]
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
      
      out <- melt(Reduce(function(x, y) merge(x, y, by = "followup"), out_list), id.vars = "followup")
      
      return(list(data = out, ce.model = if (!is.na(params@compevent)) ce.model else NA))
    }

    full <- handler(copy(params@DT)[, "trialID" := paste0(get(params@id), "_", 0, get("trial"))
                                    ][get("followup") == 0, ], params, outcome[[1]]$model, formula_cache)
    
    if (params@bootstrap) {
      UIDs <- unique(params@DT[[params@id]])
      lnID <- length(UIDs)
      
      # Pre-filter and key the data for efficient bootstrap resampling
      baseDT <- params@DT[get("followup") == 0, ]
      if (!identical(key(baseDT), params@id)) setkeyv(baseDT, params@id)
      
      # Helper for efficient keyed bootstrap sampling
      bootstrap_survival_sample <- function(baseDT, params, UIDs, lnID) {
        n_sample <- round(params@bootstrap.sample * lnID)
        id_lookup <- data.table(
          orig_id = sample(UIDs, n_sample, replace = TRUE),
          boot_idx = seq_len(n_sample)
        )
        id_mult <- max(UIDs) + 1L
        
        # Single keyed join instead of N separate filters
        RMDT <- baseDT[id_lookup, on = setNames("orig_id", params@id), allow.cartesian = TRUE
                       ][, "trialID" := paste0(get(params@id), "_", boot_idx, "_", get("trial"))
                         ][, boot_idx := NULL]
        return(RMDT)
      }
      
      if (params@parallel) {
        setDTthreads(1)
        
        result <- future_lapply(2:(params@bootstrap.nboot + 1), function(x) {
          RMDT <- bootstrap_survival_sample(baseDT, params, UIDs, lnID)
          out <- handler(RMDT, params, outcome[[x]]$model, formula_cache)
          rm(RMDT)
          return(out)
        }, future.seed = if (length(params@seed) > 1) params@seed[1] else params@seed)
      } else {
        result <- lapply(2:(params@bootstrap.nboot + 1), function(x) {
          set.seed(params@seed + x)
          RMDT <- bootstrap_survival_sample(baseDT, params, UIDs, lnID)
          out <- handler(RMDT, params, outcome[[x]]$model, formula_cache)
          rm(RMDT)
          return(out)
        })
      }
      data <- lapply(seq_along(result), function(x) result[[x]]$data)
      ce.models <- lapply(seq_along(result), function(x) result[[x]]$ce.model)
      DT.se <- rbindlist(data)[, list(SE = sd(value)), by = c("followup", "variable")]
      
      if (params@bootstrap.CI_method == "se") {
        z <- qnorm(1 - (1 - params@bootstrap.CI)/2)
        surv <- full$data[DT.se, on = c("followup", "variable")
                          ][, `:=` (LCI = max(0, value - z*SE), UCI = min(1, value + z*SE)), by = .I]
      } else {
        DT.q<- rbindlist(data)[, list(LCI = quantile(value, (1 - params@bootstrap.CI)/2),
                                   UCI = quantile(value, 1 - (1 - params@bootstrap.CI)/2)),
                               by = c("followup", "variable")]
        
        surv <- full$data[DT.se, on = c("followup", "variable")
                          ][DT.q, on = c("followup", "variable")]
      }
    } else  surv <- full$data
    out <- list(data = surv, 
                ce.model = if (!is.na(params@compevent)) if (params@bootstrap) c(list(full$ce.model), ce.models) else list(full$ce.model) else list())
    return(out)
  })
  return(result)
}
