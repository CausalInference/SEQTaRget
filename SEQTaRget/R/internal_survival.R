#' Internal function for creating survival curves
#'
#' @import data.table future doFuture doRNG future.apply
#' @importFrom fastglm fastglm
#' @importFrom stats setNames ave
#'
#' @keywords internal
internal.survival <- function(params, outcome) {
  SE <- NULL
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
      
      # Get baseline columns (everything except followup-related)
      base_cols <- names(DT)[!names(DT) %in% c("followup", paste0("followup", params@indicator.squared))]
      base_DT <- DT[, base_cols, with = FALSE]
      n_base <- nrow(base_DT)

      out_list <- c()
      for (i in seq_along(params@treat.level)) {
        surv <- paste0("surv_", params@treat.level[[i]])
        inc <- paste0("inc_", params@treat.level[[i]])
        risk <- paste0("risk_", params@treat.level[[i]])

        # Compute predictions per followup step, accumulating cumprod
        surv_accum <- rep(1.0, n_base)
        ce_accum <- if (!is.na(params@compevent)) rep(1.0, n_base) else NULL
        inc_accum <- if (!is.na(params@compevent)) rep(0.0, n_base) else NULL

        pred_DT <- copy(base_DT)
        fup_sq_col <- paste0("followup", params@indicator.squared)
        results <- vector("list", params@survival.max + 1)
        for (fup in 0:params@survival.max) {
          set(pred_DT, j = "followup", value = as.integer(fup))
          set(pred_DT, j = fup_sq_col, value = as.numeric(fup^2))
          set(pred_DT, j = tx_bas, value = as.character(params@treat.level[[i]]))

          if (params@method == "dose-response" & i == 1) {
            set(pred_DT, j = "dose", value = FALSE)
            set(pred_DT, j = "dose_sq", value = FALSE)
          } else if (params@method == "dose-response" & i != 1) {
            set(pred_DT, j = "dose", value = as.numeric(fup))
            set(pred_DT, j = "dose_sq", value = as.numeric(fup^2))
          }

          p_surv <- inline.pred(model, newdata = pred_DT, params, case = "surv", cache = cache)
          surv_accum <- surv_accum * (1 - p_surv)

          if (!is.na(params@compevent)) {
            p_ce <- inline.pred(ce.model, newdata = pred_DT, params, case = "surv", cache = cache)
            ce_accum <- ce_accum * (1 - p_surv) * (1 - p_ce)
            inc_accum <- inc_accum + p_surv * (1 - p_ce) * ce_accum
            results[[fup + 1]] <- list(followup = fup, surv_mean = mean(surv_accum), inc_mean = mean(inc_accum))
          } else {
            results[[fup + 1]] <- list(followup = fup, surv_mean = mean(surv_accum))
          }
        }

        result_dt <- rbindlist(results)
        setnames(result_dt, "surv_mean", surv)
        if (!is.na(params@compevent)) setnames(result_dt, "inc_mean", inc)

        fup0 <- data.table(followup = 0)[, (surv) := 1]
        if (!is.na(params@compevent)) fup0[, (inc) := 0]

        keep <- list("followup", inc, surv)
        kept <- intersect(keep, names(result_dt))

        out_list[[i]] <- rbind(fup0, result_dt[followup > 0
                                                ][, c(unlist(kept)), with = FALSE]
                               )[, eval(risk) := 1 - get(surv)]
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
        old_threads <- getDTthreads()
        setDTthreads(1)
        on.exit(setDTthreads(old_threads), add = TRUE)

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
}
