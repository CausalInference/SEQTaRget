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
    . <- variable <- NULL
    ce <- followup <- followup_sq <- se <- trial <- trialID <- NULL
    row_id <- surv_accum <- ce_accum <- inc_accum <- p_surv <- p_ce <- NULL
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
      
      # Only keep columns needed for prediction to minimize memory during replication
      fup_sq_col <- paste0("followup", params@indicator.squared)
      pred_cols <- unique(c(cache$covariates$cols, tx_bas))
      pred_cols <- pred_cols[!pred_cols %in% c("followup", fup_sq_col, "dose", "dose_sq")]
      base_DT <- DT[, pred_cols, with = FALSE]
      n_base <- nrow(base_DT)

      out_list <- c()
      for (i in seq_along(params@treat.level)) {
        surv <- paste0("surv_", params@treat.level[[i]])
        inc <- paste0("inc_", params@treat.level[[i]])
        risk <- paste0("risk_", params@treat.level[[i]])

        # Vectorised predictions across all followup steps at once
        n_fup <- params@survival.max + 1L
        fup_seq <- 0L:as.integer(params@survival.max)

        pred_all <- base_DT[rep(seq_len(n_base), each = n_fup)]
        pred_all[, `:=`(followup = rep(fup_seq, times = n_base),
                        row_id = rep(seq_len(n_base), each = n_fup))]
        pred_all[, (fup_sq_col) := as.numeric(followup^2)]
        pred_all[, (tx_bas) := as.character(params@treat.level[[i]])]

        if (params@method == "dose-response" && i == 1) {
          pred_all[, `:=`(dose = FALSE, dose_sq = FALSE)]
        } else if (params@method == "dose-response" && i != 1) {
          pred_all[, `:=`(dose = as.numeric(followup), dose_sq = as.numeric(followup^2))]
        }

        pred_all[, p_surv := inline.pred(model, newdata = pred_all, params, case = "surv", cache = cache)]
        pred_all[, surv_accum := cumprod(1 - p_surv), by = row_id]

        if (!is.na(params@compevent)) {
          pred_all[, p_ce := inline.pred(ce.model, newdata = pred_all, params, case = "surv", cache = cache)]
          pred_all[, ce_accum := cumprod((1 - p_surv) * (1 - p_ce)), by = row_id]
          pred_all[, inc_accum := cumsum(p_surv * (1 - p_ce) * ce_accum), by = row_id]
          result_dt <- pred_all[, list(surv_mean = mean(surv_accum), inc_mean = mean(inc_accum)), by = followup]
          setnames(result_dt, c("surv_mean", "inc_mean"), c(surv, inc))
        } else {
          result_dt <- pred_all[, list(surv_mean = mean(surv_accum)), by = followup]
          setnames(result_dt, "surv_mean", surv)
        }
        rm(pred_all)

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

    baseDT_main <- params@DT[get("followup") == 0, ]
    baseDT_main[, "trialID" := paste0(get(params@id), "_", 0, get("trial"))]
    full <- handler(baseDT_main, params, outcome[[1]]$model, formula_cache)
    rm(baseDT_main)
    
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
      rm(baseDT)
      data <- lapply(seq_along(result), function(x) result[[x]]$data)
      ce.models <- lapply(seq_along(result), function(x) result[[x]]$ce.model)
      rm(result)

      # Bind all iterations once; reuse for SE, quantile, and paired RD/RR computation
      data_all <- rbindlist(lapply(seq_along(data), function(i) {
        data[[i]][, boot_idx := i]
      }))
      rm(data)
      DT.se <- data_all[, list(SE = sd(value)), by = c("followup", "variable")]

      # Per-iteration final risks for paired RD/RR CI computation in create.risk()
      var_type <- if (any(grepl("^inc_", data_all[["variable"]]))) "^inc_" else "^risk_"
      boot_risks <- data_all[variable %like% var_type
                             ][, variable := as.character(variable)
                               ][, .SD[.N], by = c("variable", "boot_idx")
                                 ][, .(variable, boot_idx, value)]

      if (params@bootstrap.CI_method == "se") {
        z <- qnorm(1 - (1 - params@bootstrap.CI)/2)
        rm(data_all)
        surv <- full$data[DT.se, on = c("followup", "variable")
                          ][, `:=` (LCI = max(0, value - z*SE), UCI = min(1, value + z*SE)), by = .I]
      } else {
        DT.q <- data_all[, list(LCI = quantile(value, (1 - params@bootstrap.CI)/2),
                                UCI = quantile(value, 1 - (1 - params@bootstrap.CI)/2)),
                         by = c("followup", "variable")]
        rm(data_all)
        surv <- full$data[DT.se, on = c("followup", "variable")
                          ][DT.q, on = c("followup", "variable")]
      }
    } else {
      surv <- full$data
      boot_risks <- NULL
    }
  out <- list(data = surv,
              boot_risks = boot_risks,
              ce.model = if (!is.na(params@compevent)) if (params@bootstrap) c(list(full$ce.model), ce.models) else list(full$ce.model) else list())
  return(out)
}
