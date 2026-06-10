#' Generic function to format a dataset for hazard ratio calculation
#'
#' @keywords internal
#' @import data.table
#' @importFrom survival finegray coxph Surv coxph.fit agreg.fit coxph.control

internal.hazard <- function(model, params, cache) {
  event <- firstEvent <- outcomeProb <- ce <- ceProb <- trial <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  kept <- names(params@DT)[!names(params@DT) %in% c(paste0("followup", c("", params@indicator.squared)),
                                                    paste0(params@treatment, c("", params@indicator.baseline)),
                                                    "period", params@outcome)]
  
  if (!is.na(params@compevent)) {
    ce.data <- prepare.data_cached(params@DT, params, case = "surv", type = "compevent", model = NA, cache)
    ce.model <- clean_fastglm(fit_glm(ce.data$X, ce.data$y, family = quasibinomial(link = "logit"), params = params))
    rm(ce.data)
  }
  
  handler <- function(data, params, model, cache) {
    trials <- data[, kept, with = FALSE
                   ][, .SD[1], by = c(params@id, "trial")
                     ][rep(seq_len(.N), each = params@followup.max + 1)
                       ][, "followup" := seq.int(1:.N) - 1, by = c(params@id, "trial")
                         ][, paste0("followup", params@indicator.squared) := get("followup")^2]

    # Pre-allocate output list instead of concatenating with c()
    out_list <- vector("list", length(params@treat.level))

    for (i in seq_along(params@treat.level)) {
      out_list[[i]] <- copy(trials)[, eval(tx_bas) := params@treat.level[[i]]
                                    ][, "outcomeProb" := inline.pred(model, newdata = .SD, params, type = "outcome", cache = cache)
                                      ][, "outcome" := rbinom(.N, 1, fcoalesce(outcomeProb, 0.5))]

      if (!is.na(params@compevent)) {
        out_list[[i]][, "ceProb" := inline.pred(ce.model, newdata = .SD, params, case = "surv", cache = cache)
                      ][, "ce" := rbinom(.N, 1, ceProb)
                        ][, "firstEvent" := { m <- match(TRUE, outcome == 1 | ce == 1); if (is.na(m)) .N else m }, by = c(params@id, "trial")]
      } else out_list[[i]][, "firstEvent" := { m <- match(TRUE, outcome == 1); if (is.na(m)) .N else m }, by = c(params@id, "trial")]
    }

    rm(trials)
    out <- rbindlist(out_list)
    rm(out_list)
    data <- out[out[, .I[seq_len(firstEvent[1])], by = c(params@id, "trial", tx_bas)]$V1
               ][, .SD[.N], by = c(params@id, "trial", tx_bas)
                 ][, firstEvent := NULL
                   ][, event := 0
                     ][outcome == 1, event := 1]
    if (!is.na(params@compevent)) data <- data[ce == 1, event := 2][, ce := NULL]
    
    data <- data[, `:=` (outcome = NULL, event = factor(event, levels = c(0, 1, 2)))]
    rm(out)
    
    if (!is.na(params@compevent)) {
      # Fine-Gray subdistribution data, then a counting-process Cox fit via the
      # C fitter directly (agreg.fit) on a prebuilt design matrix, skipping the
      # model.frame/model.matrix rebuild that coxph() repeats on every bootstrap
      # iteration (~8x faster on this step, identical coefficient). finegray()
      # still runs each iteration as the simulated outcomes differ.
      hr.data <- finegray(Surv(followup, event) ~ ., data, etype = 1)
      x <- matrix(as.double(hr.data[[tx_bas]]), ncol = 1L, dimnames = list(NULL, tx_bas))
      y <- Surv(as.double(hr.data[["fgstart"]]), as.double(hr.data[["fgstop"]]),
                hr.data[["fgstatus"]])
      hr.res <- agreg.fit(x, y, strata = NULL, offset = NULL, init = 0,
                          control = coxph.control(), weights = hr.data[["fgwt"]],
                          method = "efron", rownames = NULL)
    } else {
      # Univariate Cox: build the one-column design matrix and call the C fitter
      # directly, skipping the model.frame/model.matrix rebuild that coxph()
      # repeats on every bootstrap iteration (~7x faster, identical coefficient).
      x <- matrix(as.double(data[[tx_bas]]), ncol = 1L, dimnames = list(NULL, tx_bas))
      y <- Surv(as.double(data[["followup"]]), data[["event"]] == 1)
      hr.res <- coxph.fit(x, y, strata = NULL, offset = NULL, init = 0,
                          control = coxph.control(), weights = NULL,
                          method = "efron", rownames = NULL)
    }
    hr.res$coefficients  # Return log hazard ratio for bootstrap monitoring
  }
  set.seed(params@seed)
  full <- handler(params@DT, params, model[[1]]$model, cache)
  if (is.na(full)) return(c(`Hazard ratio` = NA_real_, LCI = NA_real_, UCI = NA_real_))

  bootstrap <- if (params@bootstrap) {
    UIDs <- unique(params@DT[[params@id]])
    lnID <- length(UIDs)
    
    # Key the data for efficient bootstrap resampling
    if (!identical(key(params@DT), params@id)) setkeyv(params@DT, params@id)
    
    # Helper for efficient keyed bootstrap sampling
    bootstrap_hazard_sample <- function(DT, params, UIDs, lnID) {
      n_sample <- round(params@bootstrap.sample * lnID)
      id_lookup <- data.table(
        orig_id = sample(UIDs, n_sample, replace = TRUE),
        boot_idx = seq_len(n_sample)
      )
      
      # Single keyed join instead of N separate filters
      RMDT <- DT[id_lookup, on = setNames("orig_id", params@id), allow.cartesian = TRUE
                 ][, boot_idx := NULL]
      return(RMDT)
    }

    # model[[1]] is the full-data fit; bootstrap fits live at model[[2]] onwards,
    # where model[[x + 1]] was fit (in internal.analysis) on the resample drawn
    # under seed + x, matching the resample drawn here.
    if (params@parallel) {
      old_threads <- getDTthreads()
      setDTthreads(1)
      on.exit(setDTthreads(old_threads), add = TRUE)
      out <- future_lapply(1:params@bootstrap.nboot, function(x) {
        RMDT <- bootstrap_hazard_sample(params@DT, params, UIDs, lnID)
        handler(RMDT, params, model[[x + 1]]$model, cache)
      }, future.seed = if (length(params@seed) > 1) params@seed[1] else params@seed)
    } else {
      out <- lapply(1:params@bootstrap.nboot, function(x) {
        set.seed(params@seed + x)
        RMDT <- bootstrap_hazard_sample(params@DT, params, UIDs, lnID)
        handler(RMDT, params, model[[x + 1]]$model, cache)
      })
    }
  }
  if (params@bootstrap) {
    bootstrap <- unlist(bootstrap)  # log hazard ratios from bootstrap samples
    if (all(is.na(bootstrap))) return(c(`Hazard ratio` = NA_real_, LCI = NA_real_, UCI = NA_real_))
    
    # Calculate CI on log scale, then exponentiate
    if (params@bootstrap.CI_method == "se") {
      z <- qnorm(1 - (1 - params@bootstrap.CI)/2)
      se <- sd(bootstrap, na.rm = TRUE)
      ci <- exp(sort(c(full + z*se, full - z*se), decreasing = FALSE))
    } else ci <- exp(quantile(bootstrap, 
                              probs = c((1 - params@bootstrap.CI)/2, 
                                        1 - (1 - params@bootstrap.CI)/2)))
  } else {
    ci <- c(NA_real_, NA_real_)
  }
  
  out <- c(exp(full), ci)  # Exponentiate log HR to get HR
  names(out) <- c("Hazard ratio", "LCI", "UCI")
  return(out)
}
