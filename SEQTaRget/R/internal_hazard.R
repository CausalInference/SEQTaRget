#' Generic function to format a dataset for hazard ratio calculation
#'
#' @keywords internal
#' @import data.table
#' @importFrom survival finegray coxph Surv

internal.hazard <- function(model, params) {
  event <- firstEvent <- outcomeProb <- ce <- ceProb <- trial <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  kept <- names(params@DT)[!names(params@DT) %in% c(paste0("followup", c("", params@indicator.squared)),
                                                    paste0(params@treatment, c("", params@indicator.baseline)),
                                                    "period", params@outcome)]
  
  if (!is.na(params@compevent)) {
    ce.data <- prepare.data(params@DT, params, case = "surv", type = "compevent", model = NA)
    ce.model <- clean_fastglm(fastglm(ce.data$X, ce.data$y, family = quasibinomial(link = "logit"), method = params@fastglm.method))
    rm(ce.data)
  }
  
  handler <- function(data, params, model) {
    trials <- copy(data)[, kept, with = FALSE
                   ][, .SD[1], by = c(params@id, "trial")
                     ][rep(seq_len(.N), each = params@followup.max + 1)
                       ][, "followup" := seq.int(1:.N) - 1, by = c(params@id, "trial")
                         ][, paste0("followup", params@indicator.squared) := get("followup")^2]
    
    out_list <- c()
    for (i in seq_along(params@treat.level)) {
      tmp <- copy(trials)[, eval(tx_bas) := params@treat.level[[i]]
                          ][, "outcomeProb" := inline.pred(model, newdata = .SD, params, type = "outcome")
                            ][, "outcome" := rbinom(.N, 1, outcomeProb)]
      if (!is.na(params@compevent)) {
        tmp[, "ceProb" := inline.pred(ce.model, newdata = .SD, params, case = "surv")
            ][, "ce" := rbinom(.N, 1, ceProb)
              ][, "firstEvent" := if (any(outcome == 1 | ce == 1)) which(outcome == 1 | ce == 1)[1] else .N, by = c(params@id, "trial")]
      } else tmp[, "firstEvent" := if (any(outcome == 1)) which(outcome == 1)[1] else .N, by = c(params@id, "trial")]
      
      out_list[[i]] <- tmp
      rm(tmp)
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
      hr.data <- finegray(Surv(followup, event) ~ ., data, etype = 1)
      hr.res <- coxph(Surv(fgstart, fgstop, fgstatus) ~ get(tx_bas), data = hr.data)
    } else hr.res <- coxph(Surv(followup, event == 1) ~ get(tx_bas), data)
    exp(hr.res$coefficients)
  }
  full <- handler(params@DT, params, model[[1]]$model)
  if (is.na(full)) return(c(Hazard = NA_real_, LCI = NA_real_, UCI = NA_real_))

  bootstrap <- if (params@bootstrap) {
    UIDs <- unique(params@DT[[params@id]])
    lnID <- length(UIDs)
    subDT <- params@DT[, "trialID" := paste0(params@id, "_", trial)]

    if (params@parallel) {
      setDTthreads(1)
      out <- future_lapply(1:params@bootstrap.nboot, function(x) {
        id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = TRUE)
        RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) subDT[get(params@id) == id.sample[x], ]))
        handler(RMDT, params, model[[x]]$model)
      }, future.seed = params@seed)
    } else {
      out <- lapply(1:params@bootstrap.nboot, function(x) {
        set.seed(params@seed + x)
        id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = TRUE)
        RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) subDT[get(params@id) == id.sample[x], ]))
        handler(RMDT, params, model[[x]]$model)
      })
    }
  }
  gc()
  if (params@bootstrap) {
    bootstrap <- unlist(bootstrap)
    if (all(is.na(bootstrap))) return(c(Hazard = NA_real_, LCI = NA_real_, UCI = NA_real_))
    
    if (params@bootstrap.CI_method == "se") {
      z <- qnorm(1 - (1 - params@bootstrap.CI)/2)
      se <- sd(bootstrap, na.rm = TRUE) / sqrt(sum(!is.na(bootstrap)))
      ci <- sort(c(full + z*se, full - z*se), decreasing = FALSE) 
    } else ci <- quantile(bootstrap, 
                          probs = c((1 - params@bootstrap.CI)/2, 
                                    1 - (1 - params@bootstrap.CI)/2))
  } else {
    ci <- c(NA_real_, NA_real_)
  }
  
  out <- c(full, ci)
  names(out) <- c("Hazard", "LCI", "UCI")
  return(out)
}
