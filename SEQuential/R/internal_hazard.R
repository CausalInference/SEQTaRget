#' Generic function to format a dataset for hazard ratio calculation
#' @param data expanded data
#' @param params SEQuential params
#'
#' @keywords internal
#' @import data.table
#' @importFrom survival finegray coxph Surv

hazard <- function(model, params) {
  set.seed(params@seed)
  subgroups <- if (!is.na(params@subgroup)) unique(params@data[[params@subgroup]]) else 1L
  for (i in seq_along(subgroups)) {
    if (!is.na(params@subgroup)) cat("For subgroup:", subgroups[i], "\n")
    cat("Simulation Initialized with:\n")
    
    for (j in seq_along(params@time_varying)) {
      item <- unlist(params@hazard.time_varying.dist[[i]][j])
      cat(paste0(names(item), ": ", unlist(item), "\n\n"))
    }
    generators <- params@hazard.time_varying.dist[[i]]
    functions <- generators[order(names(generators))]
    tx_bas <- paste0(params@treatment, params@indicator.baseline)
    handler <- function(data, params) {
      trials <- copy(data)[, c(params@id, "trial", paste0("trial", params@indicator.squared), unlist(params@fixed)), with = FALSE
                                ][, .SD[1], by = c(params@id, "trial")
                                  ][, paste0(sort(unlist(params@time_varying)), params@indicator.baseline) := lapply(functions, function(f) f(.N))
                                    ][rep(seq_len(.N), each = params@followup.max + 1)
                                      ][, "followup" := seq.int(1:.N) - 1, by = c(params@id, "trial")
                                        ][, paste0("followup", params@indicator.squared) := get("followup")^2]
      
      txTRUE <- copy(trials)[, eval(tx_bas) := 1
                       ][, "prob" := inline.pred(model[[1]]$model[[i]]$model, newdata = .SD, params, type = "outcome")
                         ][, "outcome" := rbinom(.N, 1, prob)
                           ][, "firstEvent" := if (any(outcome == 1)) which(outcome == 1)[1] else .N, by = c(params@id, "trial")]
      txFALSE <- copy(trials)[, eval(tx_bas) := 0
                        ][, "prob" := inline.pred(model[[1]]$model[[i]]$model, newdata = .SD, params, type = "outcome")
                          ][, "outcome" := rbinom(.N, 1, prob)
                            ][, "firstEvent" := if (any(outcome == 1)) which(outcome == 1)[1] else .N, by = c(params@id, "trial")]
      rm(trials)
      out <- rbind(txTRUE, txFALSE)
      rm(txTRUE, txFALSE)
      data <- out[out[, .I[seq_len(firstEvent[1])], by = c(params@id, "trial", tx_bas)]$V1
                 ][, .SD[.N], by = c(params@id, "trial", tx_bas)
                   ][, firstEvent := NULL
                     ][, event := 0
                       ][outcome == 1, event := 1]
      
      if (!is.na(params@compevent)) {
        hr.data <- finegray(Surv(followup, event) ~ ., data, etype = 1)
        hr.res <- coxph(Surv(fgstart, fgstop, fgstatus) ~ get(tx_bas), data = hr.data)
      } else hr.res <- coxph(Surv(followup, event == 1) ~ get(tx_bas), data)
      exp(hr.res$coefficients)
    }
    full <- handler(params@DT, params)
    if (is.na(full)) return(c(Hazard = NA_real_, LCI = NA_real_, UCI = NA_real_))

    bootstrap <- if (params@bootstrap) {
      UIDs <- unique(params@DT[[params@id]])
      lnID <- length(UIDs)
      subDT <- subDT[, "trialID" := paste0(params@id, "_", trial)]

      if (params@parallel) {
        setDTthreads(1)
        out <- future_lapply(1:params@bootstrap.nboot, function(x) {
          id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = TRUE)
          RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) subDT[get(params@id) == id.sample[x], ]))
          handler(RMDT, params)
        })
      } else {
        out <- lapply(1:params@bootstrap.nboot, function(x) {
          id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = TRUE)
          RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) subDT[get(params@id) == id.sample[x], ]))
          handler(RMDT, params)
        })
      }
    }
    gc()
    if (params@bootstrap) {
      bootstrap <- unlist(bootstrap)
      if (all(is.na(bootstrap))) return(c(Hazard = NA_real_, LCI = NA_real_, UCI = NA_real_))
      
      se <- sd(bootstrap, na.rm = TRUE) / sqrt(sum(!is.na(bootstrap)))
      ci <- sort(c(full + se, full - se), decreasing = FALSE)
    } else {
      ci <- c(NA_real_, NA_real_)
    }
    
    out <- c(full, ci)
    names(out) <- c("Hazard", "LCI", "UCI")
    return(out)
  }
}
