#' Generic function to format a dataset for hazard ratio calculation
#' @param data expanded data
#' @param params SEQuential params
#'
#' @keywords internal
#' @import data.table
#' @importFrom survival finegray coxph Surv

hazard <- function(data, params) {
  event <- trial <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  kept <- c("followup", "trial", tx_bas, params@id, params@outcome, params@compevent)
  kept <- kept[kept %in% names(params@DT)]
  subDT <- copy(data)[, kept, with = FALSE][, .SD[.N], by = c("trial", params@id)
                                            ][, event := 0
                                              ][get(params@outcome) == 1, event := 1]
  if (!is.na(params@compevent)) subDT <- subDT[get(params@compevent) == 1, event := 2]
  subDT <- subDT[, event := as.factor(event)]

  handler <- function(data) {
    tryCatch({
      if (!is.na(params@compevent)) {
        hr.data <- finegray(Surv(followup, event) ~ ., data, etype = 1)
        hr.res <- coxph(Surv(fgstart, fgstop, fgstatus) ~ get(tx_bas), data = hr.data, ties="breslow")
      } else {
        hr.res <- coxph(Surv(followup, event == "1") ~ get(tx_bas), data, ties="breslow")
      }
      exp(hr.res$coefficients)
    }, error = function(e) {
      warning("Cox model failed to converge: ", conditionMessage(e))
      NA_real_
    })
  }

  full <- handler(subDT)
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
        handler(RMDT)
      })
    } else {
      out <- lapply(1:params@bootstrap.nboot, function(x) {
        id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = TRUE)
        RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) subDT[get(params@id) == id.sample[x], ]))
        handler(RMDT)
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

