#' Generic function to format a dataset for hazard ratio calculation
#' @param params SEQuential params
#' 
#' @keywords internal
#' @import data.table
#' @importFrom survival finegray coxph Surv

hazard <- function(params) {
  event <- trial <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  kept <- c("followup", "trial", tx_bas, params@id, params@outcome, params@compevent)
  kept <- kept[kept %in% names(params@DT)]
  subDT <- copy(params@DT)[, kept, with = FALSE
                           ][, .SD[.N], by = c("followup", "trial", params@id)
                             ][, event := 0
                               ][get(params@outcome) == 1, event := 1]
  if (!is.na(params@compevent)) subDT <- subDT[get(params@compevent) == 1, event := 2]
  
  handler <- function(data) {
    if (!is.na(params@compevent)) {
      hr.data <- finegray(Surv(followup, event) ~ ., data, etype = 2)
      hr.res <- coxph(Surv(fgstart, fgstop, fgstatus) ~ get(tx_bas), data = hr.data)
    } else {
      hr.res <- coxph(Surv(followup, event == 1) ~ get(tx_bas), data)
    }
    
    return(exp(hr.res$coefficients))
  }
  
  full <- handler(subDT)
  
  bootstrap <- if (params@bootstrap) {
    UIDs <- unique(params@DT[[params@id]])
    lnID <- length(UIDs)
    subDT <- subDT[, "trialID" := paste0(params@id, "_", trial)]
    
    if (params@parallel) {
      setDTthreads(1)
      out <- future_lapply(1:params@bootstrap.nboot, function(x) {
        id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = TRUE)
        RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) subDT[get(params@id) == id.sample[x],]))
        
        handler(RMDT)
      })
    } else 
      out <- lapply(1:params@bootstrap.nboot, function(x) {
        id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = TRUE)
        RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) subDT[get(params@id) == id.sample[x],]))
        
        handler(RMDT)
      })
  }
  gc()
  if (params@bootstrap) {
    se <- sd(unlist(bootstrap)) / sqrt(params@bootstrap.nboot)
    ci <- sort(c(full + se, full - se), decreasing = FALSE)
  } else ci <- c(NA_real_, NA_real_)
  out <- c(full, ci)
  names(out) <- c("Hazard", "LCI", "UCI") 
  }
