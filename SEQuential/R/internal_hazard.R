#' Generic function to format a dataset for hazard ratio calculation
#' @param data Data table containing survival data
#' @param params SEQuential params
#' @param type Hazard ratio calculation method: "cox" (cause-specific) or "finegray" (competing risks)
#' 
#' @keywords internal
#' @import data.table
#' @importFrom survival finegray coxph Surv

hazard <- function(data, params, type = "cox") {
  kept <- c("period", "trial", params@id, params@outcome, params@compevent)
  subDT <- data[, kept, with = FALSE
                ][, .SD[.N], by = c("period", "trial", params@id)
                  ][, event := 0][get(params@outcome) == 1, event := 1]

  if (method == "finegray") {
    hr.data <- finegray(Surv(period, event) ~ ., subDT[get(params@compevent) == 1, event := 2], etype = "Y")
    hr.res <- coxph(Surv(fgstart, fgstop, fgstatus) ~ regime, data = hr_data)
  } else {
    hr.res <- coxph(Surv(t0, event == 1) ~ regime, subDT)
  }
  
  return(exp(hr.res$coefficients))
}
