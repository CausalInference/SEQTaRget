#' Internal function to pull Risk Ratio and Risk Difference from data when \code{km.curves = TRUE}
#'
#' @keywords internal
create.risk <- function(data, params) {
  variable <- followup <- V1 <- V2 <- NULL
  i.value <- i.LCI <- i.UCI <- NULL
  UCI <- LCI <- NULL
  rd_lb <- rd_ub <- rr_lb <- rr_ub <- NULL
  var <- if ("inc0" %in% data[["variable"]]) "inc" else "risk"
  table <- data[, .SD[.N], by = "variable"
                ][variable %like% var, 
                  ][, followup := NULL]
  
  out <- CJ(table$variable, table$variable)[table, on = c("V2" = "variable")
                                            ][table, on = c("V1" = "variable")][V1 != V2, ]
  
  out[, `:=` (rr = value / i.value, rd = value - i.value)
      ][, `:=` (value = NULL, i.value = NULL)]
  
  
  table[, `:=` (A = sub(".*_", "", variable), 
                Method = params@method,
                variable = NULL)]
  
  if (all(c("LCI", "UCI") %in% names(out))) {
    out[, `:=` (
      rd_lb = LCI - i.LCI,
      rd_ub = UCI - i.UCI,
      rr_lb = LCI / i.LCI,
      rr_ub = UCI / i.UCI
      )][, `:=` (
      rd_lci = pmin(rd_lb, rd_ub),
      rd_uci = pmax(rd_lb, rd_ub),
      rr_lci = pmin(rr_lb, rr_ub),
      rr_uci = pmax(rr_lb, rr_ub))
      ][, `:=` (LCI = NULL, UCI = NULL, i.LCI = NULL, i.UCI = NULL,
                rd_lb = NULL, rd_ub = NULL, rr_lb = NULL, rr_ub = NULL)]
    
    setnames(out, names(out), c("A_x", "A_y", 
                                "Risk Ratio", "Risk Differerence",
                                "RD 95% LCI", "RD 95% UCI", "RR 95% LCI", "RR 95% UCI"))
    setcolorder(out, c("A_x", "A_y", "Risk Ratio", "RR 95% LCI", "RR 95% UCI",
                                     "Risk Differerence", "RD 95% LCI", "RD 95% UCI"))
    
    setnames(table, c("value", "LCI", "UCI"), c("Risk", "95% LCI", "95% UCI"))
    setcolorder(table, c("Method", "A", "Risk", "95% LCI", "95% UCI"))
  } else {
    setnames(out, names(out), c("A_x", "A_y", "Risk Ratio", "Risk Difference"))
    setnames(table, "value", "Risk")
    setcolorder(table, c("Method", "A", "Risk"))
    
  }
  return(list(risk.comparison = out, risk.data = table))
}

factorize <- function(data, params) {
  encodes <- unlist(c(params@fixed, paste0(params@treatment, params@indicator.baseline),
                      params@treatment))
  coercion <- encodes[encodes %in% names(data)]
  
  out <- data[, (coercion) := lapply(.SD, as.factor), .SDcols = coercion]
  return(out)
}

#' Nicely cleans time for readability
#'
#' @keywords internal
format.time <- function(seconds) {
  if (seconds < 60) {
    paste0(round(seconds, 2), " seconds")
  } else if (seconds < 3600) {
    minutes <- floor(seconds / 60)
    remaining_seconds <- seconds %% 60
    paste0(minutes, " minute", ifelse(minutes > 1, "s", ""),
           " ", round(remaining_seconds, 2), " second", ifelse(remaining_seconds > 1, "s", ""))
  } else {
    hours <- floor(seconds / 3600)
    remaining_seconds <- seconds %% 3600
    minutes <- floor(remaining_seconds / 60)
    seconds <- remaining_seconds %% 60
    paste0(hours, " hour", ifelse(hours > 1, "s", ""),
           " ", minutes, " minute", ifelse(minutes > 1, "s", ""),
           " ", round(seconds, 2), " second", ifelse(seconds > 1, "s", ""))
  }
}

allNA <- function(x) {
  all(sapply(x, function(y) is.na(y)))
}

equalizer <- function(list, levels) {
  if (length(list) < length(levels)) list <- c(list, rep(NA, length(levels) - length(list)))
  return(list)
}

outcome.table <- function(type, params, filter = NA) {
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  
  if (is.na(params@subgroup)) {
    out <- if (type == "unique") {
      copy(params@DT)[get(params@outcome) == 1, .SD[1], 
                      by = c(params@id, tx_bas, params@outcome)
                      ][, list(n = .N), by = c(tx_bas, params@outcome)] 
    } else {
      copy(params@DT)[get(params@outcome) == 1, list(n = .N), 
                      by = c(tx_bas, params@outcome)]
    }
  } else {
    out <- if (type == "unique") {
      copy(params@DT)[get(params@outcome) == 1 & params@subgroup == filter, 
                      .SD[1], by = c(params@id, tx_bas, params@outcome)] 
    } else {
      copy(params@DT)[get(params@outcome) == 1 & params@subgroup == filter, 
                      list(n = .N), by = c(tx_bas, params@outcome)]
    }
  }
  return(out)
}
