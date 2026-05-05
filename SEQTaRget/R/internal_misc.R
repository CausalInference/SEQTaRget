#' Internal function to pull Risk Ratio and Risk Difference from data when \code{km.curves = TRUE}
#'
#' @keywords internal
create.risk <- function(data, params, boot_risks = NULL) {
  variable <- followup <- V1 <- V2 <- NULL
  i.value <- i.LCI <- i.UCI <- i.SE <- NULL
  UCI <- LCI <- SE <- NULL
  rd_lb <- rd_ub <- rr_lb <- rr_ub <- NULL
  rd <- rd_se <- rr <- rr_se <- NULL
  boot_idx <- boot_wide <- NULL
  
  var <- if (any(grepl("^inc_", data[["variable"]]))) "inc" else "risk"
  table <- data[, .SD[.N], by = "variable"
                ][variable %like% var, 
                  ][, followup := NULL]
  
  out <- CJ(table$variable, table$variable)[table, on = c("V2" = "variable")
                                            ][table, on = c("V1" = "variable")][V1 != V2, ]
  
  out[, `:=` (rr = value / i.value, rd = value - i.value)]
  
  table[, `:=` (A = sub(".*_", "", variable), 
                Method = params@method,
                variable = NULL)]
  
  if (all(c("LCI", "UCI") %in% names(out))) {
    z <- qnorm(1 - (1 - params@bootstrap.CI)/2)
    alpha <- (1 - params@bootstrap.CI) / 2

    # Paired bootstrap: pair both arms by boot_idx and compute per-iteration
    # RD and RR so that the shared bootstrap samples are accounted for
    boot_wide <- dcast(boot_risks, boot_idx ~ variable, value.var = "value")

    rd_lci_vec <- rd_uci_vec <- rr_lci_vec <- rr_uci_vec <- numeric(nrow(out))
    for (k in seq_len(nrow(out))) {
      v1 <- as.character(out$V1[k]); v2 <- as.character(out$V2[k])
      rd_i  <- boot_wide[[v2]] - boot_wide[[v1]]
      rr_i  <- boot_wide[[v2]] / boot_wide[[v1]]
      valid_rr <- rr_i[rr_i > 0 & is.finite(rr_i)]
      if (params@bootstrap.CI_method == "se") {
        rd_lci_vec[k] <- out$rd[k] - z * sd(rd_i, na.rm = TRUE)
        rd_uci_vec[k] <- out$rd[k] + z * sd(rd_i, na.rm = TRUE)
        rr_log_se      <- sd(log(valid_rr), na.rm = TRUE)
        rr_lci_vec[k] <- exp(log(out$rr[k]) - z * rr_log_se)
        rr_uci_vec[k] <- exp(log(out$rr[k]) + z * rr_log_se)
      } else {
        rd_lci_vec[k] <- quantile(rd_i,      alpha,     na.rm = TRUE)
        rd_uci_vec[k] <- quantile(rd_i,      1 - alpha, na.rm = TRUE)
        rr_lci_vec[k] <- quantile(valid_rr,  alpha,     na.rm = TRUE)
        rr_uci_vec[k] <- quantile(valid_rr,  1 - alpha, na.rm = TRUE)
      }
    }
    rm(boot_wide)
    out[, `:=` (rd_lci = rd_lci_vec, rd_uci = rd_uci_vec,
                rr_lci = rr_lci_vec, rr_uci = rr_uci_vec)
        ][, `:=` (value = NULL, i.value = NULL, LCI = NULL, UCI = NULL,
                  i.LCI = NULL, i.UCI = NULL, SE = NULL, i.SE = NULL)]
    setnames(out, names(out), c("A_x", "A_y", 
                                "Risk Ratio", "Risk Differerence",
                                "RD 95% LCI", "RD 95% UCI", "RR 95% LCI", "RR 95% UCI"))
    setcolorder(out, c("A_x", "A_y", "Risk Ratio", "RR 95% LCI", "RR 95% UCI",
                                     "Risk Differerence", "RD 95% LCI", "RD 95% UCI"))
    
    setnames(table, c("value", "LCI", "UCI"), c("Risk", "95% LCI", "95% UCI"))
    setcolorder(table, c("Method", "A", "Risk", "95% LCI", "95% UCI"))
  } else {
    out[, `:=` (value = NULL, i.value = NULL)]
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
format_time <- function(seconds) {
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
  all(is.na(x))
}

#' Bake fixed knots into any `ns(followup, df = N)` term in `params@covariates`
#'
#' `splines::ns()` recomputes knots from whatever data it sees, so without fixed
#' knots the basis at prediction time differs from the basis used at fit time.
#' This helper rewrites every `ns(followup, df = N)` token in
#' `params@covariates` to an explicit `ns(followup, knots = c(...), Boundary.knots = c(...))`
#' computed once from the full expanded `followup` column. The result is a
#' formula whose `model.matrix` output is invariant to the row subset passed in.
#'
#' Returns the original covariates string unchanged when no `ns(followup, df = ...)`
#' token is present (e.g. user supplied custom covariates that already specify
#' knots).
#'
#' @keywords internal
bake_followup_spline <- function(params) {
  covs <- params@covariates
  if (is.null(covs) || is.na(covs) || !nzchar(covs)) return(covs)
  re <- "ns\\(\\s*followup\\s*,\\s*df\\s*=\\s*(\\d+)\\s*\\)"
  if (!grepl(re, covs)) return(covs)

  fu <- params@DT[["followup"]]
  if (is.null(fu) || length(fu) == 0L) return(covs)
  bks <- range(fu, na.rm = TRUE)

  m <- regmatches(covs, regexpr(re, covs))
  df <- as.integer(sub(re, "\\1", m))

  probs <- if (df >= 2L) seq(0, 1, length.out = df + 1L)[-c(1L, df + 1L)] else numeric(0)
  knots <- if (length(probs) > 0L) as.numeric(quantile(fu, probs, names = FALSE, na.rm = TRUE)) else numeric(0)

  replacement <- if (length(knots) == 0L) {
    sprintf("ns(followup, df = 1, Boundary.knots = c(%s, %s))",
            format(bks[1], digits = 15), format(bks[2], digits = 15))
  } else {
    sprintf("ns(followup, knots = c(%s), Boundary.knots = c(%s, %s))",
            paste(format(knots, digits = 15), collapse = ", "),
            format(bks[1], digits = 15), format(bks[2], digits = 15))
  }
  gsub(re, replacement, covs)
}

#' Extract underlying column names from RHS formula strings
#'
#' Wraps `all.vars(as.formula(...))` so that function-wrapped terms in
#' formula strings (e.g. `ns(followup, df = 4)`, `I(x^2)`, `factor(grp)`)
#' resolve to their underlying variable names rather than being treated
#' as raw column names.
#'
#' @param x character vector of RHS formula strings (may contain `+`, `*`, `:`,
#'   and function wrappers). NA / empty entries are ignored.
#'
#' @returns character vector of unique variable names referenced by `x`.
#' @keywords internal
formula_vars <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) return(character(0))
  unique(unlist(lapply(x, function(s) all.vars(stats::as.formula(paste0("~", s))))))
}

equalizer <- function(list, levels) {
  if (length(list) < length(levels)) list <- c(list, rep(NA, length(levels) - length(list)))
  return(list)
}

outcome.table <- function(params, type, filter = NA) {
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  
  if (is.na(params@subgroup)) {
    out <- if (type == "unique") {
      params@DT[get(params@outcome) == 1, .SD[1],
                by = c(params@id, tx_bas, params@outcome)
                ][, list(n = .N), by = c(tx_bas, params@outcome)]
    } else {
      params@DT[get(params@outcome) == 1, list(n = .N),
                by = c(tx_bas, params@outcome)]
    }
  } else {
    out <- if (type == "unique") {
      params@DT[get(params@outcome) == 1 & get(params@subgroup) == filter,
                .SD[1], by = c(params@id, tx_bas, params@outcome)
                ][, list(n = .N), by = c(tx_bas, params@outcome)]
    } else {
      params@DT[get(params@outcome) == 1 & get(params@subgroup) == filter,
                list(n = .N), by = c(tx_bas, params@outcome)]
    }
  }
  return(out)
}
