#' Resolve the follow-up times at which to report risk difference / ratio
#'
#' Each requested time in \code{risk.times} is snapped to the latest available
#' follow-up value at or before it (cumulative incidence is a right-continuous
#' step function). The maximum follow-up time is always included. A \code{NA}
#' \code{risk.times} reports only the maximum follow-up time.
#'
#' @param grid numeric vector of available follow-up values
#' @param risk.times numeric vector of requested follow-up times, or \code{NA}
#' @returns sorted unique numeric vector of grid follow-up values to report at
#' @keywords internal
resolve_risk_times <- function(grid, risk.times) {
  grid <- sort(unique(grid))
  final <- max(grid)
  if (length(risk.times) == 0L || all(is.na(risk.times))) return(final)
  req <- as.numeric(risk.times)
  req <- req[!is.na(req)]
  if (any(req > final)) stop("'risk.times' value(s) exceed the maximum follow-up (", final, "): ",
                             paste(req[req > final], collapse = ", "))
  if (any(req < min(grid))) stop("'risk.times' value(s) below the minimum follow-up (", min(grid), "): ",
                                 paste(req[req < min(grid)], collapse = ", "))
  snapped <- vapply(req, function(t) max(grid[grid <= t]), numeric(1))
  sort(unique(c(snapped, final)))
}

#' Internal function to pull Risk Ratio and Risk Difference from data when \code{km.curves = TRUE}
#'
#' @keywords internal
create.risk <- function(data, params, boot_risks = NULL) {
  variable <- followup <- V1 <- V2 <- value <- NULL
  i.value <- NULL
  rr <- rd <- NULL
  boot_idx <- NULL

  var <- if (any(grepl("^inc_", data[["variable"]]))) "inc" else "risk"
  report_times <- resolve_risk_times(data[["followup"]], params@risk.times)

  table <- data[variable %like% var & followup %in% report_times, ]
  has_ci <- all(c("LCI", "UCI") %in% names(data)) && !is.null(boot_risks)

  z <- qnorm(1 - (1 - params@bootstrap.CI)/2)
  alpha <- (1 - params@bootstrap.CI) / 2

  # Build the arm-pair comparison at a single follow-up time
  compare_at <- function(t) {
    tt <- table[followup == t, list(variable, value)]
    out <- CJ(tt$variable, tt$variable)[tt, on = c("V2" = "variable")
                                        ][tt, on = c("V1" = "variable")][V1 != V2, ]
    out[, `:=`(rr = value / i.value, rd = value - i.value, followup = t)]

    if (has_ci) {
      # Paired bootstrap: pair both arms by boot_idx at this follow-up time
      boot_wide <- dcast(boot_risks[followup == t], boot_idx ~ variable, value.var = "value")
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
      out[, `:=`(rd_lci = rd_lci_vec, rd_uci = rd_uci_vec,
                 rr_lci = rr_lci_vec, rr_uci = rr_uci_vec)]
    }
    out
  }

  out <- rbindlist(lapply(report_times, compare_at))

  if (has_ci) {
    out[, `:=`(value = NULL, i.value = NULL)]
    setcolorder(out, c("followup", "V1", "V2", "rr", "rr_lci", "rr_uci", "rd", "rd_lci", "rd_uci"))
    setnames(out, c("followup", "V1", "V2", "rr", "rr_lci", "rr_uci", "rd", "rd_lci", "rd_uci"),
                  c("Followup", "A_x", "A_y", "Risk Ratio", "RR 95% LCI", "RR 95% UCI",
                    "Risk Difference", "RD 95% LCI", "RD 95% UCI"))
  } else {
    out[, `:=`(value = NULL, i.value = NULL)]
    setcolorder(out, c("followup", "V1", "V2", "rr", "rd"))
    setnames(out, c("followup", "V1", "V2", "rr", "rd"),
                  c("Followup", "A_x", "A_y", "Risk Ratio", "Risk Difference"))
  }

  # Per-arm risk table
  table[, `:=`(A = sub(".*_", "", variable), Method = params@method, variable = NULL)]
  if (has_ci) {
    setnames(table, c("followup", "value", "LCI", "UCI"), c("Followup", "Risk", "95% LCI", "95% UCI"))
    setcolorder(table, c("Method", "Followup", "A", "Risk", "95% LCI", "95% UCI"))
  } else {
    setnames(table, c("followup", "value"), c("Followup", "Risk"))
    setcolorder(table, c("Method", "Followup", "A", "Risk"))
  }

  return(list(risk.comparison = out, risk.data = table))
}

factorize <- function(data, params) {
  # Fixed covariates and treatment columns are always treated as categorical.
  encodes <- unlist(c(params@fixed, paste0(params@treatment, params@indicator.baseline),
                      params@treatment))
  coercion <- encodes[encodes %in% names(data)]
  if (length(coercion) > 0) data[, (coercion) := lapply(.SD, as.factor), .SDcols = coercion]

  # Categorical (character) time-varying covariates - and their baseline (_bas)
  # counterparts - must also get a stable factor encoding, with levels fixed from
  # the full data, so that bootstrap resamples cannot realise different level sets
  # and produce model matrices that differ in their columns between fit and
  # prediction (which raises "newdata provided does not match fitted model").
  # Numeric time-varying covariates are left untouched so continuous covariates
  # are not turned into factors.
  tv <- unlist(params@time_varying)
  tv <- unique(c(tv, paste0(tv, params@indicator.baseline)))
  tv <- tv[tv %in% names(data)]
  tv_cat <- tv[vapply(tv, function(col) is.character(data[[col]]), logical(1))]
  if (length(tv_cat) > 0) data[, (tv_cat) := lapply(.SD, as.factor), .SDcols = tv_cat]

  return(data)
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

#' Count follow-up per treatment arm
#'
#' Summarises follow-up in each treatment arm, grouped by the baseline treatment
#' value, restricted to expanded-data rows with an observed (non-`NA`) outcome -
#' the person-time the outcome/survival model is fit on. Rows censored under
#' \code{method = "censoring"} (which carry \code{outcome = NA}) are excluded,
#' matching the analysis.
#'
#' \code{type = "nonunique"} counts follow-up intervals (rows), i.e. total
#' person-time; dividing the non-unique outcome counts by these gives the per-arm
#' event rate. \code{type = "unique"} counts the distinct subjects contributing
#' follow-up to the arm (a subject contributing several trials to the same arm is
#' counted once; a subject who appears in both arms is counted in each).
#'
#' @param params SEQparams object (uses the expanded \code{params@DT})
#' @param type either \code{"unique"} (distinct subjects) or \code{"nonunique"} (intervals)
#' @param filter subgroup value to restrict to, or \code{NA} for no subgroup
#' @returns data.table with the baseline-treatment column and \code{n} (count)
#' @keywords internal
followup.table <- function(params, type, filter = NA) {
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  dt <- params@DT[!is.na(get(params@outcome)), ]
  if (!is.na(params@subgroup)) dt <- dt[get(params@subgroup) == filter, ]
  out <- if (type == "unique") {
    dt[, list(n = uniqueN(get(params@id))), by = c(tx_bas)]
  } else {
    dt[, list(n = .N), by = c(tx_bas)]
  }
  setorderv(out, tx_bas)
  return(out)
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
