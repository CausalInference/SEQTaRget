#' Select the end-of-follow-up outcome measurement for each trial-period
#'
#' An end-of-follow-up outcome is measured once, at follow-up time
#' \code{end_of_fup.time} (\code{k}), rather than as a time-to-event. For each
#' (id, trial) this returns the single row the estimate is read from:
#' the measurement at exactly \code{k} when one exists, otherwise - if
#' \code{end_of_fup.window} is non-zero - the measurement nearest to \code{k}
#' within \code{[k - window, k + window]}, with ties (measurements equally far
#' either side of \code{k}) broken toward the later one, so that at least
#' \code{k} of follow-up has elapsed. Trial-periods with no
#' measurement anywhere in the window contribute no row, i.e. they are censored
#' out of the estimate.
#'
#' Rows carrying a missing outcome are not measurements: under
#' \code{method = "censoring"} these are the artificially censored (treatment
#' switch) rows, so a subject who deviates before \code{k} is correctly excluded
#' rather than contributing a carried-forward value.
#'
#' @param DT expanded data.table, weighted (carrying a \code{weight} column) or not
#' @param params SEQparams object
#' @returns data.table with one row per contributing (id, trial): the outcome
#'   value, the weight at that time, the baseline treatment arm, and the
#'   follow-up time the measurement was taken at
#' @import data.table
#' @keywords internal
endoffup.measure <- function(DT, params) {
  followup <- weight <- .dist <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  k <- params@end_of_fup.time
  w <- params@end_of_fup.window

  cols <- unique(c(params@id, "trial", "followup", tx_bas, params@outcome,
                   if (params@subgroup %in% names(DT)) params@subgroup,
                   if ("weight" %in% names(DT)) "weight"))
  candidates <- DT[!is.na(get(params@outcome)) & followup >= k - w & followup <= k + w,
                   cols, with = FALSE]
  if (nrow(candidates) == 0L) return(candidates[, "eof.value" := numeric(0)])

  # Take the measurement nearest to k. The exact-k measurement has distance 0 so
  # it always wins where one exists; ordering by (distance, descending followup)
  # and taking the first row per trial-period breaks equidistant ties - one
  # measurement the same number of periods either side of k - toward the later
  # measurement, so the trial-period has at least k of follow-up elapsed.
  candidates[, .dist := abs(followup - k)]
  setorderv(candidates, c(params@id, "trial", ".dist", "followup"), order = c(1L, 1L, 1L, -1L))
  out <- candidates[candidates[, .I[1L], by = c(params@id, "trial")]$V1
                    ][, .dist := NULL]

  # An unweighted analysis is the equally-weighted average of the same values
  if (!"weight" %in% names(out)) out[, weight := 1]
  setnames(out, params@outcome, "eof.value")
  return(out[])
}

#' Weighted end-of-follow-up average within each treatment arm
#'
#' Weight truncation (\code{weight.lower} / \code{weight.upper}, including the
#' bounds \code{weight.p99} resolves to) is applied here as it is for the
#' outcome model, since this average is the estimator in \code{end_of_fup} mode.
#'
#' Alongside the estimate this counts the trial-periods excluded for want of a
#' measurement in the window, so the share of eligible trial-periods dropped can
#' be reported next to the estimate they were dropped from. Trial-periods are the
#' denominator because they partition cleanly, whereas one subject can contribute
#' one trial and be excluded in another.
#'
#' @param DT expanded data.table for one bootstrap iteration
#' @param params SEQparams object
#' @returns named list of per-arm data.tables, one element per subgroup
#' @import data.table
#' @keywords internal
endoffup.estimate <- function(DT, params) {
  weight <- eof.value <- n <- n.excluded <- Eligible <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  measured <- endoffup.measure(DT, params)

  # One row per trial-period: the eligible total the contributors are drawn from,
  # counted over the same data as endoffup.counts() so the two agree.
  has_sub <- !is.na(params@subgroup) && params@subgroup %in% names(DT)
  periods <- unique(DT[, c(params@id, "trial", tx_bas,
                           if (has_sub) params@subgroup), with = FALSE])

  arm_average <- function(dt, elig) {
    if (nrow(dt) == 0L) return(data.table())
    dt <- copy(dt)[weight < params@weight.lower, weight := params@weight.lower
                   ][weight > params@weight.upper, weight := params@weight.upper]
    out <- dt[, list(estimate = sum(weight * eof.value) / sum(weight),
                     n = .N,
                     n.subjects = uniqueN(get(params@id))),
              by = c(tx_bas)]
    out <- out[elig[, list(Eligible = .N), by = c(tx_bas)], on = tx_bas, nomatch = NULL
               ][, n.excluded := Eligible - n][, Eligible := NULL]
    setorderv(out, tx_bas)
    out[]
  }

  if (is.na(params@subgroup)) return(list(arm_average(measured, periods)))

  groups <- sort(unique(DT[[params@subgroup]]))
  out <- lapply(groups, function(g) arm_average(measured[get(params@subgroup) == g, ],
                                                periods[get(params@subgroup) == g, ]))
  names(out) <- paste0(params@subgroup, "_", groups)
  return(out)
}

#' Assemble end-of-follow-up estimates and bootstrap confidence intervals
#'
#' Mirrors [create.risk()]: \code{eof.data} holds the per-arm estimate and
#' \code{eof.comparison} the pairwise between-arm contrasts, both with bootstrap
#' confidence intervals when \code{bootstrap = TRUE}. Contrasts are paired by
#' bootstrap iteration, so the interval accounts for the correlation between arms.
#'
#' @param full per-arm estimates from the full-data fit
#' @param boots list of per-arm estimates, one per bootstrap iteration
#' @param params SEQparams object
#' @returns list with \code{eof.data} and \code{eof.comparison} data.tables
#' @importFrom stats qnorm quantile sd
#' @import data.table
#' @keywords internal
create.endoffup <- function(full, boots, params) {
  estimate <- boot_idx <- V1 <- V2 <- i.estimate <- diff_ <- ratio <- NULL
  SE <- LCI <- UCI <- Time <- ratio_logse <- ratio_lci <- ratio_uci <- NULL
  n <- n.excluded <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  ci_lab <- paste0(format(params@bootstrap.CI * 100, trim = TRUE), "%")
  z <- qnorm(1 - (1 - params@bootstrap.CI) / 2)
  alpha <- (1 - params@bootstrap.CI) / 2
  use_se <- params@bootstrap.CI_method == "se"

  boots <- Filter(function(x) is.data.table(x) && nrow(x) > 0, boots)
  has_ci <- length(boots) > 1L
  boot_all <- if (has_ci) rbindlist(lapply(seq_along(boots), function(i) copy(boots[[i]])[, boot_idx := i])) else NULL

  data <- copy(full)
  if (has_ci) {
    se <- boot_all[, list(SE = sd(estimate, na.rm = TRUE)), by = c(tx_bas)]
    data <- data[se, on = tx_bas]
    if (use_se) {
      data[, `:=`(LCI = estimate - z * SE, UCI = estimate + z * SE)]
    } else {
      q <- boot_all[, list(LCI = quantile(estimate, alpha, na.rm = TRUE),
                           UCI = quantile(estimate, 1 - alpha, na.rm = TRUE)), by = c(tx_bas)]
      data <- data[q, on = tx_bas]
    }
    # A binary outcome is a proportion, so clamp the normal-approximation
    # interval to [0, 1]; a continuous outcome has no such bound.
    if (params@end_of_fup.type == "binary") data[, `:=`(LCI = pmax(0, LCI), UCI = pmin(1, UCI))]
  }

  # Pairwise between-arm contrasts, both directions (as in create.risk())
  arms <- full[[tx_bas]]
  pairs <- CJ(V1 = arms, V2 = arms)[V1 != V2, ]
  comparison <- if (nrow(pairs) == 0L) data.table() else {
    est <- setNames(full$estimate, as.character(arms))
    pairs[, `:=`(diff_ = est[as.character(V2)] - est[as.character(V1)],
                 ratio = est[as.character(V2)] / est[as.character(V1)])]
    if (has_ci) {
      wide <- dcast(boot_all, boot_idx ~ get(tx_bas), value.var = "estimate")
      blank <- list(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
      stat <- lapply(seq_len(nrow(pairs)), function(i) {
        v1 <- as.character(pairs$V1[i]); v2 <- as.character(pairs$V2[i])
        if (!all(c(v1, v2) %in% names(wide))) return(blank)
        d <- wide[[v2]] - wide[[v1]]
        d_se <- sd(d, na.rm = TRUE)
        # As in create.risk(), a ratio is summarised on the log scale - the scale
        # on which ratio measures are pooled - and only where it is defined, so
        # non-positive bootstrap estimates are dropped rather than yielding NaN.
        r <- wide[[v2]] / wide[[v1]]
        r_valid <- r[is.finite(r) & r > 0]
        r_logse <- if (length(r_valid) > 1L) sd(log(r_valid), na.rm = TRUE) else NA_real_
        if (use_se) {
          list(d_se, pairs$diff_[i] - z * d_se, pairs$diff_[i] + z * d_se,
               r_logse, exp(log(pairs$ratio[i]) - z * r_logse), exp(log(pairs$ratio[i]) + z * r_logse))
        } else {
          list(d_se, quantile(d, alpha, na.rm = TRUE), quantile(d, 1 - alpha, na.rm = TRUE),
               r_logse,
               if (length(r_valid) > 1L) quantile(r_valid, alpha, na.rm = TRUE) else NA_real_,
               if (length(r_valid) > 1L) quantile(r_valid, 1 - alpha, na.rm = TRUE) else NA_real_)
        }
      })
      pull <- function(j) vapply(stat, function(x) as.numeric(x[[j]]), numeric(1))
      pairs[, `:=`(SE = pull(1), LCI = pull(2), UCI = pull(3),
                   ratio_logse = pull(4), ratio_lci = pull(5), ratio_uci = pull(6))]
    }
    pairs
  }

  label <- if (params@end_of_fup.type == "binary") "Proportion" else "Mean"
  # Share of eligible trial-periods dropped for want of a measurement in the
  # window, reported beside the estimate they were dropped from.
  if (all(c("n", "n.excluded") %in% names(data))) {
    data[, "% Excluded" := 100 * n.excluded / (n + n.excluded)]
  }
  setnames(data, c(tx_bas, "estimate", "n", "n.subjects", "n.excluded"),
           c("A", label, "Trial-periods", "Subjects", "Excluded"), skip_absent = TRUE)
  if (has_ci) setnames(data, c("LCI", "UCI"), paste0(ci_lab, c(" LCI", " UCI")), skip_absent = TRUE)
  data[, `:=`(Time = params@end_of_fup.time, Type = params@end_of_fup.type)]
  setcolorder(data, intersect(c("Type", "Time", "A", label, "Trial-periods", "Subjects",
                                "Excluded", "% Excluded"), names(data)))

  if (nrow(comparison) > 0L) {
    # A ratio of means is only interpretable when the outcome is bounded away
    # from zero, which a continuous outcome need not be (it may even be
    # negative, leaving log(ratio) undefined), so it is reported for proportions
    # only. The difference is the contrast that always applies.
    ratio_cols <- c("Ratio", paste0("Ratio ", ci_lab, c(" LCI", " UCI")), "log(Ratio) SE")
    setnames(comparison, c("V1", "V2", "diff_", "ratio"), c("A_x", "A_y", "Difference", "Ratio"))
    if (has_ci) setnames(comparison,
                         c("LCI", "UCI", "SE", "ratio_lci", "ratio_uci", "ratio_logse"),
                         c(paste0("Difference ", ci_lab, c(" LCI", " UCI")), "Difference SE",
                           ratio_cols[2], ratio_cols[3], ratio_cols[4]), skip_absent = TRUE)
    comparison[, Time := params@end_of_fup.time]
    if (params@end_of_fup.type != "binary") {
      drop <- intersect(ratio_cols, names(comparison))
      if (length(drop) > 0L) comparison[, (drop) := NULL]
    }
    lead <- c("Time", "A_x", "A_y", "Difference",
              intersect(paste0("Difference ", ci_lab, c(" LCI", " UCI")), names(comparison)),
              intersect("Difference SE", names(comparison)),
              intersect(ratio_cols, names(comparison)))
    setcolorder(comparison, lead)
  }

  return(list(eof.data = data[], eof.comparison = comparison[]))
}

#' Account for every trial-period at the end-of-follow-up time
#'
#' Classifies each trial-period in the analysis data into exactly one of four
#' mutually exclusive categories, so that the trial-periods contributing to the
#' estimate can be reconciled against those excluded:
#' \itemize{
#'   \item measured at \code{k} - contributes, using the measurement at exactly
#'     \code{end_of_fup.time};
#'   \item measured in the window - contributes, having no measurement at
#'     \code{k} but one within \code{[k - window, k + window]};
#'   \item excluded, outside the window - has a measurement somewhere, but none
#'     within the window;
#'   \item excluded, no measurement - has no non-missing outcome at any
#'     follow-up time. Under \code{method = "censoring"} this includes
#'     trial-periods artificially censored before any measurement was taken.
#' }
#'
#' Counted over the same data the estimate is computed from, so the first two
#' categories always sum to the contributing trial-periods reported by
#' [end_of_fup()].
#'
#' @param DT expanded data.table, weighted or not - as passed to [endoffup.estimate()]
#' @param params SEQparams object
#' @param type either \code{"nonunique"} (trial-periods) or \code{"unique"}
#'   (distinct subjects). Trial-period counts are mutually exclusive and so sum
#'   to \code{Eligible}; subject counts need not, since one subject can fall into
#'   different categories for different trials
#' @returns named list of data.tables, one element per subgroup, each with a row
#'   per baseline treatment arm
#' @import data.table
#' @keywords internal
endoffup.counts <- function(DT, params, type) {
  followup <- .category <- N <- at.k <- in.window <- measured <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  k <- params@end_of_fup.time
  w <- params@end_of_fup.window
  by_cols <- c(params@id, "trial", tx_bas,
               if (!is.na(params@subgroup) && params@subgroup %in% names(DT)) params@subgroup)

  # One row per trial-period, flagging what it has available
  flags <- DT[, list(at.k = any(!is.na(get(params@outcome)) & followup == k),
                     in.window = any(!is.na(get(params@outcome)) &
                                       followup >= k - w & followup <= k + w),
                     measured = any(!is.na(get(params@outcome)))),
              by = by_cols]
  flags[, .category := fifelse(at.k, "At k",
                        fifelse(in.window, "In window",
                         fifelse(measured, "Excluded (outside window)",
                                           "Excluded (no measurement)")))]

  levels <- c("At k", "In window", "Excluded (outside window)", "Excluded (no measurement)")
  tabulate <- function(dt) {
    if (nrow(dt) == 0L) return(data.table())
    counted <- if (type == "unique") {
      dt[, list(N = uniqueN(get(params@id))), by = c(tx_bas, ".category")]
    } else {
      dt[, list(N = .N), by = c(tx_bas, ".category")]
    }
    out <- dcast(counted, get(tx_bas) ~ .category, value.var = "N", fill = 0L)
    setnames(out, "tx_bas", tx_bas, skip_absent = TRUE)
    for (lv in levels) if (!lv %in% names(out)) out[, (lv) := 0L]
    # Eligible is the total considered; for trial-periods the four categories
    # partition it, for subjects they may overlap so it is counted directly.
    total <- if (type == "unique") {
      dt[, list(Eligible = uniqueN(get(params@id))), by = c(tx_bas)]
    } else {
      dt[, list(Eligible = .N), by = c(tx_bas)]
    }
    setnames(out, names(out)[1], tx_bas)
    out <- out[total, on = tx_bas]
    setcolorder(out, c(tx_bas, "Eligible", levels))
    setorderv(out, tx_bas)
    out[]
  }

  if (is.na(params@subgroup)) return(list(tabulate(flags)))
  groups <- sort(unique(DT[[params@subgroup]]))
  out <- lapply(groups, function(g) tabulate(flags[get(params@subgroup) == g, ]))
  names(out) <- paste0(params@subgroup, "_", groups)
  return(out)
}
