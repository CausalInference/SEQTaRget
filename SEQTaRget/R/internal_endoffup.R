#' Select the end-of-follow-up outcome measurement for each trial-period
#'
#' An end-of-follow-up outcome is measured once, at follow-up time
#' \code{end_of_fup.time} (\code{k}), rather than as a time-to-event. For each
#' (id, trial) this returns the single row the estimate is read from:
#' the measurement at exactly \code{k} when one exists, otherwise - if
#' \code{end_of_fup.window} is non-zero - the earliest available measurement in
#' \code{[k - window, k + window]}. Trial-periods with no measurement anywhere in
#' the window contribute no row, i.e. they are censored out of the estimate.
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
  followup <- weight <- .priority <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  k <- params@end_of_fup.time
  w <- params@end_of_fup.window

  cols <- unique(c(params@id, "trial", "followup", tx_bas, params@outcome,
                   if (params@subgroup %in% names(DT)) params@subgroup,
                   if ("weight" %in% names(DT)) "weight"))
  candidates <- DT[!is.na(get(params@outcome)) & followup >= k - w & followup <= k + w,
                   cols, with = FALSE]
  if (nrow(candidates) == 0L) return(candidates[, "eof.value" := numeric(0)])

  # Prefer the exact-k measurement, then the earliest in the window; ordering by
  # (priority, followup) and taking the first row per trial-period applies both
  # rules in one pass.
  candidates[, .priority := as.integer(followup != k)]
  setorderv(candidates, c(params@id, "trial", ".priority", "followup"))
  out <- candidates[candidates[, .I[1L], by = c(params@id, "trial")]$V1
                    ][, .priority := NULL]

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
#' @param DT expanded data.table for one bootstrap iteration
#' @param params SEQparams object
#' @returns named list of per-arm data.tables, one element per subgroup
#' @import data.table
#' @keywords internal
endoffup.estimate <- function(DT, params) {
  weight <- eof.value <- NULL
  tx_bas <- paste0(params@treatment, params@indicator.baseline)
  measured <- endoffup.measure(DT, params)

  arm_average <- function(dt) {
    if (nrow(dt) == 0L) return(data.table())
    dt <- copy(dt)[weight < params@weight.lower, weight := params@weight.lower
                   ][weight > params@weight.upper, weight := params@weight.upper]
    out <- dt[, list(estimate = sum(weight * eof.value) / sum(weight),
                     n = .N,
                     n.subjects = uniqueN(get(params@id))),
              by = c(tx_bas)]
    setorderv(out, tx_bas)
    out[]
  }

  if (is.na(params@subgroup)) return(list(arm_average(measured)))

  groups <- sort(unique(DT[[params@subgroup]]))
  out <- lapply(groups, function(g) arm_average(measured[get(params@subgroup) == g, ]))
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
  SE <- LCI <- UCI <- Time <- NULL
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
      stat <- lapply(seq_len(nrow(pairs)), function(i) {
        v1 <- as.character(pairs$V1[i]); v2 <- as.character(pairs$V2[i])
        if (!all(c(v1, v2) %in% names(wide))) return(list(NA_real_, NA_real_, NA_real_))
        d <- wide[[v2]] - wide[[v1]]
        if (use_se) {
          list(sd(d, na.rm = TRUE), pairs$diff_[i] - z * sd(d, na.rm = TRUE), pairs$diff_[i] + z * sd(d, na.rm = TRUE))
        } else {
          list(sd(d, na.rm = TRUE), quantile(d, alpha, na.rm = TRUE), quantile(d, 1 - alpha, na.rm = TRUE))
        }
      })
      pairs[, `:=`(SE = vapply(stat, function(x) as.numeric(x[[1]]), numeric(1)),
                   LCI = vapply(stat, function(x) as.numeric(x[[2]]), numeric(1)),
                   UCI = vapply(stat, function(x) as.numeric(x[[3]]), numeric(1)))]
    }
    pairs
  }

  label <- if (params@end_of_fup.type == "binary") "Proportion" else "Mean"
  setnames(data, c(tx_bas, "estimate", "n", "n.subjects"),
           c("A", label, "Trial-periods", "Subjects"), skip_absent = TRUE)
  if (has_ci) setnames(data, c("LCI", "UCI"), paste0(ci_lab, c(" LCI", " UCI")), skip_absent = TRUE)
  data[, `:=`(Time = params@end_of_fup.time, Type = params@end_of_fup.type)]
  setcolorder(data, c("Type", "Time", "A", label))

  if (nrow(comparison) > 0L) {
    setnames(comparison, c("V1", "V2", "diff_", "ratio"), c("A_x", "A_y", "Difference", "Ratio"))
    if (has_ci) setnames(comparison, c("LCI", "UCI", "SE"),
                         c(paste0(ci_lab, c(" LCI", " UCI")), "Difference SE"), skip_absent = TRUE)
    comparison[, Time := params@end_of_fup.time]
    setcolorder(comparison, c("Time", "A_x", "A_y", "Difference"))
  }

  return(list(eof.data = data[], eof.comparison = comparison[]))
}
