# End-of-follow-up outcomes: a single measurement taken at end_of_fup.time,
# averaged within each baseline arm using the weight at that time, rather than
# a time-to-event fitted with a survival outcome model.

eof_run <- function(..., data = copy(SEQdata), outcome = "outcome", method = "ITT") {
  suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", outcome,
                              list("N", "L", "P"), list("sex"), method = method,
                              options = SEQopts(...), verbose = FALSE))
}

test_that("Unweighted end-of-follow-up estimate is the mean of the selected measurements", {
  skip_on_cran()
  k <- 12; w <- 3
  model <- eof_run(end_of_fup = TRUE, end_of_fup.time = k, end_of_fup.window = w,
                   data.return = TRUE)
  expect_s4_class(model, "SEQoutput")

  # Independent re-implementation of the selection rule: the non-missing value
  # nearest to k within [k - w, k + w], ties broken toward the later one. Rows
  # are in ascending followup order, so taking the last of the equally-near
  # measurements picks the later one.
  manual <- model@DT[!is.na(outcome) & followup >= k - w & followup <= k + w,
                     ][order(ID, trial, followup),
                       ][, {
                           d <- abs(followup - k)
                           i <- max(which(d == min(d)))
                           list(val = outcome[i], arm = tx_init_bas[i])
                         }, by = c("ID", "trial")
                         ][, list(manual = mean(val), n = .N), by = "arm"][order(arm)]

  got <- model@eof.data[[1]][order(A)]
  expect_equal(got$Proportion, manual$manual)
  expect_equal(got$`Trial-periods (Analysed)`, manual$n)
})

test_that("The time window only adds trial-periods with no measurement at k", {
  skip_on_cran()
  exact <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12)
  windowed <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.window = 3)

  # Widening the window can only ever add contributors, never remove them
  expect_true(all(windowed@eof.data[[1]]$`Trial-periods (Analysed)` >= exact@eof.data[[1]]$`Trial-periods (Analysed)`))
  expect_true(any(windowed@eof.data[[1]]$`Trial-periods (Analysed)` > exact@eof.data[[1]]$`Trial-periods (Analysed)`))

  # A window of 0 is the same as no window at all
  expect_equal(eof_run(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.window = 0)@eof.data[[1]],
               exact@eof.data[[1]])
})

test_that("Trial-periods measured at exactly k keep that measurement when a window is given", {
  skip_on_cran()
  k <- 12; w <- 3
  model <- eof_run(end_of_fup = TRUE, end_of_fup.time = k, end_of_fup.window = w,
                   data.return = TRUE)
  params <- model@params
  measured <- endoffup.measure(model@DT, params)

  # Every trial-period with a measurement at k must have been read at k, not
  # from elsewhere in the window
  at_k <- model@DT[!is.na(outcome) & followup == k, .(ID, trial)]
  taken <- merge(measured[, .(ID, trial, followup)], at_k, by = c("ID", "trial"))
  expect_true(nrow(taken) > 0)
  expect_true(all(taken$followup == k))

  # Fallback rows all lie inside the window and none sit at k
  expect_true(all(measured$followup >= k - w & measured$followup <= k + w))
})

test_that("The window takes the measurement nearest to k, not the earliest", {
  skip_on_cran()
  # Hand-built trial-periods where 'nearest' and 'earliest' disagree, driven
  # through the selection helper directly so the rule is tested in isolation.
  k <- 3; w <- 2
  params <- eof_run(end_of_fup = TRUE, end_of_fup.time = k, end_of_fup.window = w)@params

  DT <- data.table::data.table(
    ID          = c(1L, 1L, 2L, 2L, 3L, 3L),
    trial       = 0L,
    followup    = c(1L, 4L, 2L, 3L, 2L, 4L),
    tx_init_bas = factor(c(0, 0, 1, 1, 0, 0)),
    outcome     = c(10, 20, 30, 40, 50, 60)
  )
  got <- endoffup.measure(DT, params)[order(ID)]

  # ID 1: |1-k|=2 vs |4-k|=1, so the later measurement is nearer - the earliest
  #       rule would have taken followup 1
  # ID 2: measured at exactly k, which always wins
  # ID 3: |2-k|=|4-k|=1, an equidistant tie broken toward the later, so that at
  #       least k of follow-up has elapsed
  expect_equal(got$followup, c(4L, 3L, 4L))
  expect_equal(got$eof.value, c(20, 40, 60))
})

test_that("Continuous end-of-follow-up outcomes are supported and reported as a mean", {
  skip_on_cran()
  set.seed(42)
  d <- copy(SEQdata)
  d[, cont := 10 + 2 * as.numeric(as.character(tx_init)) + N + rnorm(.N)]
  model <- eof_run(data = d, outcome = "cont", end_of_fup = TRUE, end_of_fup.time = 12,
                   end_of_fup.type = "continuous", end_of_fup.window = 3,
                   bootstrap = TRUE, bootstrap.nboot = 5)

  expect_true("Mean" %in% names(model@eof.data[[1]]))
  expect_false("Proportion" %in% names(model@eof.data[[1]]))
  expect_true(all(model@eof.data[[1]]$Mean > 1))
  # A continuous mean is unbounded, so its interval must not be clamped to [0, 1]
  expect_true(all(model@eof.data[[1]][[grep("LCI", names(model@eof.data[[1]]), value = TRUE)]] > 1))
})

test_that("Binary end-of-follow-up intervals are clamped to [0, 1]", {
  skip_on_cran()
  model <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.window = 3,
                   bootstrap = TRUE, bootstrap.nboot = 5)
  d <- model@eof.data[[1]]
  expect_true(all(d[[grep("LCI", names(d), value = TRUE)]] >= 0))
  expect_true(all(d[[grep("UCI", names(d), value = TRUE)]] <= 1))
})

test_that("End-of-follow-up works with weighted per-protocol censoring", {
  skip_on_cran()
  model <- eof_run(method = "censoring", weighted = TRUE, end_of_fup = TRUE,
                   end_of_fup.time = 12, end_of_fup.window = 3,
                   bootstrap = TRUE, bootstrap.nboot = 5)
  expect_true(nrow(model@eof.data[[1]]) == 2)
  expect_true(all(is.finite(model@eof.data[[1]]$Proportion)))
  # Weight statistics are still reported even though no outcome model is fit
  expect_false(is.null(model@weight.statistics[[1]][[1]]$p50))
  expect_output(show(model))
})

test_that("Bootstrap gives per-arm and paired between-arm confidence intervals", {
  skip_on_cran()
  model <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.window = 3,
                   bootstrap = TRUE, bootstrap.nboot = 10)
  d <- model@eof.data[[1]]
  cmp <- model@eof.comparison[[1]]

  expect_true("SE" %in% names(d))
  expect_true(all(d[[grep("LCI", names(d), value = TRUE)]] <= d$Proportion))
  expect_true(all(d[[grep("UCI", names(d), value = TRUE)]] >= d$Proportion))

  # Both directions of the arm pair, with equal and opposite differences
  expect_equal(nrow(cmp), 2L)
  expect_equal(cmp$Difference[1], -cmp$Difference[2])
  expect_true(all(is.finite(cmp$`Difference SE`)))

  # The difference carries its own interval, bracketing the point estimate
  expect_true(all(cmp$`Difference 95% LCI` <= cmp$Difference))
  expect_true(all(cmp$`Difference 95% UCI` >= cmp$Difference))
})

test_that("A binary outcome also reports the ratio of proportions with an interval", {
  skip_on_cran()
  cmp <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.window = 3,
                 bootstrap = TRUE, bootstrap.nboot = 20)@eof.comparison[[1]]

  expect_true(all(c("Ratio", "Ratio 95% LCI", "Ratio 95% UCI", "log(Ratio) SE") %in% names(cmp)))
  expect_true(all(cmp$`Ratio 95% LCI` <= cmp$Ratio))
  expect_true(all(cmp$`Ratio 95% UCI` >= cmp$Ratio))
  # A ratio interval built on the log scale cannot cross zero
  expect_true(all(cmp$`Ratio 95% LCI` > 0))
  # Reversing the arms inverts the ratio, and the log-scale SE is direction-free
  expect_equal(cmp$Ratio[1], 1 / cmp$Ratio[2])
  expect_equal(cmp$`log(Ratio) SE`[1], cmp$`log(Ratio) SE`[2])
})

test_that("A continuous outcome reports a difference in means but no ratio", {
  skip_on_cran()
  set.seed(42)
  d <- copy(SEQdata)
  d[, cont := 10 + 2 * as.numeric(as.character(tx_init)) + N + rnorm(.N)]
  cmp <- eof_run(data = d, outcome = "cont", end_of_fup = TRUE, end_of_fup.time = 12,
                 end_of_fup.type = "continuous", end_of_fup.window = 3,
                 bootstrap = TRUE, bootstrap.nboot = 20)@eof.comparison[[1]]

  expect_true(all(c("Difference", "Difference SE") %in% names(cmp)))
  expect_false(any(grepl("Ratio", names(cmp))))

  # The difference in means is the gap between the two arm means
  est <- eof_run(data = d, outcome = "cont", end_of_fup = TRUE, end_of_fup.time = 12,
                 end_of_fup.type = "continuous", end_of_fup.window = 3)@eof.data[[1]][order(A)]
  expect_equal(cmp[A_x == est$A[1] & A_y == est$A[2]]$Difference,
               est$Mean[2] - est$Mean[1])
})

test_that("Subgroups produce one set of end-of-follow-up estimates each", {
  skip_on_cran()
  model <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12, subgroup = "sex")
  expect_named(model@eof.data, c("sex_0", "sex_1"))
  expect_named(model@eof.comparison, c("sex_0", "sex_1"))
  expect_false(identical(model@eof.data[[1]]$Proportion, model@eof.data[[2]]$Proportion))
})

test_that("end_of_fup() accessor returns estimates and comparison", {
  skip_on_cran()
  model <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12)
  out <- end_of_fup(model)
  expect_named(out[[1]], c("estimates", "comparison"))
  expect_s3_class(out[[1]]$estimates, "data.table")

  survival_model <- eof_run(km.curves = FALSE)
  expect_error(end_of_fup(survival_model), "end_of_fup = FALSE")
})

test_that("End-of-follow-up rejects incompatible options", {
  expect_error(eof_run(end_of_fup = TRUE, end_of_fup.time = 12, km.curves = TRUE),
               "not compatible with 'km.curves'")
  expect_error(eof_run(end_of_fup = TRUE, end_of_fup.time = 12, hazard = TRUE),
               "not compatible with 'km.curves'")
  expect_error(eof_run(end_of_fup = TRUE, end_of_fup.time = 12, compevent = "P"),
               "not compatible with 'compevent'")
  expect_error(eof_run(method = "dose-response", weighted = TRUE,
                       end_of_fup = TRUE, end_of_fup.time = 12),
               "not supported for the dose-response method")
})

test_that("SEQopts validates the end-of-follow-up arguments", {
  expect_error(SEQopts(end_of_fup = TRUE), "'end_of_fup.time' must be a single non-missing")
  expect_error(SEQopts(end_of_fup = TRUE, end_of_fup.time = -1), "must be non-negative")
  expect_error(SEQopts(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.type = "ordinal"),
               "must be one of")
  expect_error(SEQopts(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.window = -2),
               "must be a single non-negative number")
  # Unused arguments are not validated when the feature is off
  expect_s4_class(SEQopts(end_of_fup = FALSE, end_of_fup.type = "nonsense"), "SEQopts")
})

test_that("The requested time must lie within the expanded follow-up", {
  expect_error(eof_run(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.window = 3,
                       followup.max = 13),
               "exceeds the maximum follow-up")
  expect_error(eof_run(end_of_fup = TRUE, end_of_fup.time = 4, end_of_fup.window = 3,
                       followup.min = 2),
               "below the minimum follow-up")
})

test_that("Outcome count tables are suppressed for a continuous outcome only", {
  skip_on_cran()
  set.seed(42)
  d <- copy(SEQdata)
  d[, cont := 10 + 2 * as.numeric(as.character(tx_init)) + N + rnorm(.N)]

  # Counting outcome == 1 rows is meaningless for a continuous outcome (it would
  # count values that happen to equal exactly 1), so the tables are NA
  cont <- eof_run(data = d, outcome = "cont", end_of_fup = TRUE, end_of_fup.time = 12,
                  end_of_fup.type = "continuous")
  expect_true(all(is.na(diagnostics(cont)$outcome.unique)))
  expect_true(all(is.na(diagnostics(cont)$outcome.nonunique)))

  # Person-time is still meaningful and must survive
  expect_s3_class(diagnostics(cont)$followup.unique[[1]], "data.table")
  expect_true(sum(diagnostics(cont)$followup.nonunique[[1]]$n) > 0)

  printed <- capture.output(show(cont))
  expect_length(grep("Outcome Table", printed), 0)
  expect_true(any(grepl("Follow-up Table", printed)))

  # A binary end-of-follow-up outcome keeps its outcome tables
  bin <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12)
  expect_s3_class(diagnostics(bin)$outcome.unique[[1]], "data.table")
  expect_true(any(grepl("Outcome Table", capture.output(show(bin)))))
})

test_that("Continuous outcome tables are suppressed without losing subgroup follow-up tables", {
  skip_on_cran()
  set.seed(42)
  d <- copy(SEQdata)
  d[, cont := 10 + 2 * as.numeric(as.character(tx_init)) + N + rnorm(.N)]
  model <- eof_run(data = d, outcome = "cont", end_of_fup = TRUE, end_of_fup.time = 12,
                   end_of_fup.type = "continuous", subgroup = "sex")

  expect_named(diagnostics(model)$followup.unique, c("sex_0", "sex_1"))
  # Both subgroups still print their follow-up tables despite the outcome tables
  # (which normally drive that loop) being absent
  printed <- capture.output(show(model))
  expect_length(grep("distinct subjects contributing follow-up", printed), 2)
  expect_length(grep("person-time intervals", printed), 2)
})

test_that("End-of-follow-up expansion is not truncated at the first event", {
  skip_on_cran()
  # The survival path cuts each trial at its first outcome row. An end-of-follow-up
  # outcome is a status measured repeatedly and read at a fixed time, so that
  # truncation must not apply or the measurement at k would be discarded for
  # anyone whose status was ever 1 earlier. SEQdata cannot show this - every
  # subject's series already ends at their event - so this uses a dataset where
  # measurement genuinely continues afterwards.
  set.seed(7)
  n_id <- 40; n_t <- 20
  d <- data.table(ID = rep(seq_len(n_id), each = n_t),
                  time = rep(0:(n_t - 1), times = n_id))
  d[, `:=`(eligible = 1L,
           tx_init = rbinom(.N, 1, 0.5),
           outcome = rbinom(.N, 1, 0.3),   # a status that recurs, not a terminal event
           N = rnorm(.N), L = rnorm(.N), P = rnorm(.N))]
  d[, sex := rep(rbinom(n_id, 1, 0.5), each = n_t)]

  eof <- eof_run(data = copy(d), end_of_fup = TRUE, end_of_fup.time = 5, data.return = TRUE)
  surv <- eof_run(data = copy(d), data.return = TRUE)

  expect_gt(nrow(eof@DT), nrow(surv@DT))
  # Truncation leaves at most one outcome row per trial; without it a trial keeps several
  expect_equal(max(surv@DT[, sum(outcome == 1, na.rm = TRUE), by = c("ID", "trial")]$V1), 1)
  expect_gt(max(eof@DT[, sum(outcome == 1, na.rm = TRUE), by = c("ID", "trial")]$V1), 1)

  # And the estimate is still readable at k for trials whose status was 1 earlier
  expect_true(all(is.finite(eof@eof.data[[1]]$Proportion)))
})

test_that("The estimates table reports the censored trial-periods and their share", {
  skip_on_cran()
  model <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.window = 3)
  est <- model@eof.data[[1]][order(A)]
  expect_true(all(c("Trial-periods (Eligible)", "Trial-periods (Censored)",
                    "Trial-periods (No measurement)", "% Censored") %in% names(est)))

  nonunique <- diagnostics(model)$eof.nonunique[[1]]
  expect_equal(est$`Trial-periods (Eligible)`, nonunique$Eligible)
  expect_equal(est$`Trial-periods (Analysed)`, nonunique$`At k` + nonunique$`In window`)

  # Censored counts only the trial-periods measured outside the window; those
  # never measured at all are reported separately, so the three counts partition
  # the eligible total
  expect_equal(est$`Trial-periods (Censored)`, nonunique$`Excluded (outside window)`)
  expect_equal(est$`Trial-periods (No measurement)`, nonunique$`Excluded (no measurement)`)
  expect_equal(est$`Trial-periods (Analysed)` + est$`Trial-periods (Censored)` +
                 est$`Trial-periods (No measurement)`,
               est$`Trial-periods (Eligible)`)

  # The percentage is of the eligible trial-periods, not of the contributors
  expect_equal(est$`% Censored`, 100 * est$`Trial-periods (Censored)` / est$`Trial-periods (Eligible)`)
  expect_true(all(est$`% Censored` >= 0 & est$`% Censored` <= 100))

  # Widening the window recovers trial-periods, so fewer are censored
  narrow <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12)@eof.data[[1]][order(A)]
  expect_true(all(narrow$`Trial-periods (Censored)` >= est$`Trial-periods (Censored)`))
  expect_true(any(narrow$`Trial-periods (Censored)` > est$`Trial-periods (Censored)`))
})

test_that("The end-of-follow-up counts table accounts for every trial-period", {
  skip_on_cran()
  model <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.window = 3)
  nonunique <- diagnostics(model)$eof.nonunique[[1]]
  categories <- c("At k", "In window", "Excluded (outside window)", "Excluded (no measurement)")
  expect_true(all(c("Eligible", categories) %in% names(nonunique)))

  # Trial-period categories are mutually exclusive, so they partition Eligible
  expect_equal(rowSums(nonunique[, categories, with = FALSE]), nonunique$Eligible)

  # And the two contributing categories are exactly what the estimate is built from
  expect_equal(nonunique$`At k` + nonunique$`In window`,
               model@eof.data[[1]][order(A)]$`Trial-periods (Analysed)`)

  # Subject counts are reported too, but may overlap across categories
  unique_tab <- diagnostics(model)$eof.unique[[1]]
  expect_true(all(unique_tab$Eligible > 0))
  expect_true(all(rowSums(unique_tab[, categories, with = FALSE]) >= unique_tab$Eligible))

  printed <- capture.output(show(model))
  expect_true(any(grepl("End-of-Follow-up Table", printed)))
})

test_that("A window of zero puts every contributing trial-period in the At k category", {
  skip_on_cran()
  nonunique <- diagnostics(eof_run(end_of_fup = TRUE, end_of_fup.time = 12))$eof.nonunique[[1]]
  expect_true(all(nonunique$`In window` == 0))
  expect_true(all(nonunique$`At k` > 0))
})

test_that("Counts tables are absent unless end_of_fup is used", {
  skip_on_cran()
  expect_true(all(is.na(diagnostics(eof_run(km.curves = FALSE))$eof.nonunique)))
})

test_that("Missing outcome measurements are permitted only in end_of_fup mode", {
  skip_on_cran()
  # An end-of-follow-up outcome is measured at particular visits, so NA records
  # "not measured here" - which is what the window exists to handle.
  set.seed(11)
  n_id <- 60; n_t <- 20
  d <- data.table(ID = rep(seq_len(n_id), each = n_t), time = rep(0:(n_t - 1), times = n_id))
  d[, `:=`(eligible = 1L, tx_init = rbinom(.N, 1, 0.5),
           N = rnorm(.N), L = rnorm(.N), P = rnorm(.N),
           outcome = rbinom(.N, 1, 0.3))]
  d[, sex := rep(rbinom(n_id, 1, 0.5), each = n_t)]
  d[time %% 4 != 0, outcome := NA]   # measured every 4th visit only
  d[ID <= 8, outcome := NA]          # never measured at all

  model <- eof_run(data = copy(d), end_of_fup = TRUE, end_of_fup.time = 8, end_of_fup.window = 1)
  nonunique <- diagnostics(model)$eof.nonunique[[1]]
  categories <- c("At k", "In window", "Excluded (outside window)", "Excluded (no measurement)")

  # Never-measured trial-periods are now a real, populated category
  expect_true(sum(nonunique$`Excluded (no measurement)`) > 0)
  expect_equal(rowSums(nonunique[, categories, with = FALSE]), nonunique$Eligible)
  expect_equal(nonunique$`At k` + nonunique$`In window`,
               model@eof.data[[1]][order(A)]$`Trial-periods (Analysed)`)

  # A survival analysis still rejects the same data
  expect_error(eof_run(data = copy(d)), "Data contains NA values")
  # And so does end_of_fup if the missingness is in another column
  d2 <- copy(d)[, outcome := 1L][time == 3, N := NA_real_]
  expect_error(eof_run(data = d2, end_of_fup = TRUE, end_of_fup.time = 8),
               "outcome column only")
})

test_that("A non-binary outcome under the default type points at end_of_fup.type", {
  skip_on_cran()
  set.seed(1)
  d <- copy(SEQdata)
  d[, bio := rnorm(.N, 20)]

  # end_of_fup.type defaults to "binary", so a continuous outcome must fail
  # loudly rather than be averaged as if it were a proportion
  err <- expect_error(eof_run(data = copy(d), outcome = "bio",
                              end_of_fup = TRUE, end_of_fup.time = 12),
                      "must be binary")
  expect_match(conditionMessage(err), "end_of_fup.type", fixed = TRUE)
  # The offending values are summarised rather than listed in full
  expect_match(conditionMessage(err), "distinct values", fixed = TRUE)

  # A survival analysis fails the same way but without the end_of_fup advice
  survival_err <- expect_error(eof_run(data = copy(d), outcome = "bio"), "must be binary")
  expect_false(grepl("end_of_fup.type", conditionMessage(survival_err), fixed = TRUE))
})
