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

  # Independent re-implementation of the selection rule: value at exactly k when
  # present, else the earliest non-missing value in [k - w, k + w]
  manual <- model@DT[!is.na(outcome) & followup >= k - w & followup <= k + w,
                     ][order(ID, trial, followup),
                       ][, {
                           ex <- which(followup == k)
                           i <- if (length(ex)) ex[1] else 1L
                           list(val = outcome[i], arm = tx_init_bas[i])
                         }, by = c("ID", "trial")
                         ][, list(manual = mean(val), n = .N), by = "arm"][order(arm)]

  got <- model@eof.data[[1]][order(A)]
  expect_equal(got$Proportion, manual$manual)
  expect_equal(got$`Trial-periods`, manual$n)
})

test_that("The time window only adds trial-periods with no measurement at k", {
  skip_on_cran()
  exact <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12)
  windowed <- eof_run(end_of_fup = TRUE, end_of_fup.time = 12, end_of_fup.window = 3)

  # Widening the window can only ever add contributors, never remove them
  expect_true(all(windowed@eof.data[[1]]$`Trial-periods` >= exact@eof.data[[1]]$`Trial-periods`))
  expect_true(any(windowed@eof.data[[1]]$`Trial-periods` > exact@eof.data[[1]]$`Trial-periods`))

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
