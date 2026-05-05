library(data.table)

test_that("Expanded dataset contains no trials beyond the last eligible row per subject", {
  # max(trial) in the expanded data must equal the 0-based index of the last
  # eligible row across all subjects — trailing ineligible rows must not become trials.
  model <- suppressWarnings(SEQuential(copy(SEQdata),
    "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"), method = "ITT",
    options = SEQopts(data.return = TRUE)
  ))
  last_elig_idx <- SEQdata[, .(last_elig = max(which(eligible == 1L)) - 1L), by = ID]
  expect_equal(max(model@DT$trial), max(last_elig_idx$last_elig))
})

test_that("Expansion truncates trials at first outcome event - subject with early outcome not carried forward", {
  # Subject 1 has outcome=1 at time=0 (the only eligible period), then continues in the
  # dataset at times 1-3 with outcome=0. Without truncation they would appear in the
  # expanded data for all four periods with the later outcome=0 rows overwriting the event.
  # Subject 2 has no outcome and serves as a control.
  dt <- data.table::data.table(
    ID        = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    time      = c(0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L),
    eligible  = c(1L, 0L, 0L, 0L, 1L, 1L, 1L, 1L),
    treatment = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    outcome   = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    N         = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    sex       = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L)
  )
  model <- SEQuential(dt, "ID", "time", "eligible", "treatment", "outcome",
                      list("N"), list("sex"),
                      method = "ITT",
                      options = SEQopts(data.return = TRUE),
                      verbose = FALSE)
  # Subject 1's only trial (trial=0) should contain exactly one row (followup=0, outcome=1)
  s1_trial0 <- model@DT[ID == 1L & trial == 0L]
  expect_equal(nrow(s1_trial0), 1L)
  expect_equal(s1_trial0$outcome, 1L)
})

test_that("Expansion truncates trials at first outcome event - subject with early outcome not carried forward - test 2", {
  # Subject 1 has outcome=1 at time=0 (the only eligible period), then continues in the
  # dataset at times 1-3 with outcome=1. Without truncation they would appear in the
  # expanded data for all four periods with the later outcome=1 rows overwriting the event.
  # Subject 2 has no outcome and serves as a control.
  dt <- data.table::data.table(
    ID        = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    time      = c(0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L),
    eligible  = c(1L, 0L, 0L, 0L, 1L, 1L, 1L, 1L),
    treatment = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    outcome   = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    N         = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    sex       = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L)
  )
  model <- SEQuential(dt, "ID", "time", "eligible", "treatment", "outcome",
                      list("N"), list("sex"),
                      method = "ITT",
                      options = SEQopts(data.return = TRUE),
                      verbose = FALSE)
  # Subject 1's only trial (trial=0) should contain exactly one row (followup=0, outcome=1)
  s1_trial0 <- model@DT[ID == 1L & trial == 0L]
  expect_equal(nrow(s1_trial0), 1L)
  expect_equal(s1_trial0$outcome, 1L)
})

test_that("Pre-Expansion Excused Censoring - No excusedOne given", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.cols = c("excusedZero")))
  expect_s4_class(model, "SEQoutput")
})

test_that("Pre-Expansion Excused Censoring - No excusedZero given", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.cols = c(NA, "excusedOne")))
  expect_s4_class(model, "SEQoutput")
})

test_that("Unweighted Censoring and Dose-Reponse", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
             method = "censoring",
             options = SEQopts(weighted = FALSE))
  expect_s4_class(model, "SEQoutput")
})

test_that("ITT - Followup Class", {
  data <- copy(SEQdata)[time <= 5, ]
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(followup.class = TRUE, followup.include = FALSE)))
  expect_s4_class(model, "SEQoutput")
})

test_that("ITT - Followup Spline", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(followup.spline = TRUE, followup.include = FALSE))
  expect_s4_class(model, "SEQoutput")
})

test_that("Error 107 - followup.include = FALSE failing to create covariates", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(followup.include = FALSE))
  expect_s4_class(model, "SEQoutput")
})

test_that("expand.only = TRUE returns expanded data.table and matches data.return DT", {
  # When expand.only = TRUE, SEQuential should short-circuit after expansion and
  # return the expanded data.table directly. The contents should match the DT slot
  # from a full run with data.return = TRUE.
  data <- copy(SEQdata)
  expanded <- suppressWarnings(SEQuential(copy(data), "ID", "time", "eligible", "tx_init", "outcome",
                                          list("N", "L", "P"), list("sex"),
                                          method = "ITT",
                                          options = SEQopts(expand.only = TRUE),
                                          verbose = FALSE))
  expect_s3_class(expanded, "data.table")
  expect_true(nrow(expanded) > 0L)
  expect_true(all(c("ID", "trial", "period", "followup", "outcome") %in% names(expanded)))

  full <- suppressWarnings(SEQuential(copy(data), "ID", "time", "eligible", "tx_init", "outcome",
                                      list("N", "L", "P"), list("sex"),
                                      method = "ITT",
                                      options = SEQopts(data.return = TRUE),
                                      verbose = FALSE))
  setkeyv(expanded, c("ID", "trial", "followup"))
  ref <- copy(full@DT)
  setkeyv(ref, c("ID", "trial", "followup"))
  expect_equal(nrow(expanded), nrow(ref))
  expect_equal(expanded$ID, ref$ID)
  expect_equal(expanded$trial, ref$trial)
  expect_equal(expanded$followup, ref$followup)
  expect_equal(expanded$outcome, ref$outcome)
})
