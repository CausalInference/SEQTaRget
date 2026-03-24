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
