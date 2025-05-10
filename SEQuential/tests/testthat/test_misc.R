test_that("Pre-Expansion Excused Censoring - No excusedOne given", {
  model <- suppressWarnings(SEQuential(SEQdata, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.cols = c("excusedZero")
    )
  ))
  expect_s4_class(model, "SEQoutput")
})

test_that("Pre-Expansion Excused Censoring - No excusedZero given", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.cols = c(NA, "excusedOne")
    )
  ))
  expect_s4_class(model, "SEQoutput")
})

test_that("Unweighted Censoring and Dose-Reponse", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
             method = "censoring",
             options = SEQopts(weighted = FALSE))
  expect_s4_class(model, "SEQoutput")
})

test_that("ITT - Followup Class", {
  data <- SEQdata
  model <- SEQuential(data[time <= 5], "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(followup.class = TRUE, followup.include = FALSE))
  expect_s4_class(model, "SEQoutput")
})

test_that("ITT - Followup Spline", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(followup.spline = TRUE, followup.include = FALSE))
  expect_s4_class(model, "SEQoutput")
})
