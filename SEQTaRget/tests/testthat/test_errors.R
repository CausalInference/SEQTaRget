test_that("Early Column Erroring", {
  data <- SEQdata
  # Column tests ========================
  expect_error(SEQuential())
  expect_error(SEQuential(data))
  expect_error(SEQuential(data, id.col = "ID"))
  expect_error(SEQuential(data, id.col = "ID", time.col = "time"))
  expect_error(SEQuential(data, id.col = "ID", time.col = "time", eligible.col = "eligible"))
  expect_error(SEQuential(data, id.col = "ID", time.col = "time", eligible.col = "eligible", treatment.col = "tx_init"))
  expect_error(SEQuential(data, id.col = "ID", time.col = "time", eligible.col = "eligible", treatment.col = "tx_init", outcome.col = "outcome"))
  expect_error(SEQuential(data,
    id.col = "ID", time.col = "time", eligible.col = "eligible", treatment.col = "tx_init", outcome.col = "outcome", method = "ITT",
    time_varying.cols = c("N", "L", "P"), fixed.cols = c("sex", "foobar")
  ))
  expect_error(SEQuential(data,
    id.col = "ID", time.col = "time", eligible.col = "eligible", treatment.col = "tx_init", outcome.col = "outcome", method = "ITT",
    time_varying.cols = c("N", "L", "P"), fixed.cols = c("sex"), options = list()
  ))

  # Missing Fixed/TimeVarying columns ====
  expect_warning(SEQuential(data,
    id.col = "ID", time.col = "time", eligible.col = "eligible", treatment.col = "tx_init", outcome.col = "outcome", method = "ITT",
    time_varying.cols = c("N", "L", "P"), options = SEQopts()
  ))
  expect_warning(SEQuential(data,
    id.col = "ID", time.col = "time", eligible.col = "eligible", treatment.col = "tx_init", outcome.col = "outcome", method = "ITT",
    fixed.cols = "sex", options = SEQopts()
  ))

  # Method failure ========================
  expect_error(SEQuential(data,
    id.col = "ID", time.col = "time", eligible.col = "eligible", treatment.col = "tx_init", outcome.col = "outcome", method = "foobar",
    time_varying.cols = c("N", "L", "P"), fixed.cols = "sex", options = SEQopts()
  ))

  # Output Testing ========================
  expect_error(explore(list(), 2))
})

test_that("Multiple eligibility switches per subject are accepted (regression: #eligible_col single-transition constraint)", {
  # Subjects can legitimately become eligible, enter a trial, become ineligible,
  # then re-enrol — producing multiple 0/1 switches. A prior check incorrectly
  # rejected such datasets; this test guards against reintroducing that check.
  multi_switch <- copy(SEQdata)
  multi_switch[ID == 1 & time %in% c(5, 6, 7), eligible := 0L]  # creates two eligibility windows for subject 1: 1...1,0,0,0,1...
  expect_no_error(SEQuential(multi_switch,
    id.col = "ID", time.col = "time", eligible.col = "eligible",
    treatment.col = "tx_init", outcome.col = "outcome", method = "ITT",
    time_varying.cols = c("N", "L", "P"), fixed.cols = "sex", options = SEQopts()
  ))
})

test_that("Missing Observations in Data", {
  md <- copy(SEQdata)[1, tx_init := NA]
  expect_error(SEQuential(md, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                            method = "ITT",
                            options = SEQopts()))
})
