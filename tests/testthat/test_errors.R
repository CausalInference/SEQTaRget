test_that("Early Erroring", {
  data = SEQdata
  # Column tests ========================
  expect_error(SEQuential())
  expect_error(SEQuential(data))
  expect_error(SEQuential(data, id.col = "ID"))
  expect_error(SEQuential(data, id.col = "ID", time.col = "time"))
  expect_error(SEQuential(data, id.col = "ID", time.col = "time", eligible.col = "eligible"))
  expect_error(SEQuential(data, id.col = "ID", time.col = "time", eligible.col = "eligible", outcome.col = "outcome"))
  expect_error(SEQuential(data, id.col = "ID", time.col = "time", eligible.col = "eligible", outcome.col = "outcome", method = "ITT"))
  expect_error(SEQuential(data, id.col = "ID", time.col = "time", eligible.col = "eligible", outcome.col = "outcome", method = "ITT",
                          time_varying.cols = c("N", "L", "P")))
  expect_error(SEQuential(data, id.col = "ID", time.col = "time", eligible.col = "eligible", outcome.col = "outcome", method = "ITT",
                          time_varying.cols = c("N", "L", "P"), fixed.cols = c("sex", "race")))
  # Method Testing ========================
  expect_error(SEQuential(data, id.col = "ID", time.col = "time", eligible.col = "eligible", outcome.col = "outcome", method = "ITT",
                          time_varying.cols = c("N", "L", "P"), fixed.cols = c("sex", "race"), options = list()))

  # Output Testing ========================
  expect_error(explore(list(), 2))
})
