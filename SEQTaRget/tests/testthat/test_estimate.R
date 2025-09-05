test_that("Time Estimate - Unit Check", {
  data <- SEQdata
  time <- SEQestimate(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts())
  expect_length(time, 3)
})
