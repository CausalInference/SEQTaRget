test_that("Parallelism + Bootstrapping", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(parallel = TRUE, bootstrap = TRUE, nboot = 2))

  expect_true(length(model@outcome_model) > 1)
})
