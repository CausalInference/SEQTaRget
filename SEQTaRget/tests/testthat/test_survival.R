test_that("Survival Return", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(model@survival.curve[[1]], "ggplot")
})

test_that("Bootstrapped Survival", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE, bootstrap = TRUE, bootstrap.nboot = 2))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(model@survival.curve[[1]], "ggplot")
})

test_that("Bootstrapped Survival - Percentile", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE, bootstrap = TRUE, bootstrap.nboot = 2, bootstrap.CI_method = "percentile"))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(model@survival.curve[[1]], "ggplot")
})
