test_that("Survival Return", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(km_curve(model), "ggplot")
})

test_that("Bootstrapped Survival", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE, bootstrap = TRUE, bootstrap.nboot = 2))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(km_curve(model), "ggplot")
})

test_that("Bootstrapped Survival - Percentile", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE, bootstrap = TRUE, bootstrap.nboot = 2, bootstrap.CI_method = "percentile"))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(km_curve(model), "ggplot")
})

test_that("Bootstrapped Survival - Competing Event CIs present", {
  data <- data.table::copy(SEQdata)
  set.seed(42)
  data[, compevent := as.integer(runif(.N) < 0.02)]
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE, compevent = "compevent",
                                                        bootstrap = TRUE, bootstrap.nboot = 2))
  expect_s4_class(model, "SEQoutput")
  ci_cols <- c("RD 95% LCI", "RD 95% UCI", "RR 95% LCI", "RR 95% UCI")
  expect_true(all(ci_cols %in% names(model@risk.comparison[[1]])))
  expect_true(all(!is.na(model@risk.comparison[[1]][, ci_cols, with = FALSE])))
})
