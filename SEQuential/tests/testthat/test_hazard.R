test_that("Hazard and vcov", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(calculate.var = TRUE, hazard = TRUE))
  expect_s4_class(model, "SEQoutput")
})
