test_that("Hazard and vcov", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(hazard = TRUE))
  expect_s4_class(model, "SEQoutput")
})
