test_that("Output Methods Testing", {
  model <- SEQuential(SEQdata, "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "censoring",
                      options = SEQopts(km.curves = TRUE, weighted = TRUE, bootstrap = TRUE, bootstrap.nboot = 2))
  expect_s4_class(model, "SEQoutput")
  
  expect_output(show(model))
  expect_length(outcome(model)[[1]], 3)
  expect_length(numerator(model), 2)
  expect_length(denominator(model), 2)
  expect_length(covariates(model), 3)
  
  expect_s3_class(km_curve(model)[[1]], "ggplot")
  expect_length(km_data(model), 1)
  
  nonWeightModel <- SEQuential(SEQdata, "ID", "time", "eligible", "tx_init", "outcome",
                               list("N", "L", "P"), list("sex"),
                               method = "censoring",
                               options = SEQopts())
  expect_output(show(nonWeightModel))
})
