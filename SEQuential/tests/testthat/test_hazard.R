test_that("Hazard and vcov", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(calculate.var = TRUE, hazard = TRUE,
                                                        hazard.time_varying.dist = list(list('N' = function(n) rnorm(n, 10, 1),
                                                                  'L' = function(n) rnorm(n, 10, 1),
                                                                  'P' = function(n) rnorm(n, 10, 1)))))
  expect_s4_class(model, "SEQoutput")
})
