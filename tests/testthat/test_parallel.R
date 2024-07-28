test_that("Parallelism, Bootstrapping, Output Class Methods", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "dose-response", options = SEQopts(parallel = TRUE, weighted = TRUE,
                                                              bootstrap = TRUE, nboot = 2, ncores = 2))

  expect_true(length(model@outcome_model) > 1)
  slice <- explore(model, 2)

  expect_output(show(slice))
})
