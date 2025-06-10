test_that("Parallelism, Bootstrapping, Output Class Methods", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "dose-response", options = SEQopts(
      parallel = TRUE, weighted = TRUE,
      bootstrap = TRUE, bootstrap.nboot = 2, ncores = 1
    )
  ))

  expect_true(length(model@outcome.model[[1]]) > 1)
})

test_that("Non-Parallel Bootstrapping", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "dose-response", options = SEQopts(
      parallel = FALSE, weighted = TRUE,
      bootstrap = TRUE, bootstrap.nboot = 2, ncores = 1
    )
  ))

  expect_true(length(model@outcome.model[[1]]) > 1)
})
