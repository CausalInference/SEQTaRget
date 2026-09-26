test_that("Parallelism, Bootstrapping, Output Class Methods", {
  skip_on_cran()
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "dose-response", options = SEQopts(
      parallel = TRUE, weighted = TRUE,
      bootstrap = TRUE, bootstrap.nboot = 2, ncores = 1
    )
  ))

  expect_true(length(model@outcome.model[[1]]) > 1)
})

test_that("Non-Parallel Bootstrapping", {
  skip_on_cran()
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "dose-response", options = SEQopts(
      parallel = FALSE, weighted = TRUE,
      bootstrap = TRUE, bootstrap.nboot = 2, ncores = 1
    )
  ))

  expect_true(length(model@outcome.model[[1]]) > 1)
})

test_that("parallel = TRUE restores the caller's future plan, including on error", {
  skip_on_cran()
  old_plan <- future::plan(future::multicore, workers = 2)
  on.exit(future::plan(old_plan), add = TRUE)
  run <- function(...) suppressWarnings(SEQuential(data.table::copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                                                   list("N", "L", "P"), list("sex"), method = "ITT", verbose = FALSE,
                                                   options = SEQopts(parallel = TRUE, ncores = 1, ...)))
  run()
  expect_true(inherits(future::plan(), "multicore"))
  expect_error(run(cense = "not_a_column"), "not_a_column")
  expect_true(inherits(future::plan(), "multicore"))
})
