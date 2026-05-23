test_that("Hazard and vcov", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(hazard = TRUE))
  expect_s4_class(model, "SEQoutput")
})

test_that("Hazard estimate is reproducible with same seed", {
  skip_on_cran()
  args <- list("ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
               method = "ITT", options = SEQopts(hazard = TRUE, seed = 123L))
  
  run1 <- do.call(SEQuential, c(list(data = data.table::copy(SEQdata)), args))
  run2 <- do.call(SEQuential, c(list(data = data.table::copy(SEQdata)), args))
  
  expect_identical(run1@hazard, run2@hazard)
})

test_that("Hazard bootstrap CIs are reproducible with same seed", {
  skip_on_cran()
  args <- list("ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
               method = "ITT", options = SEQopts(hazard = TRUE, bootstrap = TRUE, bootstrap.nboot = 3, seed = 42L))
  
  run1 <- do.call(SEQuential, c(list(data = data.table::copy(SEQdata)), args))
  run2 <- do.call(SEQuential, c(list(data = data.table::copy(SEQdata)), args))
  
  expect_identical(run1@hazard, run2@hazard)
})

test_that("Hazard bootstrap percentile CIs are reproducible with same seed", {
  skip_on_cran()
  args <- list("ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
               method = "ITT", options = SEQopts(hazard = TRUE, bootstrap = TRUE, bootstrap.nboot = 3,
                                                 bootstrap.CI_method = "percentile", seed = 42L))

  run1 <- do.call(SEQuential, c(list(data = data.table::copy(SEQdata)), args))
  run2 <- do.call(SEQuential, c(list(data = data.table::copy(SEQdata)), args))

  expect_identical(run1@hazard, run2@hazard)
})

test_that("direct coxph.fit matches coxph() formula for the univariate hazard model", {
  # The non-competing-event hazard fit now calls survival::coxph.fit on a
  # prebuilt one-column design matrix instead of coxph(formula, data), to skip
  # the model.frame rebuild on every bootstrap iteration. This locks in the
  # assumption that the two give the same coefficient (guarding against a future
  # survival change), using data with the heavy ties that integer follow-up
  # produces.
  set.seed(1)
  n <- 5000
  d <- data.frame(
    followup    = sample(0:60, n, replace = TRUE),
    event       = factor(rbinom(n, 1, 0.05), levels = c(0, 1, 2)),
    tx_init_bas = rbinom(n, 1, 0.5)
  )

  b_formula <- unname(coef(
    survival::coxph(survival::Surv(followup, event == 1) ~ get("tx_init_bas"), d)
  ))

  x <- matrix(as.double(d[["tx_init_bas"]]), ncol = 1L,
              dimnames = list(NULL, "tx_init_bas"))
  y <- survival::Surv(as.double(d[["followup"]]), d[["event"]] == 1)
  b_fit <- unname(survival::coxph.fit(
    x, y, strata = NULL, offset = NULL, init = 0,
    control = survival::coxph.control(), weights = NULL,
    method = "efron", rownames = NULL
  )$coefficients)

  expect_equal(b_fit, b_formula, tolerance = 1e-8)
})
