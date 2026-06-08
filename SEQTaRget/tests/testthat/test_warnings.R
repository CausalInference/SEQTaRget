test_that("check_separation warns on non-finite coefficients", {
  # Non-finite coefs (e.g. from a solver that diverges rather than stalls)
  fake_model <- list(coefficients = c("(Intercept)" = Inf, "x" = -2.3))
  expect_warning(
    SEQTaRget:::check_separation(fake_model, label = "test model"),
    regexp = "Perfect or quasi-complete separation"
  )
})

test_that("check_separation warns on extremely large coefficients (fastglm separation pattern)", {
  # fastglm hits its iter limit and returns large-but-finite coefs, not Inf
  fake_model <- list(coefficients = c("(Intercept)" = -26.6, "x" = 53.1))
  expect_warning(
    SEQTaRget:::check_separation(fake_model, label = "test model"),
    regexp = "Perfect or quasi-complete separation"
  )
})

test_that("check_separation does not warn for a well-behaved model", {
  fake_model <- list(coefficients = c("(Intercept)" = -0.5, "x" = 1.2))
  expect_no_warning(SEQTaRget:::check_separation(fake_model, label = "test model"))
})

test_that("Weighted ITT", {
  data <- data.table::copy(SEQdata)
  expect_warning(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "ITT",
    options = SEQopts(weighted = TRUE)
  ))
})

test_that("Unexcused Excused Censoring", {
  data <- data.table::copy(SEQdata)
  expect_warning(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE, excused = TRUE)
  ))
})

test_that("Identical numerator and denominator weight models warn about unit weights", {
  # A common typo (denominator pointed at the numerator covariates) makes the two
  # weight models identical, so every stabilized weight is exactly 1 (no weighting).
  data <- data.table::copy(SEQdata)
  expect_warning(
    SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
      method = "censoring",
      options = SEQopts(weighted = TRUE, numerator = "N+L+P", denominator = "N+L+P")),
    regexp = "identical covariates"
  )
})

test_that("Differing numerator and denominator weight models do not warn about unit weights", {
  data <- data.table::copy(SEQdata)
  ws <- testthat::capture_warnings(
    SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
      method = "censoring",
      options = SEQopts(weighted = TRUE, numerator = "sex", denominator = "N+L+P"))
  )
  expect_false(any(grepl("identical covariates", ws)))
})
