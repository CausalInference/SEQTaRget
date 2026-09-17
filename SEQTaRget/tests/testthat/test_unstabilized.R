# Unstabilized IP weights: weight.stabilized = FALSE fixes the treatment-weight
# numerator at 1, so the weights are cumulative products of 1/denominator.

test_that("weight.stabilized = FALSE gives numerator-1 weights from the same denominators", {
  skip_on_cran()
  run <- function(...) suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                        list("N", "L", "P"), list("sex"), method = "censoring",
                        options = SEQopts(weighted = TRUE, seed = 1636, denominator = "N+L+P+sex",
                                          data.return = TRUE, ...), verbose = FALSE))
  stab <- run(numerator = "sex")
  unstab <- run(weight.stabilized = FALSE)

  key <- c("ID", "trial", "period")
  merged <- merge(stab@DT[, c(key, "numerator", "denominator"), with = FALSE],
                  unstab@DT[, c(key, "numerator", "denominator"), with = FALSE],
                  by = key, suffixes = c(".stab", ".unstab"))
  expect_true(merged[, all(numerator.unstab == 1)])
  # Same denominator model, so identical denominators; only the numerator differs
  expect_equal(merged$denominator.stab, merged$denominator.unstab)

  # Weights are the running product of 1/denominator within each trial
  d <- unstab@DT[ID == unstab@DT$ID[1]][trial == trial[1]][order(followup)]
  expect_equal(d$weight, cumprod(fifelse(d$followup == 0, 1, 1 / d$denominator)))

  # No numerator model is fit, and the reported numerator formula is the constant 1
  expect_true(all(is.na(unstab@weight.statistics[[1]][[1]]$coef.numerator)))
  expect_equal(unstab@numerator, "tx_init~1")
  expect_output(show(unstab))
})

test_that("weight.stabilized = FALSE warns where it cannot apply, and when numerator is supplied", {
  params <- parameter.setter(copy(SEQdata), DT = data.table(), "ID", "time", "eligible", "outcome", "tx_init",
                             list("N", "L", "P"), list("sex"), method = "ITT", verbose = TRUE,
                             opts = SEQopts(weight.stabilized = FALSE))
  expect_warning(parameter.simplifier(params), "has no effect")

  capture.output(expect_warning(
    SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
               list("N", "L", "P"), list("sex"), method = "censoring",
               options = SEQopts(weighted = TRUE, weight.stabilized = FALSE,
                                 numerator = "sex", denominator = "N+L+P+sex"), verbose = TRUE),
    "'numerator' is ignored"))
})
