# Per-treatment-level weight models: 'numerator'/'denominator' given as a
# character vector fit a separate model (with its own covariates) in each
# treatment arm. Post-expansion weights only.

test_that("Per-arm denominator formulas fit different covariates in each arm", {
  skip_on_cran()
  model <- suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "censoring",
                      options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE,
                                        numerator = c("sex", "sex"),
                                        denominator = c("N+sex", "N+L+P+sex")),
                      verbose = FALSE))
  expect_s4_class(model, "SEQoutput")

  ws <- model@weight.statistics[[1]][[1]]
  den0 <- names(coef(ws$coef.denominator[[1]]))
  den1 <- names(coef(ws$coef.denominator[[2]]))

  # Arm 0's model excludes L and P; arm 1's includes them
  expect_false(any(grepl("^L", den0)))
  expect_false(any(grepl("^P", den0)))
  expect_true(any(grepl("^L", den1)))
  expect_true(any(grepl("^P", den1)))
  expect_true(any(grepl("^N", den0)))

  # The output formula slots carry one formula per arm
  expect_length(model@denominator, 2)
  expect_output(show(model))
  expect_length(covariates(model), 3)
})

test_that("Per-arm formulas with identical elements reproduce the shared-formula fit", {
  skip_on_cran()
  shared <- suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                       list("N", "L", "P"), list("sex"),
                       method = "censoring",
                       options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE,
                                         numerator = "sex", denominator = "N+L+P+sex"),
                       verbose = FALSE))
  perarm <- suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                       list("N", "L", "P"), list("sex"),
                       method = "censoring",
                       options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE,
                                         numerator = c("sex", "sex"),
                                         denominator = c("N+L+P+sex", "N+L+P+sex")),
                       verbose = FALSE))

  ws.shared <- shared@weight.statistics[[1]][[1]]
  ws.perarm <- perarm@weight.statistics[[1]][[1]]
  for (i in 1:2) {
    expect_equal(coef(ws.shared$coef.numerator[[i]]),   coef(ws.perarm$coef.numerator[[i]]))
    expect_equal(coef(ws.shared$coef.denominator[[i]]), coef(ws.perarm$coef.denominator[[i]]))
  }

  # Same weights feed the outcome model, so its coefficients must also match
  expect_equal(coef(shared@outcome.model[[1]][[1]]$model),
               coef(perarm@outcome.model[[1]][[1]]$model))
})

test_that("Per-arm weight formulas are rejected for pre-expansion weights", {
  expect_error(
    SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
               list("N", "L", "P"), list("sex"),
               method = "censoring",
               options = SEQopts(weighted = TRUE, weight.preexpansion = TRUE,
                                 denominator = c("N+sex", "N+L+P+sex")),
               verbose = FALSE),
    "post-expansion"
  )
})

test_that("Per-arm weight formulas must match the number of treatment levels", {
  expect_error(
    SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
               list("N", "L", "P"), list("sex"),
               method = "censoring",
               options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE,
                                 denominator = c("N+sex", "N+L+sex", "N+L+P+sex")),
               verbose = FALSE),
    "one per treatment level"
  )
})

test_that("Per-arm weight formulas require a weighted, non-ITT analysis", {
  expect_error(
    suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
               list("N", "L", "P"), list("sex"),
               method = "ITT",
               options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE,
                                 denominator = c("N+sex", "N+L+P+sex")),
               verbose = FALSE)),
    "weighted, non-ITT"
  )
})

test_that("Identical per-arm numerator and denominator formulas warn", {
  # verbose = TRUE so the column-pruning notice is cat()ed rather than raised
  # as a warning, leaving the identical-covariates warning as the only one
  capture.output(expect_warning(
    SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
               list("N", "L", "P"), list("sex"),
               method = "censoring",
               options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE,
                                 numerator = c("N+sex", "sex"),
                                 denominator = c("N+sex", "N+L+P+sex")),
               verbose = TRUE),
    "identical covariates"
  ))
})
