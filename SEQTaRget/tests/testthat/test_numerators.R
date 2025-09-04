test_that("Default Numerator Creation: Pre-Expansion Dose-Response", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "dose-response", verbose = TRUE,
    opts = SEQopts(weighted = TRUE, weight.preexpansion = TRUE)
  )

  covariates <- create.default.weight.covariates(params, type = "numerator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(params@fixed, "time", "time_sq"))
  expect_true(setequal(components, expected))
})

test_that("Default Numerator Creation: Post-Expansion Dose-Response", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "dose-response", verbose = TRUE,
    opts = SEQopts(weighted = TRUE, weight.preexpansion = FALSE)
  )

  covariates <- create.default.weight.covariates(params, type = "numerator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(
    params@fixed, "followup", "followup_sq", "trial", "trial_sq",
    paste0(params@time_varying, params@indicator.baseline)
  ))
  expect_true(setequal(components, expected))
})

test_that("Default Numerator Creation: Pre-Expansion Censoring", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "censoring", verbose = TRUE,
    opts = SEQopts(weighted = TRUE, weight.preexpansion = TRUE)
  )

  covariates <- create.default.weight.covariates(params, type = "numerator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(params@fixed, "time", "time_sq"))
  expect_true(setequal(components, expected))
})

test_that("Default Numerator Creation: Post-Expansion Censoring", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "censoring", verbose = TRUE,
    opts = SEQopts(weighted = TRUE, weight.preexpansion = FALSE)
  )

  covariates <- create.default.weight.covariates(params, type = "numerator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(
    params@fixed, paste0(params@time_varying, params@indicator.baseline), "followup", "followup_sq",
    "trial", "trial_sq"
  ))
  expect_true(setequal(components, expected))
})

test_that("Default Numerator Creation: Post-Expansion Excused Censoring", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "censoring", verbose = TRUE,
    opts = SEQopts(
      weighted = TRUE, weight.preexpansion = FALSE,
      excused = TRUE
    )
  )

  covariates <- create.default.weight.covariates(params, type = "numerator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(
    params@fixed, paste0(params@time_varying, params@indicator.baseline), "followup", "followup_sq",
    "trial", "trial_sq"
  ))
  expect_true(setequal(components, expected))
})
