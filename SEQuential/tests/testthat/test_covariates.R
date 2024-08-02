test_that("Default Covariate Creation: ITT", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "ITT", opts = SEQopts()
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(
    params@fixed, paste0(params@time_varying, params@baseline.indicator),
    paste0(params@treatment, params@baseline.indicator, "*", "followup")
  ))
  expect_true(setequal(components, expected))
})

test_that("Default Covariate Creation: Pre-Expansion Dose-Response", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "dose-response", opts = SEQopts(
      pre.expansion = TRUE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c("dose", "dose_sq", "followup", "followup_sq", "period", "period_sq", params@fixed))
  expect_true(setequal(components, expected))
})

test_that("Default Covariate Creation: Post-Expansion Dose-Response", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "dose-response", opts = SEQopts(
      pre.expansion = FALSE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(
    "dose", "dose_sq", "followup", "followup_sq", "period", "period_sq", params@fixed,
    paste0(params@time_varying, params@baseline.indicator)
  ))
  expect_true(setequal(components, expected))
})

test_that("Default Covariate Creation: Pre-Expansion Censoring", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "censoring", opts = SEQopts(
      pre.expansion = TRUE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(
    params@fixed, paste0(params@treatment, params@baseline.indicator), "followup", "followup_sq", "trial", "trial_sq",
    paste0(params@treatment, params@baseline.indicator, "*", "followup")
  ))
  expect_true(setequal(components, expected))
})

test_that("Default Covariate Creation: Post-Expansion Censoring", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "censoring", opts = SEQopts(
      pre.expansion = FALSE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(
    params@fixed, paste0(params@time_varying, params@baseline.indicator), "followup", "followup_sq", "trial", "trial_sq",
    paste0(params@treatment, params@baseline.indicator, "*", "followup")
  ))
  expect_true(setequal(components, expected))
})

test_that("Default Covariate Creation: Pre-Expansion Excused Censoring", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "censoring", opts = SEQopts(
      pre.expansion = TRUE,
      excused = TRUE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(
    paste0(params@treatment, params@baseline.indicator), "followup", "followup_sq", "trial", "trial_sq",
    paste0(params@treatment, params@baseline.indicator, "*", "followup")
  ))
  expect_true(setequal(components, expected))
})

test_that("Default Covariate Creation: Post-Expansion Excused Censoring", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "censoring", opts = SEQopts(
      pre.expansion = FALSE,
      excused = TRUE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(
    params@fixed, paste0(params@time_varying, params@baseline.indicator), "followup", "followup_sq", "trial", "trial_sq",
    paste0(params@treatment, params@baseline.indicator, "*", "followup"), paste0(params@time_varying, params@baseline.indicator)
  ))
  expect_true(setequal(components, expected))
})
