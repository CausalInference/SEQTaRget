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

  expected <- c("treatment_bas", "period", "period_sq", "trial", "trial_sq",
                "sex", "race", "N_bas", "L_bas", "P_bas")
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

  expected <- c("dose", "dose_sq", "period", "period_sq", "trial", "trial_sq",
                "sex", "race", "period*dose", "period*dose_sq")
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

  expected <- c("dose", "dose_sq", "period", "period_sq", "trial", "trial_sq",
                "sex", "race", "N_bas", "L_bas", "P_bas", "period*dose", "period*dose_sq")

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

  expected <- c("treatment_bas", "period", "period_sq", "trial", "trial_sq",
                "sex", "race", "treatment_bas*period")
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

  expected <- c("treatment_bas", "period", "period_sq", "trial", "trial_sq",
                "sex", "race", "N_bas", "L_bas", "P_bas", "treatment_bas*period")
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

  expected <- c("treatment_bas", "period", "period_sq", "trial", "trial_sq",
                "treatment_bas*period")
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

  expected <- c("treatment_bas", "period", "period_sq", "trial", "trial_sq",
                "sex", "race", "N_bas", "L_bas", "P_bas", "treatment_bas*period")
  expect_true(setequal(components, expected))
})
