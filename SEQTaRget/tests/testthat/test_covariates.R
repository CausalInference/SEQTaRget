test_that("Default Covariate Creation: ITT", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race"),
    method = "ITT", verbose = TRUE, opts = SEQopts()
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- c("treatment_bas", "followup", "followup_sq", "trial", "trial_sq",
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
    method = "dose-response", verbose = TRUE, 
    opts = SEQopts(
      weight.preexpansion = TRUE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- c("dose", "dose_sq", "followup", "followup_sq", "trial", "trial_sq",
                "sex", "race")
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
    method = "dose-response", verbose = TRUE, 
    opts = SEQopts(
      weight.preexpansion = FALSE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- c("dose", "dose_sq", "followup", "followup_sq", "trial", "trial_sq",
                "sex", "race", "N_bas", "L_bas", "P_bas")

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
    method = "censoring", verbose = TRUE, 
    opts = SEQopts(
      weight.preexpansion = TRUE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- c("treatment_bas", "followup", "followup_sq", "trial", "trial_sq",
                "sex", "race")
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
    method = "censoring", verbose = TRUE,
    opts = SEQopts(
      weight.preexpansion = FALSE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- c("treatment_bas", "followup", "followup_sq", "trial", "trial_sq",
                "sex", "race", "N_bas", "L_bas", "P_bas")
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
    method = "censoring", verbose = TRUE,
    opts = SEQopts(
      weight.preexpansion = TRUE,
      excused = TRUE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- c("treatment_bas", "followup", "followup_sq", "trial", "trial_sq")
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
    method = "censoring", verbose = TRUE,
    opts = SEQopts(
      weight.preexpansion = FALSE,
      excused = TRUE,
      weighted = TRUE
    )
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- c("treatment_bas", "followup", "followup_sq", "trial", "trial_sq",
                "sex", "race", "N_bas", "L_bas", "P_bas")
  expect_true(setequal(components, expected))
})
