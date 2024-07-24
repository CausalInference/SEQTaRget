test_that("Default Denominator Creation: Pre-Expansion Dose-Response", {

  params <- parameter.setter(data = data.table(),
                             DT = data.table(),
                             id.col = "ID",
                             time.col = "time", eligible.col = "eligible",
                             outcome.col = "outcome", treatment.col = "treatment",
                             time_varying.cols = list("N", "L", "P"),
                             fixed.cols = list("sex", "race"),
                             method = "dose-response",
                             opts = SEQopts(weighted = TRUE, pre.expansion = TRUE))

  covariates <- create.default.weight.covariates(params, type = "denominator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(params@fixed, params@time_varying, "time", "time_sq"))
  expect_true(setequal(components, expected))
})

test_that("Default Denominator Creation: Post-Expansion Dose-Response", {

  params <- parameter.setter(data = data.table(),
                             DT = data.table(),
                             id.col = "ID",
                             time.col = "time", eligible.col = "eligible",
                             outcome.col = "outcome", treatment.col = "treatment",
                             time_varying.cols = list("N", "L", "P"),
                             fixed.cols = list("sex", "race"),
                             method = "dose-response",
                             opts = SEQopts(weighted = TRUE, pre.expansion = FALSE))

  covariates <- create.default.weight.covariates(params, type = "denominator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(params@fixed, params@time_varying,
                       "followup", "followup_sq", "trial", "trial_sq",
                       paste0(params@time_varying, params@baseline.indicator)))
  expect_true(setequal(components, expected))
})

test_that("Default Denominator Creation: Pre-Expansion Censoring", {

  params <- parameter.setter(data = data.table(),
                             DT = data.table(),
                             id.col = "ID",
                             time.col = "time", eligible.col = "eligible",
                             outcome.col = "outcome", treatment.col = "treatment",
                             time_varying.cols = list("N", "L", "P"),
                             fixed.cols = list("sex", "race"),
                             method = "censoring",
                             opts = SEQopts(weighted = TRUE, pre.expansion = TRUE))

  covariates <- create.default.weight.covariates(params, type = "denominator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(params@fixed, params@time_varying, "time", "time_sq"))
  expect_true(setequal(components, expected))
})

test_that("Default Denominator Creation: Post-Expansion Censoring", {

  params <- parameter.setter(data = data.table(),
                             DT = data.table(),
                             id.col = "ID",
                             time.col = "time", eligible.col = "eligible",
                             outcome.col = "outcome", treatment.col = "treatment",
                             time_varying.cols = list("N", "L", "P"),
                             fixed.cols = list("sex", "race"),
                             method = "censoring",
                             opts = SEQopts(weighted = TRUE, pre.expansion = FALSE))

  covariates <- create.default.weight.covariates(params, type = "denominator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(params@fixed, params@time_varying,
                       paste0(params@time_varying, params@baseline.indicator),
                       "followup", "followup_sq", "trial", "trial_sq"))
  expect_true(setequal(components, expected))
})

test_that("Default Denominator Creation: Pre-Expansion Excused Censoring", {

  params <- parameter.setter(data = data.table(),
                             DT = data.table(),
                             id.col = "ID",
                             time.col = "time", eligible.col = "eligible",
                             outcome.col = "outcome", treatment.col = "treatment",
                             time_varying.cols = list("N", "L", "P"),
                             fixed.cols = list("sex", "race"),
                             method = "censoring",
                             opts = SEQopts(weighted = TRUE, pre.expansion = TRUE,
                                            excused = TRUE))

  covariates <- create.default.weight.covariates(params, type = "denominator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(params@fixed, params@time_varying, "time", "time_sq"))
  expect_true(setequal(components, expected))
})

test_that("Default Denominator Creation: Post-Expansion Excused Censoring", {

  params <- parameter.setter(data = data.table(),
                             DT = data.table(),
                             id.col = "ID",
                             time.col = "time", eligible.col = "eligible",
                             outcome.col = "outcome", treatment.col = "treatment",
                             time_varying.cols = list("N", "L", "P"),
                             fixed.cols = list("sex", "race"),
                             method = "censoring",
                             opts = SEQopts(weighted = TRUE, pre.expansion = FALSE,
                                            excused = TRUE))

  covariates <- create.default.weight.covariates(params, type = "denominator")
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- unlist(c(params@fixed, paste0(params@time_varying, params@baseline.indicator), "followup", "followup_sq",
                       "trial_sq", "trial"))
  expect_true(setequal(components, expected))
})
