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

test_that("factorize() leaves numeric fixed covariates numeric and encodes non-numeric ones as factors", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "grade"),
    fixed.cols = list("age", "sex", "region", "smoker"),
    method = "ITT", verbose = TRUE, opts = SEQopts()
  )
  DT <- data.table(treatment = c(0, 1, 1), treatment_bas = c(0, 0, 1),
                   age = c(34.5, 61.2, 47.8), sex = c(0L, 1L, 1L),
                   region = c("north", "south", "north"), smoker = c(TRUE, FALSE, TRUE),
                   N = c(1.2, 3.4, 5.6), N_bas = c(1.2, 1.2, 5.6),
                   grade = c("a", "b", "b"), grade_bas = c("a", "a", "b"))
  out <- factorize(DT, params)

  # Treatment is always categorical
  expect_true(is.factor(out$treatment))
  expect_true(is.factor(out$treatment_bas))
  # Numeric covariates are untouched, whether fixed or time-varying
  expect_identical(out$age, c(34.5, 61.2, 47.8))
  expect_identical(out$sex, c(0L, 1L, 1L))
  expect_identical(out$N, c(1.2, 3.4, 5.6))
  expect_identical(out$N_bas, c(1.2, 1.2, 5.6))
  # Non-numeric covariates are encoded as factors
  expect_true(is.factor(out$region))
  expect_true(is.factor(out$smoker))
  expect_true(is.factor(out$grade))
  expect_true(is.factor(out$grade_bas))
})

test_that("A continuous fixed covariate enters the outcome model as a single term", {
  data <- data.table::copy(SEQdata)
  set.seed(1)
  ages <- data.table(ID = unique(data$ID), age = round(runif(uniqueN(data$ID), 20, 80), 1))
  data <- ages[data, on = "ID"]
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex", "age"),
    method = "ITT", options = SEQopts(), verbose = FALSE
  ))
  coefs <- names(coef(model@outcome.model[[1]][[1]]))
  expect_true("age" %in% coefs)
  expect_equal(sum(startsWith(coefs, "age")), 1L)
  expect_true("sex" %in% coefs)
})

test_that("Default Covariate Creation: subgroup drops only the subgroup variable from the fixed covariates", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex", "race", "age"),
    method = "ITT", verbose = TRUE, opts = SEQopts(subgroup = "sex")
  )
  covariates <- create.default.covariates(params)
  components <- unlist(strsplit(covariates, "\\+"))

  expected <- c("treatment_bas", "followup", "followup_sq", "trial", "trial_sq",
                "race", "age", "N_bas", "L_bas", "P_bas")
  expect_true(setequal(components, expected))
})

test_that("Transformed terms such as log(x) are not treated as simple additive formulas", {
  params <- parameter.setter(
    data = data.table(),
    DT = data.table(),
    id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"),
    fixed.cols = list("sex"),
    method = "ITT", verbose = FALSE,
    opts = SEQopts(covariates = "sex + N", cense.numerator = "sex + log(P)",
                   cense.denominator = "sex + P - 1", visit.numerator = "sex + offset(P)",
                   visit.denominator = "sex + N:P")
  )
  cache <- init_formula_cache(params)
  expect_true(cache$covariates$is_simple)
  expect_false(cache$cense_numerator$is_simple)
  expect_false(cache$cense_denominator$is_simple)
  expect_false(cache$visit_numerator$is_simple)
  expect_false(cache$visit_denominator$is_simple)

  X <- fast_model_matrix(cache$cense_numerator$formula, data.table(sex = c(0, 1), P = c(1, exp(1))),
                         cache$cense_numerator$cols, is_simple = cache$cense_numerator$is_simple)
  expect_equal(unname(X[, "log(P)"]), c(0, 1))
})

test_that("log(x) and I(log(x)) in a weight model give identical results", {
  skip_on_cran()
  fit <- function(denom) {
    suppressWarnings(SEQuential(copy(SEQdata.LTFU), "ID", "time", "eligible", "tx_init", "outcome",
               list("N", "L", "P"), list("sex"),
               method = "ITT", verbose = FALSE,
               options = SEQopts(cense = "LTFU", cense.denominator = denom)))
  }
  a <- fit("sex + N + L + log(P) + time + time_sq")
  b <- fit("sex + N + L + I(log(P)) + time + time_sq")
  expect_equal(unname(coef(a@outcome.model[[1]][[1]])), unname(coef(b@outcome.model[[1]][[1]])))
})
