test_that("ITT", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts())
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -2.2087,
                   sex = 0.1190,
                   N_bas = 0.0035,
                   L_bas = 0.0647,
                   P_bas = -0.3157,
                   tx_init_bas = 0.1709,
                   followup = 0.0275,
                   `tx_init_bas:followup` = -0.0040)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Dose-Response", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "dose-response",
                                       options = SEQopts(weighted = TRUE)))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.3247,
                   sex = 0.1447,
                   followup = -0.0567,
                   followup_sq = 0.0002,
                   period = -0.0061,
                   period_sq = 0.0075,
                   dose = 0.0563,
                   dose_sq = -0.0006)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Dose-Response", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "dose-response",
                                       options = SEQopts(weighted = TRUE, pre.expansion = FALSE)))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -7.5894,
                   sex = 0.1503,
                   N_bas = 0.0030,
                   L_bas = -0.0147,
                   P_bas = 0.3444,
                   followup = -0.0920,
                   followup_sq = 0.0001,
                   period = 0.0430,
                   period_sq = 0.0006,
                   dose = 0.0519,
                   dose_sq = -0.0005)

  test <- as.list(model@outcome_model[[1]])
})

test_that("Pre-Expansion Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "censoring",
                                       options = SEQopts(weighted = TRUE)))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.7970,
                   sex = 0.0484,
                   tx_init_bas = 0.3981,
                   followup = 0.0136,
                   followup_sq = 0.0011,
                   trial = -0.0137,
                   trial_sq = 0.0011,
                   `tx_init_bas:followup` = 0.0172)

  test <- as.list(model@outcome_model[[1]])
})

test_that("Post-Expansion Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "censoring",
                                       options = SEQopts(weighted = TRUE, pre.expansion = FALSE)))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -9.1036,
                   sex = 0.0825,
                   N_bas = 0.0048,
                   L_bas = 0.0140,
                   P_bas = 0.4470,
                   followup = 0.0151,
                   followup_sq = 0.0000,
                   trial = 0.0666,
                   trial_sq = 0.0005,
                   tx_init_bas = 0.3899,
                   `tx_init_bas:followup` = 0.0159)

  test <- as.list(model@outcome_model[[1]])
})

test_that("Post-Expansion Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "censoring",
                                       options = SEQopts(weighted = TRUE, pre.expansion = FALSE)))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -9.1036,
                   sex = 0.0825,
                   N_bas = 0.0048,
                   L_bas = 0.0140,
                   P_bas = 0.4470,
                   followup = 0.0151,
                   followup_sq = 0.0000,
                   trial = 0.0666,
                   trial_sq = 0.0005,
                   tx_init_bas = 0.3899,
                   `tx_init_bas:followup` = 0.0159)

  test <- as.list(model@outcome_model[[1]])
})

test_that("Pre-Expansion Excused Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "censoring",
                                       options = SEQopts(weighted = TRUE, excused = TRUE,
                                                         excused.col1 = "excusedOne",
                                                         excused.col0 = "excusedZero")))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.8136,
                   followup = 0.0238,
                   followup_sq = 0.0000,
                   trial = 0.0104,
                   trial_sq = 0.0008,
                   tx_init_bas = 0.01443,
                   `tx_init_bas:followup` = 0.0029)

  test <- as.list(model@outcome_model[[1]])
})
