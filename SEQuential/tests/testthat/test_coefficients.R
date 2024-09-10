test_that("ITT", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "ITT", options = SEQopts()
  )
  expect_s4_class(model, "SEQoutput")

  expected <- list(
    `(Intercept)` = -2.2087,
    sex = 0.1190,
    N_bas = 0.0035,
    L_bas = 0.0647,
    P_bas = -0.3157,
    tx_init_bas = 0.1709,
    followup = 0.0275,
    `tx_init_bas:followup` = -0.0040
  )

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Dose-Response", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "dose-response",
    options = SEQopts(weighted = TRUE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(
    `(Intercept)` = -4.3247,
    sex = 0.1447,
    followup = -0.0567,
    followup_sq = 0.0002,
    period = -0.0061,
    period_sq = 0.0075,
    dose = 0.0563,
    dose_sq = -0.0006
  )

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Dose-Response", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "dose-response",
    options = SEQopts(weighted = TRUE, pre.expansion = FALSE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(
    `(Intercept)` = -7.5894,
    sex = 0.1503,
    N_bas = 0.0030,
    L_bas = -0.0147,
    P_bas = 0.3444,
    followup = -0.0920,
    followup_sq = 0.0001,
    period = 0.0430,
    period_sq = 0.0006,
    dose = 0.0519,
    dose_sq = -0.0005
  )

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(
    `(Intercept)` = -4.7970,
    sex = 0.0484,
    tx_init_bas = 0.3981,
    followup = 0.0136,
    followup_sq = 0.0011,
    trial = -0.0137,
    trial_sq = 0.0011,
    `tx_init_bas:followup` = 0.0172
  )

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE, pre.expansion = FALSE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(
    `(Intercept)` = -9.1036,
    sex = 0.0898,
    N_bas = 0.0048,
    L_bas = 0.0121,
    P_bas = 0.4412,
    followup = 0.0162,
    followup_sq = 0.0001,
    trial = 0.0668,
    trial_sq = 0.0005,
    tx_init_bas = 0.3962,
    `followup:tx_init_bas` = 0.0135
  )

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Excused Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.col1 = "excusedOne",
      excused.col0 = "excusedZero"
    )
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(
    `(Intercept)` = -4.8136,
    tx_init_bas = 0.1440,
    followup = 0.0238,
    followup_sq = 0.0000,
    trial = 0.0104,
    trial_sq = 0.0008,
    `tx_init_bas:followup` = 0.0029
  )

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Excused Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.col1 = "excusedOne",
      excused.col0 = "excusedZero",
      pre.expansion = FALSE
    )
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(
    `(Intercept)` = -8.3349,
    sex = 0.1507,
    N_bas = 0.0015,
    L_bas = 0.0027,
    P_bas = 0.3625,
    followup = 0.0387,
    followup_sq = -0.0001,
    trial = 0.0667,
    trial_sq = 0.0005,
    tx_init_bas = 0.2652,
    `followup:tx_init_bas` = -0.0040
  )
  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion ITT (Cense 1 - LTFU)", {
  data <- SEQdata.LTFU
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", pre.expansion = TRUE))

  expect_s4_class(model, "SEQoutput")

  expected <- list(
    `(Intercept)` = -1.4348244162825,
    sex = 0.0578472989119999,
    N_bas = 6.62467716389115e-06,
    L_bas = -0.0569574476787094,
    P_bas = -0.396831308904797,
    tx_init_bas = -0.0393725815326663,
    followup = 0.0266190146171601,
    `tx_init_bas:followup` = -0.000581459477945257)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion ITT (Cense 1 - LTFU)", {
  data <- SEQdata.LTFU
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", pre.expansion = FALSE))

  expect_s4_class(model, "SEQoutput")

  expected <- list(
    `(Intercept)` = -6.20645685197078,
    sex = 0.047444072162239,
    N_bas = 0.000581363083345401,
    L_bas = -0.159943984561706,
    P_bas = 0.157473854878466,
    trial = 0.0470295163644785,
    trial_sq = 0.000655423692321318,
    tx_init_bas = -0.163600019438483,
    followup = 0.0229638301738138,
    `tx_init_bas:followup` = 0.00839924678598092)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, 1e-2)
})
