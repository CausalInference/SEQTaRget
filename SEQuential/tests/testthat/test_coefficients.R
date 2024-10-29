test_that("ITT", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "ITT", options = SEQopts()
  )
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -5.02942217261295, sex = -0.186519877899898,
                   N_bas = 0.00591305969530564, L_bas = -0.411063838555718,
                   P_bas = -0.34380974364171, tx_init_bas = -0.038363548779841,
                   followup = -0.00291701909163772, `tx_init_bas:followup` = 0.00377865941191602)

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

  expected <- list(`(Intercept)` = -11.4417351608548, sex = -0.163192423386571,
                   followup = -0.0432922184499701, followup_sq = 0.000295330528705324,
                   period = 0.221728730290202, period_sq = -0.002610013288857,
                   dose = 0.021123755997328, dose_sq = -0.000610780720343925)

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

  expected <- list(`(Intercept)` = -7.50966403170878, sex = -1.72072288567008,
                   N_bas = -0.180878532613169, L_bas = -1.35357975725507, P_bas = -0.315567834784421,
                   followup = -0.222354534435652, followup_sq = -0.00203939581867324,
                   period = 0.102467272662354, period_sq = -0.000992805396310875,
                   dose = 0.506520565113171, dose_sq = -0.00165198936300335)

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

  expected <- list(`(Intercept)` = -8.90006271263947, sex = -0.168307226189814,
                   tx_init_bas = -1.51548502727564, followup = -0.370152693976708,
                   followup_sq = -0.00533482119536857, trial = 0.143145871365135,
                   trial_sq = -0.00163488036831193, `tx_init_bas:followup` = 0.481227538284218)

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

  expected <- list(`(Intercept)` = -19.2644245703105, sex = -0.31617598415799,
                   N_bas = 0.0136235339739534, L_bas = -0.419120847436274, P_bas = 1.12998601318899,
                   followup = -0.000801590556116782, followup_sq = -0.000672608287089215,
                   trial = 0.29253355124786, trial_sq = -0.00204302905024789,
                   tx_init_bas = -0.428678959540854, `followup:tx_init_bas` = 0.0294250606101416)

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
  expect_equal(test, expected, tolerance = 1e-2)
})
