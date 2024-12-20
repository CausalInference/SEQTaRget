test_that("ITT", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "ITT", options = SEQopts()
  )
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -6.85931554847644, tx_init_bas = 0.225309379527041,
                   followup = 0.0353817161706519, followup_sq = -0.000159868670243376,
                   trial = 0.0447178957317482, trial_sq = 0.00057616846490925,
                   sex = 0.127045833687237, N_bas = 0.00328670775503307, L_bas = -0.0138508823648217,
                   P_bas = 0.200928902773364, `tx_init_bas:followup` = -0.00170402147034585)

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

  expected <- list(`(Intercept)` = -4.86763263204364, dose = 0.0587390562206159,
                   dose_sq = -0.00118380141790317, followup = -0.00431945932798065,
                   followup_sq = -5.55487092255259e-05, trial = 0.0105381474212626,
                   trial_sq = 0.000777410850631594, sex = 0.143071081126181,
                   `dose:followup` = 0.000410848850309337, `dose_sq:followup` = 6.47486169924423e-06)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Dose-Response", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "dose-response",
    options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -6.30496019430908, dose = 0.0563118272591801,
                   dose_sq = -0.00108341694741683, followup = -0.000385444751753247,
                   followup_sq = -2.76279122301102e-05, trial = 0.038663860324228,
                   trial_sq = 0.000592371347249259, sex = 0.140800782221396,
                   N_bas = 0.00294526675426801, L_bas = -0.0198798285825866,
                   P_bas = 0.148759071019851, `dose:followup` = 0.00016411135430163,
                   `dose_sq:followup` = 9.06019295358436e-06)

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

  expected <- list(`(Intercept)` = -4.79700899537026, tx_init_bas = 0.398141240108939,
                   followup = 0.0136455646615243, followup_sq = 1.10939448747926e-05,
                   trial = -0.0137282592316101, trial_sq = 0.00113039188898435,
                   sex = 0.0484052979031332, `tx_init_bas:followup` = 0.0172102945332125)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -9.06477775509384, tx_init_bas = 0.39626255316031,
                   followup = 0.0161578399116614, followup_sq = 9.81440250280792e-05,
                   trial = 0.0668234881353082, trial_sq = 0.000576688612022773,
                   sex = 0.0898855112932864, N_bas = 0.00476008107851383, L_bas = 0.0121635809493038,
                   P_bas = 0.441266300593487, `tx_init_bas:followup` = 0.0134658425932256)

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
      excused.col0 = "excusedZero")
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.81364966959259, tx_init_bas = 0.14437068321007,
                   followup = 0.0238113576764499, followup_sq = 1.92156796527957e-05,
                   trial = 0.01046106282765, trial_sq = 0.000813014415799579,
                   `tx_init_bas:followup` = 0.00297084310612163)

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
      weight.preexpansion = FALSE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -8.41659937250324, tx_init_bas = 0.266541378237528,
                   followup = 0.0387265194721102, followup_sq = -0.000162098583307465,
                   trial = 0.0667353767560562, trial_sq = 0.000501526880018303,
                   sex = 0.150727875314015, N_bas = 0.00151683958054008, L_bas = 0.00275340474918396,
                   P_bas = 0.362537379927435, `tx_init_bas:followup` = -0.0041408873576683)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion ITT (Cense 1 - LTFU)", {
  data <- SEQdata.LTFU
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", weight.preexpansion = TRUE, fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -21.5961979249185, tx_init_bas = -0.00900593453254002,
                   followup = 0.0253307575432704, followup_sq = -0.000556227879884725,
                   trial = 0.285535103308443, trial_sq = -0.00136624353554935,
                   sex = -0.190092831118129, N_bas = 0.00658683930721275, L_bas = -0.448911874075032,
                   P_bas = 1.38926265916341, `tx_init_bas:followup` = 0.00383613762896999)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion ITT (Cense 1 - LTFU)", {
  data <- SEQdata.LTFU
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", weight.preexpansion = FALSE, fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -21.594890540512, tx_init_bas = -0.0090653673782976,
                   followup = 0.0253239238090987, followup_sq = -0.000556027956315004,
                   trial = 0.285516825890481, trial_sq = -0.00136620665011392,
                   sex = -0.190220229415778, N_bas = 0.00658300214440885, L_bas = -0.448958593335045,
                   P_bas = 1.38913912261648, `tx_init_bas:followup` = 0.00383774199912364)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("ITT - Multinomial, Treatment Levels 1,2", {
  data <- SEQdata.multitreatment
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(multinomial = TRUE, treat.level = c(1,2)))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -25.4995812185046, tx_init_bas = -12.8546300966185,
                     followup = -1.84645713547047, followup_sq = -0.023387257908664,
                     trial = 0.272450214929289, trial_sq = -0.00390896317632053, sex = 17.495483770532,
                     N_bas = 0.0548857956131868, L_bas = 0.809008246889685, P_bas = 1.45718621133478,
                     `tx_init_bas:followup` = 1.48015622295032)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})
