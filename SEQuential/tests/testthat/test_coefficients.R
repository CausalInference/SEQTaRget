test_that("ITT", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "ITT", options = SEQopts(fastglm.method = 1)
  )
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -6.26627443639013, sex = 0.127632647053313,
                   N_bas = 0.00336674007525289, L_bas = -0.00971230944593865,
                   P_bas = 0.201448183755029, period = -0.00966160482864084,
                   period_sq = 0.000474434602091899, trial = 0.033033635239569,
                   trial_sq = 0.000335479193077798, `tx_init_bas:followup` = 0.00460177613319777)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Dose-Response", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "dose-response",
    options = SEQopts(weighted = TRUE, fastglm.method = 1)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.32474356846359, sex = 0.144717719030081,
                   followup = -0.056728478677267, followup_sq = 0.000244796209735332,
                   period = -0.00610756989436788, period_sq = 0.000750893537720868,
                   dose = 0.0563740546694681, dose_sq = -0.000625968782842412)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Dose-Response", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "dose-response",
    options = SEQopts(weighted = TRUE, pre.expansion = FALSE, fastglm.method = 1)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -7.58943493090689, sex = 0.150337796151131,
                   N_bas = 0.00305026238665071, L_bas = -0.014701011756117,
                   P_bas = 0.344448960986518, followup = -0.0920465165286913,
                   followup_sq = 0.000106744135811356, period = 0.0430881040917413,
                   period_sq = 0.000671193509297932, dose = 0.0519502786426668,
                   dose_sq = -0.000540087391733833)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE, fastglm.method = 1)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.79700899537035, sex = 0.0484052979031926,
                   tx_init_bas = 0.398141240108889, followup = 0.0136455646615077,
                   followup_sq = 1.10939448748916e-05, trial = -0.0137282592316021,
                   trial_sq = 0.00113039188898424, `tx_init_bas:followup` = 0.0172102945332294)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Censoring", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE, pre.expansion = FALSE, fastglm.method = 1)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -8.77850126329417, sex = 0.0952139174831454,
                   N_bas = 0.00479928263067027, L_bas = 0.0161239204223941,
                   P_bas = 0.441140738518659, followup = -0.0159571453043091,
                   followup_sq = -2.06632166798897e-05, trial = 0.0695842214362399,
                   trial_sq = 0.000530823802052693, `followup:tx_init_bas` = 0.0523774093115016)

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
      excused.col0 = "excusedZero", fastglm.method = 1
    )
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.81364966959224, tx_init_bas = 0.144370683209823,
                   followup = 0.0238113576764377, followup_sq = 1.92156796528875e-05,
                   trial = 0.0104610628276582, trial_sq = 0.000813014415799294,
                   `tx_init_bas:followup` = 0.00297084310612618)

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
      pre.expansion = FALSE,
      fastglm.method = 1
    )
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -8.21350777019439, sex = 0.151849240310461,
                   N_bas = 0.00160182678590164, L_bas = 0.00773401847222666,
                   P_bas = 0.36011742436451, followup = 0.0308627357673543,
                   followup_sq = -0.000124886225032565, trial = 0.0684296384691867,
                   trial_sq = 0.000464686422291178, `followup:tx_init_bas` = 0.00465482408840434)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion ITT (Cense 1 - LTFU)", {
  data <- SEQdata.LTFU
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", pre.expansion = TRUE, fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -5.04322109457601, sex = -0.187152435515976,
                   N_bas = 0.00590440085361474, L_bas = -0.411252486037057,
                   P_bas = -0.349498295645025, `tx_init_bas:followup` = 0.00139638835609859)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion ITT (Cense 1 - LTFU)", {
  data <- SEQdata.LTFU
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", pre.expansion = FALSE, fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -21.5119227047619, sex = -0.190195216530171,
                   N_bas = 0.00658336488940953, L_bas = -0.448962845953137,
                   P_bas = 1.39187856559769, trial = 0.293745487674761, trial_sq = -0.00152815760657456,
                   `tx_init_bas:followup` = 0.00405428865377567)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})
