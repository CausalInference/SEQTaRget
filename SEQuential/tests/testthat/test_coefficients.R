test_that("ITT", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "ITT", options = SEQopts(glm.fitter = "fastglm", fastglm.method = 1)
  )
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -2.20875780252027, sex = 0.119046760181495,
                   N_bas = 0.00351775314968957, L_bas = 0.0647008384198997,
                   P_bas = -0.315666063832447, tx_init_bas = 0.170901420441045,
                   followup = 0.0275952851415662, `tx_init_bas:followup` = -0.00403797501315619)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Dose-Response", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "dose-response",
    options = SEQopts(weighted = TRUE, glm.fitter = "fastglm", fastglm.method = 1)
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
    options = SEQopts(weighted = TRUE, pre.expansion = FALSE, glm.fitter = "fastglm", fastglm.method = 1)
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
    options = SEQopts(weighted = TRUE, glm.fitter = "fastglm", fastglm.method = 1)
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
    options = SEQopts(weighted = TRUE, pre.expansion = FALSE, glm.fitter = "fastglm", fastglm.method = 1)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -9.06477775511434, sex = 0.089885511293321,
                   N_bas = 0.00476008107851924, L_bas = 0.0121635809492721,
                   P_bas = 0.441266300595648, followup = 0.0161578399116783,
                   followup_sq = 9.81440250274935e-05, trial = 0.0668234881356795,
                   trial_sq = 0.000576688612020573, tx_init_bas = 0.396262553160296,
                   `followup:tx_init_bas` = 0.0134658425932361)

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
      excused.col0 = "excusedZero",
      glm.fitter = "fastglm", fastglm.method = 1
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
      glm.fitter = "fastglm", fastglm.method = 1
    )
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -8.41659937254366, sex = 0.150727875314278,
                   N_bas = 0.00151683958054173, L_bas = 0.00275340474922261,
                   P_bas = 0.362537379931659, followup = 0.0387265194721085,
                   followup_sq = -0.000162098583307205, trial = 0.06673537675684,
                   trial_sq = 0.000501526880012748, tx_init_bas = 0.266541378237597,
                   `followup:tx_init_bas` = -0.00414088735767523)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion ITT (Cense 1 - LTFU)", {
  data <- SEQdata.LTFU
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", pre.expansion = TRUE, glm.fitter = "fastglm", fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -5.02942424811771, sex = -0.186503833847345,
                   N_bas = 0.0059133633570627, L_bas = -0.41107354109782, P_bas = -0.343809466045548,
                   tx_init_bas = -0.0383599016851488, followup = -0.00291742341595245,
                   `tx_init_bas:followup` = 0.00377859672149782)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion ITT (Cense 1 - LTFU)", {
  data <- SEQdata.LTFU
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", pre.expansion = FALSE, glm.fitter = "fastglm", fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -21.4654805657124, sex = -0.190318475661994,
                   N_bas = 0.00657926148709646, L_bas = -0.449024852535419,
                   P_bas = 1.3920709427832, trial = 0.293748425298631, trial_sq = -0.00152812560581487,
                   tx_init_bas = -0.0588007711996982, followup = -0.00179227197321662,
                   `tx_init_bas:followup` = 0.0062424347680558)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})
