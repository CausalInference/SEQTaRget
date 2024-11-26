test_that("ITT", {
  data <- SEQdata
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "ITT", options = SEQopts(fastglm.method = 1)
  )
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -6.33307227325846, tx_init_bas = 0.190877159771496,
                   period = -0.00916312831659826, period_sq = 0.000506882961739945,
                   trial = 0.0281051448214869, trial_sq = 0.000366516467528188,
                   sex = 0.126639104874727, N_bas = 0.00334549382844125, L_bas = -0.0124774991554364,
                   P_bas = 0.201463911076644)

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

  expected <- list(`(Intercept)` = -4.33446453752286, dose = 0.0717751517889089,
                   dose_sq = -0.000761820851833014, period = -0.0477535251697336,
                   period_sq = 0.00076630330731055, trial = 0.0191278336187042,
                   trial_sq = 0.00054676427175286, sex = 0.142955212299474,
                   `dose:period` = -0.000513899775244349, `dose_sq:period` = 7.00540026095867e-06)

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

  expected <- list(`(Intercept)` = -5.85446995029802, dose = 0.0813732729612705,
                   dose_sq = -0.00122027806193458, period = -0.0446117473973746,
                   period_sq = 0.000762897663437379, trial = 0.0460264356946087,
                   trial_sq = 0.000356238369324901, sex = 0.141738077855886,
                   N_bas = 0.0030986654489113, L_bas = -0.0178609021399875,
                   P_bas = 0.153750968891163, `dose:period` = -0.0007853241216421,
                   `dose_sq:period` = 1.71865370793618e-05)

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

  expected <- list(`(Intercept)` = -4.53568971410011, tx_init_bas = 0.23738502714034,
                   period = 0.00593707276865754, period_sq = 0.000246528000093099,
                   trial = -0.0355737036143195, trial_sq = 0.000967157394476585,
                   sex = 0.0425290618544458, `tx_init_bas:period` = 0.0096070869424151)

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

  expected <- list(`(Intercept)` = -8.84855794932114, tx_init_bas = 0.296037001026725,
                   period = 0.000866110574723778, period_sq = 0.000388149404777796,
                   trial = 0.0476263762255556, trial_sq = 0.000348698499086251,
                   sex = 0.0857959336437619, N_bas = 0.00467909638734656, L_bas = 0.00544378715821034,
                   P_bas = 0.4499841858896, `tx_init_bas:period` = 0.00703562616704658)

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

  expected <- list(`(Intercept)` = -4.2701539218291, tx_init_bas = 0.110811149893957,
                   period = -0.017220820771173, period_sq = 0.0006111512520016,
                   trial = -0.00697919074301742, trial_sq = 0.000616208350394808,
                   `tx_init_bas:period` = 0.00258857152517637)

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

  expected <- list(`(Intercept)` = -7.88637604899815, tx_init_bas = 0.161710829362199,
                   period = -0.00483436893432092, period_sq = 0.000459370039333975,
                   trial = 0.0488623911961782, trial_sq = 0.000288543231011635,
                   sex = 0.149778969872995, N_bas = 0.00160752996570635, L_bas = 0.00453609101278976,
                   P_bas = 0.360975294944106, `tx_init_bas:period` = 0.000850129931439267)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion ITT (Cense 1 - LTFU)", {
  data <- SEQdata.LTFU
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", pre.expansion = TRUE, fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -24.4894227327396, tx_init_bas = 0.0817753642254611,
                   period = 0.21413646202376, period_sq = -0.00291398886923746,
                   trial = 0.253773061598757, trial_sq = -0.000781385695122158,
                   sex = -0.186474187953656, N_bas = 0.0065077224496683, L_bas = -0.449512688403006,
                   P_bas = 1.37027931867904)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion ITT (Cense 1 - LTFU)", {
  data <- SEQdata.LTFU
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", pre.expansion = FALSE, fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -24.4879360955557, tx_init_bas = 0.0817404333931434,
                   period = 0.214101446362142, period_sq = -0.00291347608450675,
                   trial = 0.2537647514562, trial_sq = -0.000781476580970658,
                   sex = -0.186610917326086, N_bas = 0.00650388742186271, L_bas = -0.449592953508595,
                   P_bas = 1.37019867907897)

  test <- as.list(model@outcome_model[[1]])
  expect_equal(test, expected, tolerance = 1e-2)
})
