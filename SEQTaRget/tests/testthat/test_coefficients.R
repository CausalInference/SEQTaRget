test_that("ITT", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "ITT", options = SEQopts()
  )
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -6.82850603562684, tx_init_bas1 = 0.189350030900384, 
                   followup = 0.0337151569876223, followup_sq = -0.000146912022349962, 
                   trial = 0.0445661655603746, trial_sq = 0.000578777043895173, 
                   sex = 0.12717241010585, N_bas = 0.0032906669395465, L_bas = -0.0133924204920901, 
                   P_bas = 0.20072409919197)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
  
  show(model)
})

test_that("Pre-Expansion Dose-Response", {
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "dose-response",
    options = SEQopts(weighted = TRUE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.83836161019692, dose = 0.0555200174748001, 
                   dose_sq = -0.000588987868497995, followup = -0.00897736326044159, 
                   followup_sq = 0.000220255171131842, trial = 0.0104682866902423, 
                   trial_sq = 0.000777447490482556, sex = 0.139721988283041)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Dose-Response", {
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "dose-response",
    options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -6.2659017048277, dose = 0.048626017557512, 
                   dose_sq = -0.000468828727867736, followup = -0.0039759068186394, 
                   followup_sq = 0.000166764416798306, trial = 0.0386627996702701, 
                   trial_sq = 0.00059284496249684, sex = 0.140659541199455, 
                   N_bas = 0.00300014599970332, L_bas = -0.0210633816426347, 
                   P_bas = 0.148672505687797)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Censoring", {
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.86681104231166, tx_init_bas1 = 0.480523847494747, 
                   followup = 0.0292196056514481, followup_sq = 4.5181874579918e-05, 
                   trial = -0.013811945721892, trial_sq = 0.00113098930127373, 
                   sex = 0.0476313418785775)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Censoring", {
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -9.17226678515974, tx_init_bas1 = 0.470755472011569, 
                   followup = 0.0290210871964639, followup_sq = 7.89372268604027e-05, 
                   trial = 0.0670019228702671, trial_sq = 0.000583432366457826, 
                   sex = 0.0816261723249604, N_bas = 0.00487021276539206, L_bas = 0.013503198983259, 
                   P_bas = 0.446657380156616)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
  
  show(model)
})

test_that("Pre-Expansion Excused Censoring", {
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.cols = c("excusedZero", "excusedOne"))
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -5.0618303955656, tx_init_bas1 = 0.171017823034017, 
                   followup = -0.0108113857288237, followup_sq = 0.0010269383069014, 
                   trial = 0.0607902743813715, trial_sq = -0.000144738427593527)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Excused Censoring", {
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.cols = c("excusedZero", "excusedOne"),
      weight.preexpansion = FALSE, weight.upper = 1)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -7.89204875360584, tx_init_bas1 = 0.245649985320103, 
                   followup = 0.0373443362402412, followup_sq = -0.000223047568054156, 
                   trial = 0.0586498977853606, trial_sq = 0.000539362161580214, 
                   sex = 0.0845200664142512, N_bas = 0.00445311720239293, L_bas = -0.010283144035433, 
                   P_bas = 0.317411946012023)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion ITT (Cense 1 - LTFU)", {
  data <- data.table::copy(SEQdata.LTFU)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", weight.preexpansion = TRUE, fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -21.6405620525594, tx_init_bas1 = 0.0685251597169625, 
                   followup = 0.0287510195589004, followup_sq = -0.000576218149982575, 
                   trial = 0.285543817417294, trial_sq = -0.00137304711207655, 
                   sex = -0.190047710435459, N_bas = 0.00658945184598712, L_bas = -0.448999735097287, 
                   P_bas = 1.3875130950729)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion ITT (Cense 1 - LTFU)", {
  skip_on_cran()
  data <- data.table::copy(SEQdata.LTFU)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", weight.preexpansion = FALSE, fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -21.6392841784154, tx_init_bas1 = 0.0685037706919654, 
                   followup = 0.0287455207242284, followup_sq = -0.000576024665947725, 
                   trial = 0.285525630382568, trial_sq = -0.00137301343916652, 
                   sex = -0.190175239671448, N_bas = 0.00658560896685363, L_bas = -0.449046832747418, 
                   P_bas = 1.38738995062596)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("ITT - Multinomial, Treatment Levels 1,2", {
  data <- data.table::copy(SEQdata.multitreatment)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(multinomial = TRUE, treat.level = c(1,2)))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -42.505263097371, tx_init_bas2 = 1.76628017233961, 
                   followup = 0.144735360566272, followup_sq = -0.00372549951637432, 
                   trial = 0.289307099192178, trial_sq = -0.00426660812393486, 
                   sex = 17.7920513772194, N_bas = 0.0557442916451221, L_bas = 0.784786269192456, 
                   P_bas = 1.47034117591808)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion ITT: visit variable", {
  data <- data.table::copy(SEQdata.LTFU)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(visit = "LTFU", weight.preexpansion = TRUE, fastglm.method = 1,
                                        weighted = TRUE))
  
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -21.6363470211732, tx_init_bas1 = 0.0681370591202886, 
                   followup = 0.0287415276768208, followup_sq = -0.000573404701373437, 
                   trial = 0.285474021791075, trial_sq = -0.00137296623436822, 
                   sex = -0.193955597012598, N_bas = 0.00650191598271096, L_bas = -0.446707997333715, 
                   P_bas = 1.38704735078189)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
  expect_equal(model@params@visit, "LTFU")
  expect_equal(model@params@visit.denominator, "tx_lag+time+time_sq+sex+N+L+P")
  expect_equal(model@params@visit.numerator, "tx_lag+time+time_sq+sex")
})
