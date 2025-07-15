test_that("ITT", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "ITT", options = SEQopts()
  )
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -6.8593155484953, tx_init_bas1 = 0.225309379526984, 
                   followup = 0.0353817161706608, followup_sq = -0.000159868670243564, 
                   trial = 0.0447178957320877, trial_sq = 0.00057616846490727, 
                   sex1 = 0.127045833687322, N_bas = 0.00328670775503695, L_bas = -0.0138508823648277, 
                   P_bas = 0.20092890277535, `tx_init_bas1:followup` = -0.00170402147034209)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Dose-Response", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "dose-response",
    options = SEQopts(weighted = TRUE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.89437436861547, dose = 0.0578172493878777, 
                   dose_sq = -0.00126433049568143, followup = 0.000817315276267894, 
                   followup_sq = -0.00017242835071024, trial = 0.0104883292627362, 
                   trial_sq = 0.000781016829235595, sex1 = 0.172630682250999, 
                   `dose:followup` = 0.000354769280320122, `dose_sq:followup` = 1.0605834086728e-05)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Dose-Response", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "dose-response",
    options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -6.28409662777892, dose = 0.0554565105699927, 
                   dose_sq = -0.00107799456270646, followup = 3.18615459075794e-05, 
                   followup_sq = -3.82682414803644e-05, trial = 0.0382644719826706, 
                   trial_sq = 0.000596069931869212, sex1 = 0.140842013054163, 
                   N_bas = 0.00297698449941808, L_bas = -0.0205414282747305, 
                   P_bas = 0.14686055362848, `dose:followup` = 0.000185281011892035, 
                   `dose_sq:followup` = 8.89205007331301e-06)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Censoring", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -4.8331064655654, tx_init_bas1 = 0.401676361798836, 
                   followup = 0.0147725158310964, followup_sq = 4.04157002656377e-05, 
                   trial = -0.0131874685320351, trial_sq = 0.00113427136978775, 
                   sex1 = 0.0841593672798384, `tx_init_bas1:followup` = 0.0163419157652325)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Censoring", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE, weight.preexpansion = FALSE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -9.0844372847784, tx_init_bas1 = 0.375383391826688, 
                   followup = 0.0115926847602433, followup_sq = 3.85125072076895e-05, 
                   trial = 0.0668737514863143, trial_sq = 0.000584719074047399, 
                   sex1 = 0.0819971019400167, N_bas = 0.00480761695729425, L_bas = 0.0140302929317927, 
                   P_bas = 0.446176305145455, `tx_init_bas1:followup` = 0.0193363456582428)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion Excused Censoring", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.cols = c("excusedZero", "excusedOne"))
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -5.2575896658564, tx_init_bas1 = 0.377685654920678, 
                   followup = 0.00760616508976486, followup_sq = 0.000746470267632687, 
                   trial = 0.0522808132127782, trial_sq = 9.77743011844572e-05, 
                   `tx_init_bas1:followup` = -0.0118014806930868)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion Excused Censoring", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.cols = c("excusedZero", "excusedOne"),
      weight.preexpansion = FALSE)
  ))
  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -8.76816941042751, tx_init_bas1 = 0.107023621033081, 
                   followup = 0.0271440568550105, followup_sq = -7.42456369201693e-06, 
                   trial = 0.0843245609402986, trial_sq = 0.000216179686613992, 
                   sex1 = 0.273375015185671, N_bas = 0.00220052489616481, L_bas = 0.0205008297547291, 
                   P_bas = 0.397196435394296, `tx_init_bas1:followup` = 0.00471743743001701)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Pre-Expansion ITT (Cense 1 - LTFU)", {
  data <- copy(SEQdata.LTFU)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", weight.preexpansion = TRUE, fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -21.5961892056874, tx_init_bas1 = -0.00900539101755618, 
                   followup = 0.0253307917110129, followup_sq = -0.000556228315864808, 
                   trial = 0.285534937686781, trial_sq = -0.00136624272388734, 
                   sex1 = -0.190092359484432, N_bas = 0.00658683429207927, L_bas = -0.448911925083965, 
                   P_bas = 1.38926165576551, `tx_init_bas1:followup` = 0.00383610589535659)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Post-Expansion ITT (Cense 1 - LTFU)", {
  data <- copy(SEQdata.LTFU)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", weight.preexpansion = FALSE, fastglm.method = 1))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -21.594890540512, tx_init_bas1 = -0.0090653673782976, 
                   followup = 0.0253239238090987, followup_sq = -0.000556027956315004, 
                   trial = 0.285516825890481, trial_sq = -0.00136620665011392, 
                   sex1 = -0.190220229415778, N_bas = 0.00658300214440885, L_bas = -0.448958593335045, 
                   P_bas = 1.38913912261648, `tx_init_bas1:followup` = 0.00383774199912364)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("ITT - Multinomial, Treatment Levels 1,2", {
  data <- SEQdata.multitreatment
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(multinomial = TRUE, treat.level = c(1,2)))

  expect_s4_class(model, "SEQoutput")

  expected <- list(`(Intercept)` = -38.3542109513865, tx_init_bas2 = -12.8546300967697, 
                   followup = -0.366300912520843, followup_sq = -0.0233872579089223, 
                   trial = 0.272450214929967, trial_sq = -0.00390896317632731, 
                   sex1 = 17.4954834067681, N_bas = 0.0548857956131806, L_bas = 0.80900824688992, 
                   P_bas = 1.45718621133794, `tx_init_bas2:followup` = 1.48015622296358)

  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})
