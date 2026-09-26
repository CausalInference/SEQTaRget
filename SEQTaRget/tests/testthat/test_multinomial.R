test_that("Multinomial ITT", {
  data_multi <- data.table::copy(SEQdata.multitreatment)
  model <- SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1, 2))
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -38.9787788811494, tx_init_bas1 = -2.50395466491941, 
                   tx_init_bas2 = -0.743287991503768, followup = 0.0797712238734039, 
                   followup_sq = -0.00241914909985099, trial = 0.394571194786938, 
                   trial_sq = -0.00619391201996135, sex = 16.9577220100443, 
                   N_bas = 0.0523878535070352, L_bas = 0.821748855840917, P_bas = 1.36996588560245)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
  
  # Testing show - no weights
  show(model)
})

test_that("Multinomial Censoring Pre-Expansion", {
  data_multi <- data.table::copy(SEQdata.multitreatment)
  model <- SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "censoring", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1, 2),
                                                              weighted = TRUE)
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -449.832275405838, tx_init_bas1 = 18.1973453707481, 
                   tx_init_bas2 = -0.718731801861477, followup = 0.774243620146147, 
                   followup_sq = -0.28420788583335, trial = 24.3523643961688, 
                   trial_sq = -0.362290475321413, sex = 18.6843757434277)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
  
  # Testing show - weights
  show(model)
})

test_that("Multinomial Censoring Post-Expansion", {
  data_multi <- data.table::copy(SEQdata.multitreatment)
  model <- SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "censoring", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1, 2),
                                                              weighted = TRUE, weight.preexpansion = FALSE)
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -448.710562199608, tx_init_bas1 = 17.9284837382002, 
                   tx_init_bas2 = -0.761587951039862, followup = 0.790913824567396, 
                   followup_sq = -0.289055118679556, trial = 23.903453873774, 
                   trial_sq = -0.355064037507412, sex = 18.7273679453924, N_bas = 0.0041197541792462, 
                   L_bas = 0.352474411172885, P_bas = 1.07955496166783)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Multinomial Censoring Excused Pre-Expansion", {
  data_multi <- data.table::copy(SEQdata.multitreatment)
  model <- suppressWarnings(SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                                       method = "censoring", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1),
                                                              weighted = TRUE, weight.preexpansion = TRUE,
                                                              excused = TRUE, excused.cols = c("excusedZero", "excusedOne")))
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -50.7111118692773, tx_init_bas1 = -4.566272104771, 
                   followup = 0.777168505, followup_sq = -0.0278074277750756, 
                   trial = 3.50456119247621, trial_sq = -0.0697743189877489)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Multinomial Censoring Excused Post-Expansion", {
  data_multi <- data.table::copy(SEQdata.multitreatment)
  model <- suppressWarnings(SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                                       method = "censoring", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1),
                                                                               weighted = TRUE, weight.preexpansion = FALSE,
                                                                               excused = TRUE, excused.cols = c("excusedZero", "excusedOne")))
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -8.93642594359111, tx_init_bas1 = -5.61451410491509, 
                   followup = 1.04686013063719, followup_sq = -0.0993244245914494, 
                   trial = 0.486647207785848, trial_sq = -0.0121460057546972, 
                   sex = 10.2124817342716, N_bas = 0.114894698211935, L_bas = 0.377648021676872, 
                   P_bas = -2.23793202270577)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Multinomial weights give each arm's stayers the probability of staying, whatever the treat.level order", {
  skip_on_cran()
  stayers <- function(treat.level) {
    model <- suppressWarnings(SEQuential(data.table::copy(SEQdata.multitreatment), "ID", "time", "eligible", "tx_init", "outcome",
                                         list("N", "L", "P"), list("sex"), method = "censoring", verbose = FALSE,
                                         options = SEQopts(multinomial = TRUE, treat.level = treat.level, weighted = TRUE,
                                                           weight.preexpansion = FALSE, data.return = TRUE)))
    SEQ_data(model)[followup > 0 & tx_init == tx_init_bas, .(denominator = mean(denominator)), keyby = tx_init_bas]
  }
  a <- stayers(c(0, 1, 2))
  # Observed stay rates in SEQdata.multitreatment are 0.80, 0.95 and 0.95
  expect_true(all(a$denominator > 0.75))
  expect_equal(stayers(c(2, 0, 1)), a)
})
