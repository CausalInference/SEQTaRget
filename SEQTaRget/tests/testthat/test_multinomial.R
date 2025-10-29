test_that("Multinomial ITT", {
  data_multi <- copy(SEQdata.multitreatment)
  model <- SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1, 2))
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -38.9787788811494, tx_init_bas1 = -2.50395466491941, 
                   tx_init_bas2 = -0.743287991503768, followup = 0.0797712238734039, 
                   followup_sq = -0.00241914909985099, trial = 0.394571194786938, 
                   trial_sq = -0.00619391201996135, sex1 = 16.9577220100443, 
                   N_bas = 0.0523878535070352, L_bas = 0.821748855840917, P_bas = 1.36996588560245)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
  
  # Testing show - no weights
  show(model)
})

test_that("Multinomial Censoring Pre-Expansion", {
  data_multi <- copy(SEQdata.multitreatment)
  model <- SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "censoring", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1, 2),
                                                              weighted = TRUE)
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -449.454379243513, tx_init_bas1 = 18.0763747518197, 
                   tx_init_bas2 = -0.844043473716606, followup = 0.774857892280033, 
                   followup_sq = -0.284158628692633, trial = 24.3387136677569, 
                   trial_sq = -0.36209910311883, sex1 = 18.6694525247154)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
  
  # Testing show - weights
  show(model)
})

test_that("Multinomial Censoring Post-Expansion", {
  data_multi <- copy(SEQdata.multitreatment)
  model <- SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "censoring", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1, 2),
                                                              weighted = TRUE, weight.preexpansion = FALSE)
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -448.524963183844, tx_init_bas1 = 17.2990160079927, 
                   tx_init_bas2 = -3.53621951339759, followup = 0.79091382186495, 
                   followup_sq = -0.289055117887172, trial = 23.903453730725, 
                   trial_sq = -0.35506403537735, sex1 = 19.1712390550441, N_bas = 0.00411975419044303, 
                   L_bas = 0.352474411282074, P_bas = 1.07955496263324)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Multinomial Censoring Excused Pre-Expansion", {
  data_multi <- copy(SEQdata.multitreatment)
  model <- suppressWarnings(SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                                       method = "censoring", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1),
                                                              weighted = TRUE, weight.preexpansion = TRUE,
                                                              excused = TRUE, excused.cols = c("excusedZero", "excusedOne")))
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -52.3894943563174, tx_init_bas1 = -4.39028002359654, 
                   followup = 0.709855917998915, followup_sq = -0.0273142821592202, 
                   trial = 3.60040336502847, trial_sq = -0.0706023467658923)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})

test_that("Multinomial Censoring Excused Post-Expansion", {
  data_multi <- copy(SEQdata.multitreatment)
  model <- suppressWarnings(SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                                       method = "censoring", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1),
                                                                               weighted = TRUE, weight.preexpansion = FALSE,
                                                                               excused = TRUE, excused.cols = c("excusedZero", "excusedOne")))
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -22.8581470824005, tx_init_bas1 = -3.61440913739626, 
                   followup = 0.243691412750202, followup_sq = -0.0164953482205765, 
                   trial = 1.1171226465557, trial_sq = -0.024879010022683, sex1 = 21.3341934036542, 
                   N_bas = -0.0331495480042171, L_bas = -0.0656028381271247, 
                   P_bas = -2.5498722549852)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})
