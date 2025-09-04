test_that("Multinomial ITT", {
  data_multi <- copy(SEQdata.multitreatment)
  model <- SEQuential(data_multi, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(multinomial = TRUE, treat.level = c(0, 1, 2))
  )
  expect_s4_class(model, "SEQoutput")
  
  expected <- list(`(Intercept)` = -40.0659152478923, tx_init_bas1 = 0.894544979578208, 
                   tx_init_bas2 = -3.09691654494703, followup = 0.170743493086, 
                   followup_sq = -0.00564594916525636, trial = 0.38154366851834, 
                   trial_sq = -0.00585184971467783, sex1 = 17.8575956153573, 
                   N_bas = 0.0515401432496117, L_bas = 0.817805914912013, P_bas = 1.36144301440333, 
                   `tx_init_bas1:followup` = -0.642974252754863, `tx_init_bas2:followup` = 0.120583421108189)
  
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
  
  expected <- list(`(Intercept)` = -460.793218785779, tx_init_bas1 = 28.3633489128303, 
                   tx_init_bas2 = -118.33560462378, followup = 3.53252183349464, 
                   followup_sq = -0.284158628709524, trial = 24.3387137279972, 
                   trial_sq = -0.362099104015334, sex1 = 19.7213168951556, 
                   `tx_init_bas1:followup` = -2.7576639411757, 
                   `tx_init_bas2:followup` = 8.65440067620562)
  
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
  
  expected <- list(`(Intercept)` = -605.753784029134, tx_init_bas1 = 169.575539586038, 
                   tx_init_bas2 = 33.3044604342727, followup = 21.7252557989417, 
                   followup_sq = -0.289055119203924, trial = 23.9034539836864, 
                   trial_sq = -0.355064039141562, sex1 = 24.1235320870467, N_bas = 0.00411975415353011, 
                   L_bas = 0.352474411070244, P_bas = 1.07955496033118, 
                   `tx_init_bas1:followup` = -20.934341972539, 
                   `tx_init_bas2:followup` = -9.98556305368828)
  
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
  
  expected <- list(`(Intercept)` = -58.1903976075198, tx_init_bas1 = 6.56252269538545, 
                   followup = 1.45486706273228, followup_sq = -0.0527804629686317, 
                   trial = 3.66632489118433, trial_sq = -0.0719613222262424, 
                   `tx_init_bas1:followup` = -1.67337179570479)
  
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
  
  expected <- list(`(Intercept)` = -7.9621883057148, tx_init_bas1 = 0.980863152775663, 
                   followup = 0.644300028032517, followup_sq = -0.0315534845854166, 
                   trial = 1.09659829963825, trial_sq = -0.0252606693575758, 
                   sex1 = 6.7011865089521, N_bas = -0.0329505689776581, L_bas = -0.0149355786758694, 
                   P_bas = -2.83244833596151, `tx_init_bas1:followup` = -0.940753941433046)
  
  test <- as.list(coef(model@outcome.model[[1]][[1]]))
  expect_equal(test, expected, tolerance = 1e-2)
})
