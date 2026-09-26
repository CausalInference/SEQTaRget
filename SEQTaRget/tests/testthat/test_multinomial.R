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
  
  expected <- list(`(Intercept)` = -449.303401632919, tx_init_bas1 = 18.1757926801988, 
                   tx_init_bas2 = -0.731840897420851, followup = 0.773093243824455, 
                   followup_sq = -0.284168948653283, trial = 24.3233957877663, 
                   trial_sq = -0.361878001032878, sex = 18.6859136701556)
  
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
  
  expected <- list(`(Intercept)` = -448.556328969027, tx_init_bas1 = 17.9072810845517, 
                   tx_init_bas2 = -0.807449870373759, followup = 0.791653874698164, 
                   followup_sq = -0.289468077734324, trial = 23.8930882667934, 
                   trial_sq = -0.354923320313731, sex = 18.7437405771097, N_bas = 0.00362892358099154, 
                   L_bas = 0.351094964247247, P_bas = 1.08933609926642)
  
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
  
  expected <- list(`(Intercept)` = -46.2709969346726, tx_init_bas1 = -4.32046243378271, 
                   followup = 0.606615641500371, followup_sq = -0.0232451765293424, 
                   trial = 3.1538796309021, trial_sq = -0.0611677087418465)
  
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

test_that("multinomial() recovers baseline-category logit coefficients and class probabilities", {
  set.seed(42)
  n <- 20000
  x <- rnorm(n)
  eta <- cbind(0, -0.5 + 0.8 * x, 0.3 + 1.5 * x)
  p <- exp(eta) / rowSums(exp(eta))
  y <- apply(p, 1, function(pr) sample(0:2, 1, prob = pr))
  X <- cbind(1, x)
  model <- SEQTaRget:::multinomial(X, y, params = SEQopts())
  coefs <- sapply(model$models, coef)
  expect_equal(unname(coefs), cbind(c(-0.5, 0.8), c(0.3, 1.5)), tolerance = 0.1)
  expect_lt(max(abs(SEQTaRget:::multinomial.predict(model, X) - p)), 0.03)
})
