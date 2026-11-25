library(data.table)

ce_run <- function(method = "ITT", ...) {
  suppressWarnings(SEQuential(copy(SEQdata.LTFU), "ID", "time", "eligible", "tx_init", "outcome",
                              list("N", "L", "P"), list("sex"), method = method, verbose = FALSE,
                              options = SEQopts(compevent = "LTFU", weighted = method != "ITT", ...)))
}

test_that("The competing event model is fit on the outcome model's rows and matches pySEQTarget", {
  skip_on_cran()
  model <- ce_run(km.curves = TRUE)
  # pySEQTarget 6a9928e, the same analysis: model.outcome_model[0]["compevent"].params
  expected <- c(`(Intercept)` = -9.009629, tx_init_bas1 = -0.176885, followup = 0.003366,
                followup_sq = 0.000087, trial = 0.077813, trial_sq = -0.000642, sex = -0.221532,
                N_bas = -0.001023, L_bas = 0.075146, P_bas = 0.334465, `tx_init_bas1:followup` = 0.005414)
  expect_equal(coef(compevent(model)[[1]]), expected, tolerance = 1e-4)
  # and its km_data cumulative incidence at follow-up 60
  inc <- km_data(model)[followup == 60 & variable %like% "^inc_"]
  expect_equal(inc$value, c(0.021072, 0.023279), tolerance = 1e-4)
})

test_that("The weighted competing event model matches pySEQTarget", {
  skip_on_cran()
  model <- ce_run("censoring", km.curves = TRUE)
  inc <- km_data(model)[followup == 60 & variable %like% "^inc_"]
  expect_equal(inc$value, c(0.003490, 0.008983), tolerance = 1e-3)
})

test_that("The competing event models are kept for hazard ratios and every bootstrap", {
  skip_on_cran()
  model <- ce_run(hazard = TRUE, bootstrap = TRUE, bootstrap.nboot = 2, seed = 1)
  expect_true(is.finite(model@hazard[[1]][["Hazard ratio"]]))
  ce <- compevent(model)
  expect_length(ce, 3)
  expect_true(all(vapply(ce, function(m) is.finite(coef(m)[["followup"]]), logical(1))))
})
