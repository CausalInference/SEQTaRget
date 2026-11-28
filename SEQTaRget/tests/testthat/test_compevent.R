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
  # Cumulative incidence at follow-up 60. pySEQTarget 6a9928e gives 0.021072 and
  # 0.023279, as it does not yet lag the event-free survival
  inc <- km_data(model)[followup == 60 & variable %like% "^inc_"]
  expect_equal(inc$value, c(0.021180, 0.023400), tolerance = 1e-4)
})

test_that("The weighted competing event cumulative incidence", {
  skip_on_cran()
  model <- ce_run("censoring", km.curves = TRUE)
  # pySEQTarget 6a9928e, without the lag: 0.003490 and 0.008983
  inc <- km_data(model)[followup == 60 & variable %like% "^inc_"]
  expect_equal(inc$value, c(0.003512, 0.009026), tolerance = 1e-3)
})

test_that("With no competing events the cumulative incidence equals the risk", {
  skip_on_cran()
  data <- copy(SEQdata)[, ce_zero := 0L]
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"), method = "ITT", verbose = FALSE,
                                       options = SEQopts(km.curves = TRUE, compevent = "ce_zero")))
  km <- km_data(model)
  for (arm in c("0", "1")) {
    inc <- km[variable == paste0("inc_", arm)][order(followup)]$value
    risk <- km[variable == paste0("risk_", arm)][order(followup)]$value
    expect_false(anyNA(inc))
    expect_equal(inc, risk, tolerance = 1e-10)
  }
})

test_that("The competing event models are kept for hazard ratios and every bootstrap", {
  skip_on_cran()
  model <- ce_run(hazard = TRUE, bootstrap = TRUE, bootstrap.nboot = 2, seed = 1)
  expect_true(is.finite(model@hazard[[1]][["Hazard ratio"]]))
  ce <- compevent(model)
  expect_length(ce, 3)
  expect_true(all(vapply(ce, function(m) is.finite(coef(m)[["followup"]]), logical(1))))
})
