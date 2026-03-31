library(data.table)

# ── class_methods.R accessor functions ──────────────────────────────────────

test_that("risk_data and risk_comparison return data when km.curves = TRUE", {
  model <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "censoring",
                      options = SEQopts(km.curves = TRUE, weighted = TRUE))
  expect_true(is.data.table(risk_data(model)))
  expect_true(is.data.table(risk_comparison(model)))
  expect_true(nrow(risk_data(model)) > 0)
  expect_true(nrow(risk_comparison(model)) > 0)
})

test_that("risk_data and risk_comparison error when km.curves = FALSE", {
  model <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts())
  expect_error(risk_data(model), "km.curves")
  expect_error(risk_comparison(model), "km.curves")
})

test_that("hazard_ratio returns values and errors when hazard = FALSE", {
  model_hz <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                         list("N", "L", "P"), list("sex"),
                         method = "ITT", options = SEQopts(hazard = TRUE))
  expect_true(is.numeric(hazard_ratio(model_hz)[[1]]))

  model_no <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                         list("N", "L", "P"), list("sex"),
                         method = "ITT", options = SEQopts())
  expect_error(hazard_ratio(model_no), "hazard")
})

test_that("diagnostics returns info list", {
  model <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts())
  info <- diagnostics(model)
  expect_true(is.list(info))
  expect_true("outcome.unique" %in% names(info))
  expect_true("outcome.nonunique" %in% names(info))
})

test_that("SEQ_data returns data when data.return = TRUE and errors otherwise", {
  model_dr <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                         list("N", "L", "P"), list("sex"),
                         method = "ITT", options = SEQopts(data.return = TRUE))
  expect_s3_class(SEQ_data(model_dr), "data.table")
  expect_true(nrow(SEQ_data(model_dr)) > 0)

  model_no <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                         list("N", "L", "P"), list("sex"),
                         method = "ITT", options = SEQopts())
  expect_error(SEQ_data(model_no), "data.return")
})

test_that("accessor functions error on non-SEQoutput objects", {
  expect_error(numerator(list()), "SEQoutput")
  expect_error(denominator(list()), "SEQoutput")
  expect_error(outcome(list()), "SEQoutput")
  expect_error(covariates(list()), "SEQoutput")
  expect_error(km_curve(list()), "SEQoutput")
  expect_error(km_data(list()), "SEQoutput")
  expect_error(compevent(list()), "SEQoutput")
  expect_error(risk_data(list()), "SEQoutput")
  expect_error(risk_comparison(list()), "SEQoutput")
  expect_error(hazard_ratio(list()), "SEQoutput")
  expect_error(diagnostics(list()), "SEQoutput")
  expect_error(SEQ_data(list()), "SEQoutput")
})

test_that("numerator/denominator error on unweighted model", {
  model <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts())
  expect_error(numerator(model), "weighted")
  expect_error(denominator(model), "weighted")
})

test_that("km_curve errors on invalid plot.type", {
  model <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "censoring",
                      options = SEQopts(km.curves = TRUE, weighted = TRUE))
  expect_error(km_curve(model, plot.type = "invalid"), "plot.type")
})

test_that("km_curve with custom plot parameters", {
  model <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "censoring",
                      options = SEQopts(km.curves = TRUE, weighted = TRUE))
  p <- km_curve(model, plot.type = "risk",
                plot.title = "Test Title", plot.subtitle = "Test Subtitle",
                plot.labels = c("A", "B"), plot.colors = c("red", "blue"))
  expect_s3_class(p, "ggplot")
})

test_that("compevent accessor errors when no compevent specified", {
  model <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts())
  expect_error(compevent(model), "competing event")
})

# ── show method branches ────────────────────────────────────────────────────

test_that("show with hazard model", {
  model <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(hazard = TRUE))
  expect_output(show(model), "Hazard")
})

test_that("show with km.curves", {
  model <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "censoring",
                      options = SEQopts(km.curves = TRUE, weighted = TRUE))
  expect_output(show(model), "Risk")
})

test_that("show with LTFU weights", {
  model <- SEQuential(copy(SEQdata.LTFU), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", fastglm.method = 1))
  expect_output(show(model), "LTFU")
})

test_that("show with competing event", {
  data <- copy(SEQdata)
  set.seed(42)
  data[, compevent := as.integer(runif(.N) < 0.02)]
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(km.curves = TRUE, compevent = "compevent"))
  expect_output(show(model), "Competing Event")
})

# ── internal_multinomial.R ──────────────────────────────────────────────────

test_that("multinomial model fitting and prediction", {
  set.seed(1)
  X <- cbind(1, matrix(rnorm(300), 100, 3))
  y <- sample(c(0, 1, 2), 100, replace = TRUE)
  model <- SEQTaRget:::multinomial(X, y, method = 2)
  expect_true(is.list(model))
  expect_true("models" %in% names(model))
  expect_true("baseline" %in% names(model))
  # Predict for a specific target
  pred <- SEQTaRget:::multinomial.predict(model, X, target = "1")
  expect_true(is.numeric(pred))
  expect_equal(length(pred), 100)
})

test_that("multinomial.summary returns a well-formed data.frame", {
  set.seed(1)
  X <- cbind(1, matrix(rnorm(300), 100, 3))
  y <- sample(c(0, 1, 2), 100, replace = TRUE)
  model <- SEQTaRget:::multinomial(X, y, method = 2)
  result <- SEQTaRget:::multinomial.summary(model)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Class", "Term", "Coefficient", "Std.Error", "Z.Value", "P.Value") %in% names(result)))
  # Should have baseline row + rows for each non-baseline class
  expect_true(nrow(result) > 1)
  # Baseline coefficient should be 0
  expect_equal(result$Coefficient[result$Class == model$baseline], 0)
})

test_that("multinomial.predict returns full probability matrix when target is NULL", {
  set.seed(1)
  X <- cbind(1, matrix(rnorm(300), 100, 3))
  y <- sample(c(0, 1, 2), 100, replace = TRUE)
  model <- SEQTaRget:::multinomial(X, y, method = 2)
  probs <- SEQTaRget:::multinomial.predict(model, X, target = NULL)
  expect_true(is.matrix(probs))
  expect_equal(ncol(probs), 3)
  expect_equal(nrow(probs), 100)
  # Probabilities should sum to 1 per row
  expect_equal(rowSums(probs), rep(1, 100), tolerance = 1e-10)
})

test_that("multinomial.predict errors on invalid target", {
  set.seed(1)
  X <- cbind(1, matrix(rnorm(300), 100, 3))
  y <- sample(c(0, 1, 2), 100, replace = TRUE)
  model <- SEQTaRget:::multinomial(X, y, method = 2)
  expect_error(SEQTaRget:::multinomial.predict(model, X, target = "99"), "not found")
})

# ── internal_misc.R format.time ─────────────────────────────────────────────

test_that("format.time handles seconds, minutes, and hours", {
  expect_match(SEQTaRget:::format.time(30), "seconds")
  expect_match(SEQTaRget:::format.time(90), "minute")
  expect_match(SEQTaRget:::format.time(3700), "hour")
})

# ── SEQexpand.R: selection.first_trial ──────────────────────────────────────

test_that("selection.first_trial restricts to first trial per subject", {
  model <- suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(selection.first_trial = TRUE, data.return = TRUE,
                                        covariates = "followup+followup_sq+sex+N_bas+L_bas+P_bas")))
  dt <- SEQ_data(model)
  # Each subject should have exactly one trial value
  trials_per_id <- dt[, uniqueN(trial), by = "ID"]
  expect_true(all(trials_per_id$V1 == 1))
})

# ── SEQexpand.R: selection.random ───────────────────────────────────────────

test_that("selection.random subsamples controls", {
  model <- suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(selection.prob = 0.9, data.return = TRUE)))
  expect_s4_class(model, "SEQoutput")
})

# ── SEQuential.R: validation paths ──────────────────────────────────────────

test_that("SEQuential errors on non-binary outcome", {
  bad <- copy(SEQdata)
  bad[1, outcome := 2L]
  expect_error(SEQuential(bad, "ID", "time", "eligible", "tx_init", "outcome",
                          list("N", "L", "P"), list("sex"),
                          method = "ITT", options = SEQopts()), "binary")
})

test_that("SEQuential errors on non-binary eligible column", {
  bad <- copy(SEQdata)
  bad[1, eligible := 2L]
  expect_error(SEQuential(bad, "ID", "time", "eligible", "tx_init", "outcome",
                          list("N", "L", "P"), list("sex"),
                          method = "ITT", options = SEQopts()), "binary")
})

test_that("SEQuential errors on duplicate id/time combinations", {
  bad <- rbind(copy(SEQdata), copy(SEQdata)[ID == 1 & time == 0, ])
  expect_error(SEQuential(bad, "ID", "time", "eligible", "tx_init", "outcome",
                          list("N", "L", "P"), list("sex"),
                          method = "ITT", options = SEQopts()), "duplicate")
})

test_that("SEQuential errors on overlapping time_varying and fixed columns", {
  expect_error(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                          list("N", "L", "P", "sex"), list("sex"),
                          method = "ITT", options = SEQopts()),
               "time_varying.cols and fixed.cols")
})

test_that("SEQuential errors on wrong treat.level count for non-multinomial", {
  expect_error(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                          list("N", "L", "P"), list("sex"),
                          method = "ITT",
                          options = SEQopts(treat.level = c(0, 1, 2))), "treat.level")
})

test_that("SEQuential errors on missing treat.level values", {
  expect_error(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                          list("N", "L", "P"), list("sex"),
                          method = "ITT",
                          options = SEQopts(treat.level = c(0, 99))), "treat.level")
})

# ── internal_hazard.R: hazard with competing event ──────────────────────────

test_that("Hazard with competing event", {
  data <- copy(SEQdata)
  set.seed(42)
  data[, compevent := as.integer(runif(.N) < 0.02)]
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(hazard = TRUE, compevent = "compevent"))
  expect_s4_class(model, "SEQoutput")
  hr <- hazard_ratio(model)
  expect_true(is.numeric(hr[[1]]))
})

test_that("Hazard bootstrap with competing event", {
  data <- copy(SEQdata)
  set.seed(42)
  data[, compevent := as.integer(runif(.N) < 0.02)]
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(hazard = TRUE, compevent = "compevent",
                                        bootstrap = TRUE, bootstrap.nboot = 2))
  hr <- hazard_ratio(model)
  expect_true("LCI" %in% names(hr))
  expect_true("UCI" %in% names(hr))
})

# ── SEQestimate.R: additional branches ──────────────────────────────────────

test_that("SEQestimate with bootstrap and LTFU", {
  time <- SEQestimate(copy(SEQdata.LTFU), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(cense = "LTFU", km.curves = TRUE,
                                        bootstrap = TRUE, bootstrap.nboot = 2))
  expect_length(time, 3)
  expect_true(is.character(time$totalTime))
})

test_that("SEQestimate errors on missing args", {
  expect_error(SEQestimate(), "Data was not supplied")
  expect_error(SEQestimate(SEQdata), "ID column")
  expect_error(SEQestimate(SEQdata, "ID"), "Time column")
  expect_error(SEQestimate(SEQdata, "ID", "time"), "Eligibility")
  expect_error(SEQestimate(SEQdata, "ID", "time", "eligible"), "Treatment")
  expect_error(SEQestimate(SEQdata, "ID", "time", "eligible", "tx_init"), "Outcome")
  expect_error(SEQestimate(SEQdata, "ID", "time", "eligible", "tx_init", "outcome"), "Method")
})

# ── internal_plot.R: inc plot type ──────────────────────────────────────────

test_that("Survival curve with inc plot type for competing event", {
  data <- copy(SEQdata)
  set.seed(42)
  data[, compevent := as.integer(runif(.N) < 0.02)]
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(km.curves = TRUE, compevent = "compevent",
                                        plot.type = "inc"))
  expect_s3_class(km_curve(model), "ggplot")
})

# ── internal_models.R: subgroup path ────────────────────────────────────────

test_that("Subgroup analysis produces correct output structure", {
  model <- SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(subgroup = "sex",
                                        covariates = "tx_init_bas+followup+followup_sq+trial+trial_sq+N_bas+L_bas+P_bas"))
  expect_s4_class(model, "SEQoutput")
  # Should have one model per subgroup level
  expect_true(length(model@outcome.model) > 1)
})

# ── SEQopts.R validation paths ─────────────────────────────────────────────

test_that("SEQopts errors on invalid parameters", {
  expect_error(SEQopts(bootstrap.nboot = -1), "bootstrap.nboot")
  expect_error(SEQopts(bootstrap.CI = 1.5), "bootstrap.CI")
  expect_error(SEQopts(bootstrap.sample = 0), "bootstrap.sample")
  expect_error(SEQopts(ncores = 0), "ncores")
  expect_error(SEQopts(nthreads = -1), "nthreads")
  expect_error(SEQopts(selection.prob = 2), "selection.prob")
  expect_error(SEQopts(fastglm.method = 5), "fastglm.method")
})

test_that("SEQopts errors when followup.min >= followup.max", {
  expect_error(SEQopts(followup.min = 10, followup.max = 5), "followup.min")
})
