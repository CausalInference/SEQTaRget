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
  skip_on_cran()
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
  params <- SEQopts()
  model <- SEQTaRget:::multinomial(X, y, params = params)
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
  params <- SEQopts()
  model <- SEQTaRget:::multinomial(X, y, params = params)
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
  params <- SEQopts()
  model <- SEQTaRget:::multinomial(X, y, params = params)
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
  params <- SEQopts()
  model <- SEQTaRget:::multinomial(X, y, params = params)
  expect_error(SEQTaRget:::multinomial.predict(model, X, target = "99"), "not found")
})

# ── internal_misc.R format_time ─────────────────────────────────────────────

test_that("format_time handles seconds, minutes, and hours", {
  expect_match(SEQTaRget:::format_time(30), "seconds")
  expect_match(SEQTaRget:::format_time(90), "minute")
  expect_match(SEQTaRget:::format_time(3700), "hour")
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

test_that("SEQuential runs with selection.prob set (smoke test)", {
  # Smoke test only: selection.prob has no effect unless selection.random = TRUE,
  # so this just confirms the option is accepted and the run completes. The actual
  # subsampling behaviour is checked in the two tests below.
  model <- suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(selection.prob = 0.9, data.return = TRUE)))
  expect_s4_class(model, "SEQoutput")
})

test_that("selection.random keeps all treated starts and subsamples control starts", {
  skip_on_cran()
  prob <- 0.5

  # One row per trial-start (followup == 0), counted by baseline treatment arm.
  arm_counts <- function(DT) {
    s <- DT[followup == 0, .N, by = tx_init_bas]
    stats::setNames(s$N, as.character(s$tx_init_bas))
  }

  base <- suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(data.return = TRUE, seed = 1L), verbose = FALSE))
  sel <- suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(selection.random = TRUE, selection.prob = prob,
                                        data.return = TRUE, seed = 1L), verbose = FALSE))

  base_counts <- arm_counts(base@DT)
  sel_counts  <- arm_counts(sel@DT)

  # Treated trial-starts (baseline tx_init = 1) are all retained
  expect_equal(sel_counts[["1"]], base_counts[["1"]])
  # Control trial-starts (baseline tx_init = 0) are reduced to the requested fraction
  expect_lt(sel_counts[["0"]], base_counts[["0"]])
  expect_equal(sel_counts[["0"]], round(base_counts[["0"]] * prob))
})

test_that("selection.random is reproducible with a fixed seed", {
  skip_on_cran()
  args <- list("ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
               method = "ITT",
               options = SEQopts(selection.random = TRUE, selection.prob = 0.5,
                                 data.return = TRUE, seed = 7L))
  run1 <- suppressWarnings(do.call(SEQuential, c(list(data = copy(SEQdata)), args)))
  run2 <- suppressWarnings(do.call(SEQuential, c(list(data = copy(SEQdata)), args)))
  expect_equal(run1@DT, run2@DT)
})

# ── internal_models.R: weight truncation ────────────────────────────────────

test_that("weight.lower/weight.upper truncate the weights used in the outcome fit", {
  skip_on_cran()
  # Truncation is applied to the weight vector passed to the GLM fit (not the
  # returned @DT or weight.statistics), so its effect is checked through the
  # fitted coefficients. SEQdata weights span ~0.54-2.16, so a band entirely
  # above that range clamps every weight to the same constant. A GLM is invariant
  # to a uniform scaling of its weights, so two such all-constant clamps must give
  # identical coefficients, while a genuinely varying-weight fit must differ.
  cf <- function(m) coef(m@outcome.model[[1]][[1]])
  args <- list("ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
               method = "censoring")
  mk <- function(opts) suppressWarnings(do.call(SEQuential, c(list(data = copy(SEQdata)), args,
                                                              list(options = opts, verbose = FALSE))))

  varying  <- mk(SEQopts(weighted = TRUE, seed = 1L))
  clamp3   <- mk(SEQopts(weighted = TRUE, weight.lower = 3,  weight.upper = 4,  seed = 1L))
  clamp10  <- mk(SEQopts(weighted = TRUE, weight.lower = 10, weight.upper = 11, seed = 1L))

  # Both bands clamp all weights to a constant -> scale-invariant, identical fit
  expect_equal(cf(clamp3), cf(clamp10), tolerance = 1e-6)
  # Clamping away the real weight variation changes the fit
  expect_false(isTRUE(all.equal(cf(clamp3), cf(varying), tolerance = 1e-6)))
})

test_that("weight.p99 truncates at the 1st/99th percentile weights", {
  skip_on_cran()
  # weight.p99 = TRUE sets weight.lower/upper to the p01/p99 of the (untruncated)
  # weights, which are reported in weight.statistics. So it must be equivalent to
  # explicitly passing those percentiles as the bounds, and must differ from an
  # untruncated weighted fit.
  cf <- function(m) coef(m@outcome.model[[1]][[1]])
  args <- list("ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
               method = "censoring")
  mk <- function(opts) suppressWarnings(do.call(SEQuential, c(list(data = copy(SEQdata)), args,
                                                              list(options = opts, verbose = FALSE))))

  p99  <- mk(SEQopts(weighted = TRUE, weight.p99 = TRUE, seed = 1L))
  ws   <- p99@weight.statistics[[1]][[1]]
  expl <- mk(SEQopts(weighted = TRUE, weight.lower = ws$p01, weight.upper = ws$p99, seed = 1L))
  none <- mk(SEQopts(weighted = TRUE, seed = 1L))

  expect_equal(cf(p99), cf(expl), tolerance = 1e-8)
  expect_false(isTRUE(all.equal(cf(p99), cf(none), tolerance = 1e-6)))
})

# ── internal_covariates.R: followup.include / trial.include ──────────────────

test_that("followup.include / trial.include add or drop their outcome-model terms", {
  skip_on_cran()
  # These flags control whether the follow-up and trial terms (and their squares)
  # enter the outcome model formula (internal_covariates.R), so the effect is
  # visible in the fitted coefficient names.
  nm <- function(m) names(coef(m@outcome.model[[1]][[1]]))
  args <- list("ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
               method = "ITT")
  mk <- function(opts) suppressWarnings(do.call(SEQuential, c(list(data = copy(SEQdata)), args,
                                                              list(options = opts, verbose = FALSE))))

  both     <- nm(mk(SEQopts()))                          # both included by default
  no_fup   <- nm(mk(SEQopts(followup.include = FALSE)))
  no_trial <- nm(mk(SEQopts(trial.include = FALSE)))

  # Baseline: all four terms present
  expect_true(all(c("followup", "followup_sq", "trial", "trial_sq") %in% both))

  # followup.include = FALSE drops the follow-up terms but keeps the trial terms
  expect_false(any(c("followup", "followup_sq") %in% no_fup))
  expect_true(all(c("trial", "trial_sq") %in% no_fup))

  # trial.include = FALSE drops the trial terms but keeps the follow-up terms
  expect_false(any(c("trial", "trial_sq") %in% no_trial))
  expect_true(all(c("followup", "followup_sq") %in% no_trial))
})

# ── internal_models.R: followup.class ────────────────────────────────────────

test_that("followup.class encodes follow-up as a factor (one term per level)", {
  skip_on_cran()
  # followup.class = TRUE treats follow-up as categorical (internal_models.R coerces
  # it to a factor), so the outcome model gains one dummy per non-reference level
  # instead of the linear followup / followup_sq pair. It is exclusive with
  # followup.include, so that is switched off here.
  m <- suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"), method = "ITT",
                      options = SEQopts(followup.include = FALSE, followup.class = TRUE,
                                        data.return = TRUE), verbose = FALSE))
  cf <- names(coef(m@outcome.model[[1]][[1]]))
  dummies <- grep("^followup[0-9]+$", cf, value = TRUE)

  # Categorical, not continuous: no linear follow-up terms
  expect_false("followup" %in% cf)
  expect_false("followup_sq" %in% cf)
  # One dummy per non-reference follow-up level
  expect_gt(length(dummies), 2L)
  expect_equal(length(dummies), length(unique(m@DT$followup)) - 1L)
})

# ── internal_glmHelpers.R: weight.lag_condition ──────────────────────────────

test_that("weight.lag_condition conditions each arm's weight model on its treatment lag", {
  skip_on_cran()
  # weight.lag_condition = TRUE (default) fits each treatment arm's weight model
  # only on the rows in that arm's treatment-lag stratum (prepare.data_cached
  # subsets on tx_lag == model); = FALSE fits both arms on the full data. The
  # number of observations behind each fitted denominator model makes this visible.
  args <- list("ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
               method = "censoring")
  mk <- function(opts) suppressWarnings(do.call(SEQuential, c(list(data = copy(SEQdata)), args,
                                                              list(options = opts, verbose = FALSE))))
  # Observations behind each per-arm denominator model (fastglm df.null + 1)
  nobs <- function(m) vapply(m@weight.statistics[[1]][[1]]$coef.denominator,
                             function(x) x$df.null + 1L, integer(1))

  on  <- nobs(mk(SEQopts(weighted = TRUE, weight.lag_condition = TRUE,  seed = 1L)))
  off <- nobs(mk(SEQopts(weighted = TRUE, weight.lag_condition = FALSE, seed = 1L)))

  # FALSE: both arms fit on the full data -> equal observation counts
  expect_equal(off[[1]], off[[2]])
  # TRUE: arms fit on disjoint treatment-lag strata that partition that full data
  expect_false(on[[1]] == on[[2]])
  expect_equal(on[[1]] + on[[2]], off[[1]])
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
  skip_on_cran()
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
  skip_on_cran()
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
