library(data.table)

test_that("formula_vars strips function wrappers and returns underlying variable names", {
  expect_equal(formula_vars("ns(followup, df = 4)"), "followup")
  expect_setequal(formula_vars("tx_init_bas*ns(followup, df = 4)"),
                  c("tx_init_bas", "followup"))
  expect_setequal(formula_vars("a+b+I(c^2)+factor(d)+ns(e, df = 3)"),
                  c("a", "b", "c", "d", "e"))
  expect_equal(formula_vars(NA_character_), character(0))
  expect_equal(formula_vars(""), character(0))
})

test_that("followup.spline = TRUE produces multi-column basis with baked knots", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "ITT",
                                       options = SEQopts(followup.spline = TRUE,
                                                         followup.spline.df = 4L,
                                                         followup.include = FALSE,
                                                         km.curves = TRUE),
                                       verbose = FALSE))
  expect_s4_class(model, "SEQoutput")
  covs <- model@params@covariates
  # Knots and Boundary.knots are baked into the formula
  expect_match(covs, "ns\\(followup, ?knots ?= ?c\\([^)]+\\), ?Boundary\\.knots ?= ?c\\([^)]+\\)\\)")
  # No bare 'df = 4' token left after baking
  expect_false(grepl("df ?= ?4", covs))
  # Outcome model has 4 spline basis main-effect columns + 4 interaction
  # columns from the tx_bas*ns(followup, ...) interaction.
  coef_names <- names(model@outcome.model[[1]][[1]]$coefficients)
  spline_main <- grep("^ns\\(followup,", coef_names, value = TRUE)
  spline_main <- spline_main[!grepl(":", spline_main)]
  spline_int  <- grep(":ns\\(followup,", coef_names, value = TRUE)
  expect_equal(length(spline_main), 4L)
  expect_equal(length(spline_int), 4L)
})

test_that("followup.spline.df controls the number of basis columns", {
  data <- copy(SEQdata)
  for (df in c(3L, 5L)) {
    model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                         list("N", "L", "P"), list("sex"),
                                         method = "ITT",
                                         options = SEQopts(followup.spline = TRUE,
                                                           followup.spline.df = df,
                                                           followup.include = FALSE),
                                         verbose = FALSE))
    coef_names <- names(model@outcome.model[[1]][[1]]$coefficients)
    spline_main <- grep("^ns\\(followup,", coef_names, value = TRUE)
    spline_main <- spline_main[!grepl(":", spline_main)]
    expect_equal(length(spline_main), df,
                 info = paste0("df = ", df))
  }
})

test_that("baked spline basis is invariant to row subset (knots survive prediction grids)", {
  # If knots weren't baked, ns() would recompute them per subset and produce a
  # different basis at prediction time. With baked knots, the basis values for
  # a given followup should be identical regardless of which other rows are
  # passed alongside it.
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "ITT",
                                       options = SEQopts(followup.spline = TRUE,
                                                         followup.spline.df = 4L,
                                                         followup.include = FALSE),
                                       verbose = FALSE))
  covs <- model@params@covariates
  fmla <- stats::as.formula(paste0("~", covs))

  # Build a tiny "newdata" with only required columns and varying followup
  newdata_full <- data.frame(
    tx_init_bas = factor(0L, levels = c(0L, 1L)),
    followup    = 0:10,
    trial       = 0L,
    trial_sq    = 0L,
    sex         = factor(0L, levels = c(0L, 1L)),
    N_bas = 0, L_bas = 0, P_bas = 0
  )
  X_full <- stats::model.matrix(fmla, newdata_full)
  # Now pass a subset (just one row) — basis at followup = 5 must match
  X_sub <- stats::model.matrix(fmla, newdata_full[6, , drop = FALSE])
  spline_cols <- grep("^ns\\(", colnames(X_full), value = TRUE)
  spline_cols <- spline_cols[!grepl(":", spline_cols)]
  expect_equal(unname(X_full[6, spline_cols]),
               unname(X_sub[1, spline_cols]))
})

test_that("user-supplied covariates with ns() pass through expansion and are baked", {
  data <- copy(SEQdata)
  # Hand-roll a covariates string that includes ns() — expansion should treat
  # it as referring to followup, not as a column called "ns(followup, df = 3)".
  custom <- "tx_init_bas+ns(followup, df = 3)+trial+trial_sq+sex+N_bas+L_bas+P_bas"
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "ITT",
                                       options = SEQopts(covariates = custom,
                                                         followup.include = FALSE),
                                       verbose = FALSE))
  expect_s4_class(model, "SEQoutput")
  # User-supplied df-form ns() terms are baked too, so the basis at prediction
  # time matches the one fitted (SEQopts strips whitespace from covariates).
  expect_match(model@params@covariates,
               "ns\\(followup, ?knots ?= ?c\\([^)]+\\), ?Boundary\\.knots ?= ?c\\([^)]+\\)\\)")
  expect_false(grepl("df ?= ?3", model@params@covariates))
  coef_names <- names(model@outcome.model[[1]][[1]]$coefficients)
  expect_equal(length(grep("^ns\\(followup,", coef_names)), 3L)
})

test_that("followup.spline.df validation rejects non-positive integers", {
  expect_error(SEQopts(followup.spline = TRUE, followup.spline.df = 0L),
               "followup.spline.df")
  expect_error(SEQopts(followup.spline = TRUE, followup.spline.df = -1L),
               "followup.spline.df")
})

test_that("weight.spline.df validation rejects non-positive integers", {
  expect_error(SEQopts(weight.spline = TRUE, weight.spline.df = 0L),
               "weight.spline.df")
  expect_error(SEQopts(weight.spline = TRUE, weight.spline.df = -1L),
               "weight.spline.df")
  expect_warning(SEQopts(weight.spline = TRUE, weight.spline.df = 2L),
                 "weight.spline.df")
})

test_that("weight.spline replaces the time quadratics in the default weight models", {
  mk <- function(preexpansion) {
    params <- parameter.setter(
      data = data.table(), DT = data.table(), id.col = "ID",
      time.col = "time", eligible.col = "eligible",
      outcome.col = "outcome", treatment.col = "treatment",
      time_varying.cols = list("N", "L", "P"), fixed.cols = list("sex"),
      method = "censoring", verbose = FALSE,
      opts = SEQopts(weighted = TRUE, weight.preexpansion = preexpansion,
                     weight.spline = TRUE, weight.spline.df = 4L, cense = "LTFU")
    )
    lapply(c("numerator", "denominator"), function(type)
      c(weight = create.default.weight.covariates(params, type),
        cense = create.default.LTFU.covariates(params, type)))
  }

  post <- unlist(mk(FALSE))
  # Post-expansion weight models run on two time axes: both become spline bases
  expect_true(all(grepl("ns(followup, df = 4)", post, fixed = TRUE)))
  expect_true(all(grepl("ns(trial, df = 4)", post, fixed = TRUE)))
  expect_false(any(grepl("followup_sq|trial_sq", post)))

  pre <- unlist(mk(TRUE))
  # Pre-expansion weight models only see the subject's own time column
  expect_true(all(grepl("ns(time, df = 4)", pre, fixed = TRUE)))
  expect_false(any(grepl("time_sq", pre)))
})

test_that("weight.spline = FALSE leaves the default weight models quadratic in time", {
  params <- parameter.setter(
    data = data.table(), DT = data.table(), id.col = "ID",
    time.col = "time", eligible.col = "eligible",
    outcome.col = "outcome", treatment.col = "treatment",
    time_varying.cols = list("N", "L", "P"), fixed.cols = list("sex"),
    method = "censoring", verbose = FALSE,
    opts = SEQopts(weighted = TRUE, weight.preexpansion = FALSE)
  )
  covs <- create.default.weight.covariates(params, "denominator")
  expect_false(grepl("ns(", covs, fixed = TRUE))
  expect_true(all(c("followup", "followup_sq", "trial", "trial_sq") %in%
                    unlist(strsplit(covs, "\\+"))))
})

test_that("weight-model ns() knots are baked from the data the weight models are fit on", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "censoring",
                                       options = SEQopts(weighted = TRUE,
                                                         weight.preexpansion = FALSE,
                                                         weight.spline = TRUE,
                                                         weight.spline.df = 4L,
                                                         data.return = TRUE),
                                       verbose = FALSE))
  for (covs in c(model@params@numerator, model@params@denominator)) {
    expect_match(covs, "ns\\(followup, ?knots ?= ?c\\([^)]+\\), ?Boundary\\.knots ?= ?c\\([^)]+\\)\\)")
    expect_match(covs, "ns\\(trial, ?knots ?= ?c\\([^)]+\\), ?Boundary\\.knots ?= ?c\\([^)]+\\)\\)")
    expect_false(grepl("df ?= ?4", covs))
  }

  # The baked knots are the quantiles ns() would place over the post-expansion
  # data the weight models are fit on - not, e.g., the pre-expansion time column
  knots_of <- function(covs, var) {
    token <- regmatches(covs, regexpr(paste0("ns\\(", var, ",[^)]+\\), ?Boundary\\.knots ?= ?c\\([^)]+\\)\\)"), covs))
    as.numeric(unlist(regmatches(token, gregexpr("[0-9]+(\\.[0-9]+)?", token))))
  }
  fu <- model@DT$followup
  expected <- c(quantile(fu, c(0.25, 0.5, 0.75), names = FALSE), range(fu))
  expect_equal(knots_of(model@params@denominator, "followup"), expected)
})

test_that("a df-form ns() weight model matches the same model with explicit knots", {
  # Without baked knots, ns() rebuilds its basis from whatever rows it is handed,
  # so the basis used to predict the weights differs from the one fitted and the
  # two specifications - which are mathematically the same model - disagree.
  data <- copy(SEQdata)
  fit <- function(followup_term) {
    suppressWarnings(SEQuential(copy(data), "ID", "time", "eligible", "tx_init", "outcome",
                                list("N", "L", "P"), list("sex"),
                                method = "censoring",
                                options = SEQopts(weighted = TRUE,
                                                  weight.preexpansion = FALSE,
                                                  data.return = TRUE,
                                                  numerator = paste0("sex+N_bas+L_bas+P_bas+trial+trial_sq+", followup_term),
                                                  denominator = paste0("sex+N+L+P+N_bas+L_bas+P_bas+trial+trial_sq+", followup_term)),
                                verbose = FALSE))
  }
  df_form <- fit("ns(followup, df = 4)")

  fu <- df_form@DT$followup
  explicit <- sprintf("ns(followup, knots = c(%s), Boundary.knots = c(%s, %s))",
                      paste(quantile(fu, c(0.25, 0.5, 0.75), names = FALSE), collapse = ", "),
                      min(fu), max(fu))
  expect_equal(df_form@DT$weight, fit(explicit)@DT$weight)
})

test_that("arm-specific weight models each get their ns() knots baked", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "censoring",
                                       options = SEQopts(weighted = TRUE,
                                                         weight.preexpansion = FALSE,
                                                         numerator = c("sex+ns(followup, df = 3)",
                                                                       "sex+ns(followup, df = 5)"),
                                                         denominator = c("sex+N+L+P+ns(followup, df = 3)",
                                                                         "sex+N+L+P+ns(followup, df = 5)")),
                                       verbose = FALSE))
  expect_length(model@params@denominator, 2L)
  expect_false(any(grepl("df ?= ?[35]", model@params@denominator)))
  # df = 3 gives 2 interior knots, df = 5 gives 4
  n_knots <- vapply(model@params@denominator, function(covs) {
    token <- regmatches(covs, regexpr("knots ?= ?c\\([^)]+\\)", covs))
    length(unlist(strsplit(token, ",")))
  }, integer(1))
  expect_equal(unname(n_knots), c(2L, 4L))
})

test_that("bake_spline_knots leaves terms it cannot bake alone", {
  DT <- data.table(followup = 0:100, grp = factor(rep(c("a", "b"), length.out = 101)))
  # Variable absent from the data
  expect_equal(bake_spline_knots("ns(missing, df = 4)", DT), "ns(missing, df = 4)")
  # Non-numeric variable
  expect_equal(bake_spline_knots("ns(grp, df = 4)", DT), "ns(grp, df = 4)")
  # Already-explicit knots
  explicit <- "ns(followup, knots = c(1, 2), Boundary.knots = c(0, 100))"
  expect_equal(bake_spline_knots(explicit, DT), explicit)
  # NA and empty formulas pass through
  expect_equal(bake_spline_knots(NA_character_, DT), NA_character_)
  expect_equal(bake_spline_knots(character(0), DT), character(0))
  # Several terms in one formula, each baked against its own variable
  baked <- bake_spline_knots("ns(followup, df = 2)+ns(followup, df = 4)", DT)
  expect_equal(length(gregexpr("Boundary.knots", baked, fixed = TRUE)[[1]]), 2L)
  expect_false(grepl("df ?= ?[24]", baked))
})

test_that("bake_spline_knots drops tied interior knots with a warning", {
  # Only 3 distinct values, so ns(df = 4)'s quantile knots are tied / on the
  # boundary; baking those verbatim would give a silently degenerate basis
  DT <- data.table(x = rep(0:2, each = 50))
  expect_warning(baked <- bake_spline_knots("ns(x, df = 4)", DT),
                 "too few distinct values")
  expect_match(baked, "^ns\\(x, knots ?= ?c\\(1\\), Boundary\\.knots ?= ?c\\(0, 2\\)\\)$")
})
