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

test_that("user-supplied covariates with ns() pass through expansion (all.vars extraction)", {
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
  # User-supplied covariates are not auto-baked; the ns() term survives expansion
  # (SEQopts strips whitespace from covariates, so allow optional spaces in the regex).
  expect_match(model@params@covariates, "ns\\(followup, ?df ?= ?3\\)")
})

test_that("followup.spline.df validation rejects non-positive integers", {
  expect_error(SEQopts(followup.spline = TRUE, followup.spline.df = 0L),
               "followup.spline.df")
  expect_error(SEQopts(followup.spline = TRUE, followup.spline.df = -1L),
               "followup.spline.df")
})
