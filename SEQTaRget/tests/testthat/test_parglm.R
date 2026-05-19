# ── SEQopts validation ─────────────────────────────────────────────────────────

test_that("SEQopts rejects invalid glm.package", {
  expect_error(SEQopts(glm.package = "glm"), "'glm.package' must be one of")
})

test_that("SEQopts rejects non-list parglm.control", {
  expect_error(SEQopts(parglm.control = "FAST"), "'parglm.control' must be NULL")
})

test_that("SEQopts accepts a valid parglm.control object", {
  ctrl <- parglm::parglm.control(method = "FAST")
  expect_no_error(SEQopts(glm.package = "parglm", parglm.control = ctrl))
})

# ── fit_glm unit tests ─────────────────────────────────────────────────────────

test_that("fit_glm with parglm returns coefficients comparable to fastglm", {
  set.seed(42)
  X <- cbind(1, matrix(rnorm(500), 100, 5))
  y <- rbinom(100, 1, 0.3)

  m_fg <- SEQTaRget:::fit_glm(X, y, family = stats::quasibinomial(), params = SEQopts())
  m_pg <- SEQTaRget:::fit_glm(X, y, family = stats::quasibinomial(), params = SEQopts(glm.package = "parglm", nthreads = 1L))

  expect_equal(unname(coef(m_fg)), unname(coef(m_pg)), tolerance = 1e-4)
})

test_that("fit_glm with parglm respects prior weights", {
  set.seed(42)
  X <- cbind(1, matrix(rnorm(500), 100, 5))
  y <- rbinom(100, 1, 0.3)
  w <- runif(100, 0.5, 2)

  m_fg <- SEQTaRget:::fit_glm(X, y, family = stats::quasibinomial(), weights = w, params = SEQopts())
  m_pg <- SEQTaRget:::fit_glm(X, y, family = stats::quasibinomial(), weights = w, params = SEQopts(glm.package = "parglm", nthreads = 1L))

  expect_equal(unname(coef(m_fg)), unname(coef(m_pg)), tolerance = 1e-4)
})

# ── predict_model unit tests ───────────────────────────────────────────────────

test_that("predict_model returns response-scale predictions in [0, 1] for parglm", {
  set.seed(42)
  X <- cbind(1, matrix(rnorm(500), 100, 5))
  y <- rbinom(100, 1, 0.3)

  m <- SEQTaRget:::fit_glm(X, y, family = stats::quasibinomial(), params = SEQopts(glm.package = "parglm", nthreads = 1L))
  pred <- SEQTaRget:::predict_model(m, X, "response")

  expect_equal(length(pred), 100)
  expect_true(all(pred >= 0 & pred <= 1))
})

test_that("predict_model parglm and fastglm give matching predictions", {
  set.seed(42)
  X <- cbind(1, matrix(rnorm(500), 100, 5))
  y <- rbinom(100, 1, 0.3)

  m_fg <- SEQTaRget:::fit_glm(X, y, family = stats::quasibinomial(), params = SEQopts())
  m_pg <- SEQTaRget:::fit_glm(X, y, family = stats::quasibinomial(), params = SEQopts(glm.package = "parglm", nthreads = 1L))

  expect_equal(
    SEQTaRget:::predict_model(m_fg, X, "response"),
    SEQTaRget:::predict_model(m_pg, X, "response"),
    tolerance = 1e-4
  )
})

# ── Integration tests ──────────────────────────────────────────────────────────

test_that("SEQuential ITT with parglm returns SEQoutput", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(glm.package = "parglm", nthreads = 1L))
  expect_s4_class(model, "SEQoutput")
})

test_that("SEQuential parglm outcome coefficients match fastglm within tolerance", {
  data_fg <- data.table::copy(SEQdata)
  data_pg <- data.table::copy(SEQdata)

  m_fg <- suppressWarnings(SEQuential(data_fg, "ID", "time", "eligible", "tx_init", "outcome",
                                      list("N", "L", "P"), list("sex"),
                                      method = "ITT", options = SEQopts(glm.package = "fastglm")))
  m_pg <- SEQuential(data_pg, "ID", "time", "eligible", "tx_init", "outcome",
                     list("N", "L", "P"), list("sex"),
                     method = "ITT", options = SEQopts(glm.package = "parglm", nthreads = 1L))

  expect_equal(
    coef(m_fg@outcome.model[[1]][[1]]),
    coef(m_pg@outcome.model[[1]][[1]]),
    tolerance = 1e-4
  )
})

test_that("SEQuential dose-response with parglm returns SEQoutput", {
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(
    SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
               list("N", "L", "P"), list("sex"),
               method = "dose-response",
               options = SEQopts(glm.package = "parglm", weighted = TRUE))
  )
  expect_s4_class(model, "SEQoutput")
})

test_that("SEQuential parglm accepts custom parglm.control", {
  data <- data.table::copy(SEQdata)
  ctrl <- parglm::parglm.control(method = "FAST")
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(glm.package = "parglm", parglm.control = ctrl))
  expect_s4_class(model, "SEQoutput")
})

# ── Spline tests ──────────────────────────────────────────────────────────────

test_that("SEQuential parglm works with followup.spline = TRUE", {
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(
    SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
               list("N", "L", "P"), list("sex"),
               method = "ITT",
               options = SEQopts(glm.package = "parglm",
                                 followup.spline = TRUE,
                                 followup.spline.df = 4L,
                                 followup.include = FALSE),
               verbose = FALSE)
  )
  expect_s4_class(model, "SEQoutput")
  coef_names <- names(model@outcome.model[[1]][[1]]$coefficients)
  expect_true(any(grepl("^ns\\(followup,", coef_names)))
})

test_that("parglm spline coefficients match fastglm within tolerance", {
  data_fg <- data.table::copy(SEQdata)
  data_pg <- data.table::copy(SEQdata)

  opts_base <- list("ID", "time", "eligible", "tx_init", "outcome",
                    list("N", "L", "P"), list("sex"),
                    method = "ITT", verbose = FALSE)

  m_fg <- suppressWarnings(
    do.call(SEQuential, c(list(data_fg), opts_base,
                          list(options = SEQopts(glm.package = "fastglm",
                                                 followup.spline = TRUE,
                                                 followup.spline.df = 4L,
                                                 followup.include = FALSE))))
  )
  m_pg <- suppressWarnings(
    do.call(SEQuential, c(list(data_pg), opts_base,
                          list(options = SEQopts(glm.package = "parglm",
                                                 followup.spline = TRUE,
                                                 followup.spline.df = 4L,
                                                 followup.include = FALSE))))
  )

  expect_equal(
    unname(coef(m_fg@outcome.model[[1]][[1]])),
    unname(coef(m_pg@outcome.model[[1]][[1]])),
    tolerance = 1e-4
  )
})

