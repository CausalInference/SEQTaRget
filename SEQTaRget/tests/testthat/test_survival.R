test_that("Survival Return", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(km_curve(model), "ggplot")
})

test_that("Bootstrapped Survival", {
  skip_on_cran()
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE, bootstrap = TRUE, bootstrap.nboot = 2))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(km_curve(model), "ggplot")
})

test_that("Bootstrapped Survival - Percentile", {
  skip_on_cran()
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE, bootstrap = TRUE, bootstrap.nboot = 2, bootstrap.CI_method = "percentile"))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(km_curve(model), "ggplot")
})

test_that("Survival output followup labeling - followup=k represents survival after k intervals", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE))
  surv <- model@survival.data[[1]]
  survival_max <- max(data[["time"]])
  # Output should run from followup=0 (baseline) to followup=survival.max+1 (end of final interval)
  expect_equal(min(surv$followup), 0)
  expect_equal(max(surv$followup), survival_max + 1)
  expect_equal(length(unique(surv$followup)), survival_max + 2)
  # Baseline row should have risk=0 and surv=1
  baseline <- surv[surv$followup == 0, ]
  expect_true(all(baseline$value[grepl("^risk_", baseline$variable)] == 0))
  expect_true(all(baseline$value[grepl("^surv_", baseline$variable)] == 1))
})

test_that("Bootstrapped Survival - Competing Event CIs present", {
  skip_on_cran()
  data <- data.table::copy(SEQdata)
  set.seed(42)
  data[, compevent := as.integer(runif(.N) < 0.02)]
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE, compevent = "compevent",
                                                        bootstrap = TRUE, bootstrap.nboot = 2))
  expect_s4_class(model, "SEQoutput")
  ci_cols <- c("RD 95% LCI", "RD 95% UCI", "RR 95% LCI", "RR 95% UCI")
  expect_true(all(ci_cols %in% names(model@risk.comparison[[1]])))
  expect_true(all(!is.na(model@risk.comparison[[1]][, ci_cols, with = FALSE])))
})

test_that("risk.comparison reports bootstrap SEs for RD and log(RR) under both CI methods", {
  skip_on_cran()
  se_cols <- c("RD SE", "log(RR) SE")
  fit <- function(method) {
    suppressWarnings(SEQuential(data.table::copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                     list("N", "L", "P"), list("sex"), method = "ITT",
                     options = SEQopts(km.curves = TRUE, bootstrap = TRUE, bootstrap.nboot = 25,
                                       seed = 7L, bootstrap.CI_method = method), verbose = FALSE))
  }

  rc_se <- risk_comparison(fit("se"))
  expect_true(all(se_cols %in% names(rc_se)))
  expect_true(all(!is.na(rc_se[, se_cols, with = FALSE])))
  # SEs are non-negative
  expect_true(all(rc_se[["RD SE"]] >= 0) && all(rc_se[["log(RR) SE"]] >= 0))
  # Under the "se" CI method the stored RD SE exactly reconstructs the symmetric RD CI
  z <- qnorm(0.975)
  expect_equal(rc_se[["RD 95% LCI"]], rc_se[["Risk Difference"]] - z * rc_se[["RD SE"]])
  expect_equal(rc_se[["RR 95% LCI"]], exp(log(rc_se[["Risk Ratio"]]) - z * rc_se[["log(RR) SE"]]))

  # The SEs are a property of the bootstrap distribution, so they are reported
  # (and identical) even when percentile CIs are requested.
  rc_pct <- risk_comparison(fit("percentile"))
  expect_true(all(se_cols %in% names(rc_pct)))
  expect_true(all(!is.na(rc_pct[, se_cols, with = FALSE])))
  expect_equal(rc_pct[, se_cols, with = FALSE], rc_se[, se_cols, with = FALSE])
})

test_that("CI column labels track bootstrap.CI rather than hardcoding 95%", {
  skip_on_cran()
  model <- suppressWarnings(SEQuential(data.table::copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                   list("N", "L", "P"), list("sex"), method = "ITT",
                   options = SEQopts(km.curves = TRUE, bootstrap = TRUE, bootstrap.nboot = 10,
                                     seed = 7L, bootstrap.CI = 0.9), verbose = FALSE))
  rc <- risk_comparison(model)
  rd <- risk_data(model)
  expect_true(all(c("RR 90% LCI", "RR 90% UCI", "RD 90% LCI", "RD 90% UCI") %in% names(rc)))
  expect_true(all(c("90% LCI", "90% UCI") %in% names(rd)))
  # The old hardcoded 95% labels must not appear when a 90% CI was requested
  expect_false(any(grepl("95%", names(rc))))
  expect_false(any(grepl("95%", names(rd))))
})

test_that("risk.times reports RD/RR at requested follow-up times plus the final time", {
  data <- data.table::copy(SEQdata)
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(km.curves = TRUE, risk.times = c(2, 5),
                                        bootstrap = TRUE, bootstrap.nboot = 5),
                      verbose = FALSE))
  rc <- risk_comparison(model)
  expect_true("Followup" %in% names(rc))
  final <- max(model@survival.data[[1]]$followup)
  # Requested times (2, 5) are snapped onto the grid and the final time is always included
  expect_setequal(unique(rc$Followup), c(2, 5, final))
  # CIs present at every reported time
  ci_cols <- c("RD 95% LCI", "RD 95% UCI", "RR 95% LCI", "RR 95% UCI")
  expect_true(all(ci_cols %in% names(rc)))
  expect_true(all(!is.na(rc[, ci_cols, with = FALSE])))
})

test_that("risk.times default reports only the final follow-up time", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                      list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE), verbose = FALSE)
  rc <- risk_comparison(model)
  final <- max(model@survival.data[[1]]$followup)
  expect_equal(unique(rc$Followup), final)
})

test_that("risk.times errors when a requested time exceeds maximum follow-up", {
  data <- data.table::copy(SEQdata)
  expect_error(
    SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
               list("N", "L", "P"), list("sex"),
               method = "ITT", options = SEQopts(km.curves = TRUE, risk.times = 1e6),
               verbose = FALSE),
    "maximum follow-up"
  )
})
