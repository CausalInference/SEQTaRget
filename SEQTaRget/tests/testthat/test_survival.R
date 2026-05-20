test_that("Survival Return", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(km_curve(model), "ggplot")
})

test_that("Bootstrapped Survival", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(km.curves = TRUE, bootstrap = TRUE, bootstrap.nboot = 2))
  expect_s4_class(model, "SEQoutput")
  expect_s3_class(km_curve(model), "ggplot")
})

test_that("Bootstrapped Survival - Percentile", {
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
