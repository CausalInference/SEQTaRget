library(data.table)

test_that("Post-Expansion Excused Censoring", {
  data <- copy(SEQdata)
  data[N > 10, deviation := TRUE]
  model <- expect_error(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "censoring",
                                       options = SEQopts(
                                         weighted = TRUE, deviation = TRUE,
                                         deviation.col = "deviation",
                                         weight.preexpansion = FALSE)
  ))
})

test_that("Each deviation excused column excuses deviations only in its own arm", {
  skip_on_cran()
  # deviation = TRUE is disabled in SEQopts(), so set the slots directly
  censored <- function(excused_cols) {
    data <- copy(SEQdata)
    data[, deviation := as.integer(N > 10)]
    opts <- SEQopts(data.return = TRUE)
    opts@deviation <- TRUE
    opts@deviation.col <- "deviation"
    opts@deviation.conditions <- list("== 1", "== 1")
    if (!all(is.na(excused_cols))) {
      opts@deviation.excused <- TRUE
      opts@deviation.excused_cols <- as.list(excused_cols)
    }
    model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                         list("N", "L", "P"), list("sex"), method = "censoring", verbose = FALSE,
                                         options = opts))
    SEQ_data(model)[, .(censored = sum(censored)), keyby = tx_init_bas]$censored
  }
  none <- censored(c(NA, NA))
  zero <- censored(c("excusedZero", NA))
  expect_lt(zero[1], none[1])
  expect_equal(zero[2], none[2])
  one <- censored(c(NA, "excusedOne"))
  expect_equal(one[1], none[1])
  expect_lt(one[2], none[2])
})
