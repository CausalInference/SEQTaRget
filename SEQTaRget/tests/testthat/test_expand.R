library(data.table)

expand_with <- function(batch, method = "censoring", data = SEQdata, ...) {
  model <- suppressWarnings(SEQuential(copy(data), "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"), method = method, verbose = FALSE,
                                       options = SEQopts(expand.batch_size = batch, data.return = TRUE, ...)))
  if (is.data.table(model)) model else list(data = SEQ_data(model), coef = coef(model@outcome.model[[1]][[1]]))
}

test_that("Expanding the IDs in batches gives the same data and estimates as expanding them at once", {
  skip_on_cran()
  # 7 IDs per batch splits SEQdata's 300 IDs into 43 batches
  expect_identical(expand_with(7, "ITT", expand.only = TRUE), expand_with(Inf, "ITT", expand.only = TRUE))
  expect_identical(expand_with(7, weighted = TRUE), expand_with(Inf, weighted = TRUE))
  expect_identical(expand_with(7, weighted = TRUE, excused = TRUE, excused.cols = c("excusedZero", "excusedOne")),
                   expand_with(Inf, weighted = TRUE, excused = TRUE, excused.cols = c("excusedZero", "excusedOne")))
  expect_identical(expand_with(7, "dose-response", weighted = TRUE), expand_with(Inf, "dose-response", weighted = TRUE))
  expect_identical(expand_with(7, "ITT", followup.min = 2, followup.max = 10),
                   expand_with(Inf, "ITT", followup.min = 2, followup.max = 10))
  # The untreated trials are sampled before batching, so the sample is the same
  expect_identical(expand_with(7, weighted = TRUE, selection.random = TRUE, selection.prob = 0.3, seed = 7),
                   expand_with(Inf, weighted = TRUE, selection.random = TRUE, selection.prob = 0.3, seed = 7))
})

test_that("Batched expansion reports the same row counts", {
  skip_on_cran()
  counts <- function(batch) {
    grep("observations", capture.output(
      SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                 method = "censoring", options = SEQopts(expand.only = TRUE, expand.batch_size = batch))), value = TRUE)
  }
  expect_identical(counts(7), counts(Inf))
})

test_that("expand.batch_size is checked", {
  expect_error(SEQopts(expand.batch_size = 0), "expand.batch_size")
  expect_error(SEQopts(expand.batch_size = c(1, 2)), "expand.batch_size")
  expect_s4_class(SEQopts(expand.batch_size = 1000), "SEQopts")
})
