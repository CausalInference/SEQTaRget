test_that("Pre-Expansion Excused Censoring - No excusedOne given", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "censoring",
                                       options = SEQopts(weighted = TRUE, excused = TRUE,
                                                         excused.col0 = "excusedZero")))
  expect_s4_class(model, "SEQoutput")
})

test_that("Pre-Expansion Excused Censoring - No excusedZero given", {
  data <- SEQdata
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "censoring",
                                       options = SEQopts(weighted = TRUE, excused = TRUE,
                                                         excused.col1 = "excusedOne")))
  expect_s4_class(model, "SEQoutput")
})
