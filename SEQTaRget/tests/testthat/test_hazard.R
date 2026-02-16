test_that("Hazard and vcov", {
  data <- data.table::copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT", options = SEQopts(hazard = TRUE))
  expect_s4_class(model, "SEQoutput")
})

test_that("Hazard estimate is reproducible with same seed", {
  args <- list("ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
               method = "ITT", options = SEQopts(hazard = TRUE, seed = 123L))
  
  run1 <- do.call(SEQuential, c(list(data = data.table::copy(SEQdata)), args))
  run2 <- do.call(SEQuential, c(list(data = data.table::copy(SEQdata)), args))
  
  expect_identical(run1@hazard, run2@hazard)
})
