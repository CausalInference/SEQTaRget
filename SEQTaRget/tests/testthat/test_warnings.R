test_that("Weighted ITT", {
  data <- data.table::copy(SEQdata)
  expect_warning(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "ITT",
    options = SEQopts(weighted = TRUE)
  ))
})

test_that("Unexcused Excused Censoring", {
  data <- data.table::copy(SEQdata)
  expect_warning(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(weighted = TRUE, excused = TRUE)
  ))
})
