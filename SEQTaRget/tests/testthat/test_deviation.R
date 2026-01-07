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
