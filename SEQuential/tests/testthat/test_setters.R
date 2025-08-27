test_that("Setter Tests", {
  expect_s4_class(object = SEQopts(), "SEQopts")
  expect_s4_class(object = parameter.setter(
    data = data.table(), DT = data.table(),
    id.col = NA_character_, time.col = NA_character_,
    eligible.col = NA_character_, outcome.col = NA_character_,
    treatment.col = NA_character_, time_varying.cols = list(),
    fixed.cols = list(), method = NA_character_,
    opts = SEQopts(), verbose = TRUE
  ), "SEQparams")
})
