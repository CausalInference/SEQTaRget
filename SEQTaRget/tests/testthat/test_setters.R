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

test_that("parameter.setter propagates every user-facing SEQopts slot to SEQparams", {
  # Regression guard: a silently-dropped opts@<slot> in parameter.setter means
  # users' SEQopts() choices for that slot are ignored and the SEQparams
  # prototype default is used instead (this is how selection.random was a
  # no-op end-to-end despite the user setting it TRUE).
  #
  # LTFU is intentionally not in parameter.setter — it is not exposed in the
  # SEQopts() constructor and is derived from `cense` in parameter.simplifier.
  opts_slots <- setdiff(slotNames("SEQopts"), "LTFU")

  body_text <- paste(deparse(body(parameter.setter)), collapse = "\n")
  referenced <- vapply(opts_slots, function(s)
    grepl(paste0("opts@", gsub("\\.", "\\\\.", s), "(?![A-Za-z._])"),
          body_text, perl = TRUE), logical(1))

  missing_slots <- opts_slots[!referenced]
  expect_equal(missing_slots, character(0))
})
