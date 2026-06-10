library(data.table)

test_that("Expanded dataset contains no trials beyond the last eligible row per subject", {
  # max(trial) in the expanded data must equal the 0-based index of the last
  # eligible row across all subjects — trailing ineligible rows must not become trials.
  model <- suppressWarnings(SEQuential(copy(SEQdata),
    "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"), method = "ITT",
    options = SEQopts(data.return = TRUE)
  ))
  last_elig_idx <- SEQdata[, .(last_elig = max(which(eligible == 1L)) - 1L), by = ID]
  expect_equal(max(model@DT$trial), max(last_elig_idx$last_elig))
})

test_that("Expansion truncates trials at first outcome event - subject with early outcome not carried forward", {
  # Subject 1 has outcome=1 at time=0 (the only eligible period), then continues in the
  # dataset at times 1-3 with outcome=0. Without truncation they would appear in the
  # expanded data for all four periods with the later outcome=0 rows overwriting the event.
  # Subject 2 has no outcome and serves as a control.
  dt <- data.table::data.table(
    ID        = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    time      = c(0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L),
    eligible  = c(1L, 0L, 0L, 0L, 1L, 1L, 1L, 1L),
    treatment = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    outcome   = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    N         = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    sex       = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L)
  )
  model <- SEQuential(dt, "ID", "time", "eligible", "treatment", "outcome",
                      list("N"), list("sex"),
                      method = "ITT",
                      options = SEQopts(data.return = TRUE),
                      verbose = FALSE)
  # Subject 1's only trial (trial=0) should contain exactly one row (followup=0, outcome=1)
  s1_trial0 <- model@DT[ID == 1L & trial == 0L]
  expect_equal(nrow(s1_trial0), 1L)
  expect_equal(s1_trial0$outcome, 1L)
})

test_that("Expansion truncates trials at first outcome event - subject with early outcome not carried forward - test 2", {
  # Subject 1 has outcome=1 at time=0 (the only eligible period), then continues in the
  # dataset at times 1-3 with outcome=1. Without truncation they would appear in the
  # expanded data for all four periods with the later outcome=1 rows overwriting the event.
  # Subject 2 has no outcome and serves as a control.
  dt <- data.table::data.table(
    ID        = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    time      = c(0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L),
    eligible  = c(1L, 0L, 0L, 0L, 1L, 1L, 1L, 1L),
    treatment = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    outcome   = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    N         = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),
    sex       = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L)
  )
  model <- SEQuential(dt, "ID", "time", "eligible", "treatment", "outcome",
                      list("N"), list("sex"),
                      method = "ITT",
                      options = SEQopts(data.return = TRUE),
                      verbose = FALSE)
  # Subject 1's only trial (trial=0) should contain exactly one row (followup=0, outcome=1)
  s1_trial0 <- model@DT[ID == 1L & trial == 0L]
  expect_equal(nrow(s1_trial0), 1L)
  expect_equal(s1_trial0$outcome, 1L)
})

test_that("Pre-Expansion Excused Censoring - No excusedOne given", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.cols = c("excusedZero")))
  expect_s4_class(model, "SEQoutput")
})

test_that("Pre-Expansion Excused Censoring - No excusedZero given", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
    list("N", "L", "P"), list("sex"),
    method = "censoring",
    options = SEQopts(
      weighted = TRUE, excused = TRUE,
      excused.cols = c(NA, "excusedOne")))
  expect_s4_class(model, "SEQoutput")
})

test_that("Unweighted Censoring and Dose-Reponse", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
             method = "censoring",
             options = SEQopts(weighted = FALSE))
  expect_s4_class(model, "SEQoutput")
})

test_that("ITT - Followup Class", {
  data <- copy(SEQdata)[time <= 5, ]
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(followup.class = TRUE, followup.include = FALSE)))
  expect_s4_class(model, "SEQoutput")
})

test_that("followup.class + km.curves produces survival curves without predict mismatch", {
  data <- copy(SEQdata)[time <= 5, ]
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(followup.class = TRUE, followup.include = FALSE, km.curves = TRUE)))
  expect_s4_class(model, "SEQoutput")
  expect_true(nrow(model@survival.data[[1]]) > 0)
})

test_that("followup.class + km.curves + compevent produces survival curves without predict mismatch", {
  set.seed(42)
  data <- copy(SEQdata)[time <= 5, ]
  data[, compevent := as.integer(runif(.N) < 0.05)]
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(followup.class = TRUE, followup.include = FALSE,
                                        km.curves = TRUE, compevent = "compevent")))
  expect_s4_class(model, "SEQoutput")
  expect_true(nrow(model@survival.data[[1]]) > 0)
})

test_that("ITT - Followup Spline", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(followup.spline = TRUE, followup.include = FALSE))
  expect_s4_class(model, "SEQoutput")
})

test_that("Error 107 - followup.include = FALSE failing to create covariates", {
  data <- copy(SEQdata)
  model <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", list("N", "L", "P"), list("sex"),
                      method = "ITT",
                      options = SEQopts(followup.include = FALSE))
  expect_s4_class(model, "SEQoutput")
})

test_that("expand.only = TRUE returns expanded data.table and matches data.return DT", {
  # When expand.only = TRUE, SEQuential should short-circuit after expansion and
  # return the expanded data.table directly. The contents should match the DT slot
  # from a full run with data.return = TRUE.
  data <- copy(SEQdata)
  expanded <- suppressWarnings(SEQuential(copy(data), "ID", "time", "eligible", "tx_init", "outcome",
                                          list("N", "L", "P"), list("sex"),
                                          method = "ITT",
                                          options = SEQopts(expand.only = TRUE),
                                          verbose = FALSE))
  expect_s3_class(expanded, "data.table")
  expect_true(nrow(expanded) > 0L)
  expect_true(all(c("ID", "trial", "period", "followup", "outcome") %in% names(expanded)))

  full <- suppressWarnings(SEQuential(copy(data), "ID", "time", "eligible", "tx_init", "outcome",
                                      list("N", "L", "P"), list("sex"),
                                      method = "ITT",
                                      options = SEQopts(data.return = TRUE),
                                      verbose = FALSE))
  setkeyv(expanded, c("ID", "trial", "followup"))
  ref <- copy(full@DT)
  setkeyv(ref, c("ID", "trial", "followup"))
  expect_equal(nrow(expanded), nrow(ref))
  expect_equal(expanded$ID, ref$ID)
  expect_equal(expanded$trial, ref$trial)
  expect_equal(expanded$followup, ref$followup)
  expect_equal(expanded$outcome, ref$outcome)
})

test_that("info follow-up tables count intervals and unique subjects per treatment arm", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(copy(data), "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "censoring",
                                       options = SEQopts(weighted = FALSE, data.return = TRUE),
                                       verbose = FALSE))
  fnn <- model@info$followup.nonunique
  fu <- model@info$followup.unique
  expect_type(fnn, "list")
  expect_length(fnn, 1L)
  nn <- fnn[[1]]; un <- fu[[1]]
  expect_true(all(c("tx_init_bas", "n") %in% names(nn)))
  expect_true(all(c("tx_init_bas", "n") %in% names(un)))
  expect_setequal(as.character(nn$tx_init_bas), c("0", "1"))

  # Non-unique: per-arm interval count == non-NA outcome rows in the analysis DT
  expected_nn <- model@DT[!is.na(outcome), .N, by = tx_init_bas]
  setkey(expected_nn, tx_init_bas); setkeyv(nn, "tx_init_bas")
  expect_equal(nn$n, expected_nn$N)
  expect_equal(sum(nn$n), model@DT[!is.na(outcome), .N])

  # Unique: per-arm distinct subject count
  expected_un <- model@DT[!is.na(outcome), uniqueN(ID), by = tx_init_bas]
  setkey(expected_un, tx_init_bas); setkeyv(un, "tx_init_bas")
  expect_equal(un$n, expected_un$V1)

  # Unique counts cannot exceed interval counts within an arm
  expect_true(all(un$n <= nn$n))
})

test_that("info follow-up tables are reported per subgroup", {
  data <- copy(SEQdata)
  model <- suppressWarnings(SEQuential(copy(data), "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "ITT",
                                       options = SEQopts(subgroup = "sex"),
                                       verbose = FALSE))
  expect_named(model@info$followup.unique, c("sex_0", "sex_1"))
  expect_named(model@info$followup.nonunique, c("sex_0", "sex_1"))
  for (tbl in model@info$followup.nonunique) expect_true(all(c("tx_init_bas", "n") %in% names(tbl)))
})

test_that("info compevent tables count intervals and unique subjects per treatment arm", {
  set.seed(42)
  data <- copy(SEQdata)
  data[, compevent := as.integer(runif(.N) < 0.05)]
  model <- suppressWarnings(SEQuential(copy(data), "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P"), list("sex"),
                                       method = "ITT",
                                       options = SEQopts(compevent = "compevent", data.return = TRUE),
                                       verbose = FALSE))
  cnn <- model@info$compevent.nonunique
  cun <- model@info$compevent.unique
  expect_type(cnn, "list")
  expect_length(cnn, 1L)
  nn <- cnn[[1]]; un <- cun[[1]]
  expect_true(all(c("tx_init_bas", "compevent", "n") %in% names(nn)))
  expect_true(all(c("tx_init_bas", "compevent", "n") %in% names(un)))
  expect_setequal(as.character(nn$tx_init_bas), c("0", "1"))

  # Non-unique: per-arm interval count == compevent == 1 rows in the analysis DT
  expected_nn <- model@DT[compevent == 1L, .N, by = tx_init_bas]
  setkey(expected_nn, tx_init_bas); setkeyv(nn, "tx_init_bas")
  expect_equal(nn$n, expected_nn$N)

  # Unique: per-arm distinct subject count with compevent == 1
  expected_un <- model@DT[compevent == 1L, uniqueN(ID), by = tx_init_bas]
  setkey(expected_un, tx_init_bas); setkeyv(un, "tx_init_bas")
  expect_equal(un$n, expected_un$V1)

  # Unique counts cannot exceed interval counts within an arm
  expect_true(all(un$n <= nn$n))

  # compevent tables are NA when compevent not specified
  model_no_ce <- suppressWarnings(SEQuential(copy(SEQdata), "ID", "time", "eligible", "tx_init", "outcome",
                                             list("N", "L", "P"), list("sex"),
                                             method = "ITT", options = SEQopts(), verbose = FALSE))
  expect_true(is.na(model_no_ce@info$compevent.unique))
  expect_true(is.na(model_no_ce@info$compevent.nonunique))
})

test_that("bootstrap ID relabeling is injective for large numeric IDs", {
  # Small non-negative integer IDs keep the fast arithmetic relabeling
  small <- 1:50
  f <- SEQTaRget:::bootstrap_id_relabeler(small, 40L)
  ids <- f(rep(small, each = 2), seq_len(100))
  expect_true(is.numeric(ids))
  expect_equal(anyDuplicated(ids), 0L)

  # 10-digit IDs overflow the 2^53 exact-integer range under the arithmetic
  # scheme: consecutive copy indices round to the same double, silently merging
  # distinct bootstrap copies of a subject. These must fall back to strings.
  big <- 9e9 + 1:50
  g <- SEQTaRget:::bootstrap_id_relabeler(big, 40L)
  ids_big <- g(rep(big[1], 100), seq_len(100))
  expect_equal(anyDuplicated(ids_big), 0L)

  # The old arithmetic scheme demonstrably collides at this magnitude
  id_mult <- max(big) + 1
  old_ids <- as.numeric(big[1]) * id_mult + seq_len(100)
  expect_gt(anyDuplicated(old_ids), 0L)

  # Negative and non-integer numeric IDs also use the collision-proof path
  h <- SEQTaRget:::bootstrap_id_relabeler(c(-5, 1, 10), 3L)
  expect_true(is.character(h(c(-5, -5), 1:2)))
  k <- SEQTaRget:::bootstrap_id_relabeler(c(0.5, 1.5), 2L)
  expect_true(is.character(k(c(0.5, 0.5), 1:2)))
})

test_that("character time-varying covariates get a stable factor encoding", {
  # A character categorical among the time-varying covariates must be coerced to a
  # factor (levels fixed from the full data) so bootstrap resamples cannot realise
  # different level sets and trigger "newdata does not match fitted model".
  set.seed(1)
  data <- copy(SEQdata)
  data[, grp := sample(c("a", "b", "c"), .N, replace = TRUE)]
  model <- suppressWarnings(SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                                       list("N", "L", "P", "grp"), list("sex"),
                                       method = "ITT",
                                       options = SEQopts(data.return = TRUE, bootstrap = TRUE,
                                                         bootstrap.nboot = 3),
                                       verbose = FALSE))
  expect_s4_class(model, "SEQoutput")
  # The baseline counterpart used in the ITT model must be a factor in the expanded data
  expect_true("grp_bas" %in% names(model@DT))
  expect_s3_class(model@DT$grp_bas, "factor")
  # Numeric time-varying covariates must remain numeric (not turned into factors)
  expect_true(is.numeric(model@DT$N_bas))
})
