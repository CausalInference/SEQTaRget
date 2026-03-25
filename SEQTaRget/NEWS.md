# SEQTaRget (development version)

- Removed several `local()` wrappers and made several code optimizations.
- Improved documentation of the datasets in the package.
- Implement check for perfect separation when fitting logistic regression models.
- Fixed a bug in and make some improvements to `internal.weights()`.
- Removed three unused slots in `SEQopts()`.
- Add alt text to figures in vignettes.
- Fixed `SEQuential()` `time.col` validation detecting and repairing non-zero-indexed time.
- Add validation for `eligible.col` values
- Add Paul Madley-Dowd as a co-author
- Add check for overlapping `time_varying.cols` and `fixed.cols`
- Add bounds validation for numeric and integer options in `SEQopts()`
- Add check for duplicate id/time combinations in input data
- Add check that `treat.level` values exist in the treatment column
- Add validation for `excused.cols` flags
- Add validation for `followup.min`/`max` ordering
- Add binary check for outcome.col in non-hazard analyses
- Add `treat.level` length validation for multinomial and non-multinomial analyses
- Add binary validation for `cense.eligible` and `weight.eligible_cols`
- Remove additional eligibility rows if not needed
- Amend defaults for `followup.min` and `weight.lower` from `-Inf` to `0`
- Fix bootstrapping for risk difference and risk ratio estimates to use paired per-iteration estimates
- Optimizations to use less RAM
- Fix duplicate scale_color_manual warning and plot.subtitle label bug in `internal.plot()`
- Run doseresponse and ITT vignette chunks on GitHub Actions
- Fix `km_curve()` returning list instead of ggplot for non-subgroup case
- Fix `km_curve()` subtitle condition
- Fix `risk.comparison()` CIs being `NA` with competing events

# SEQTaRget v1.3.6

- Added a `set.seed()` call in `internal.hazard()` to make main estimate reproducible. And also implement fix to ensure the bootstrapping, including both standard error and percentiles, is deterministic given the seed.

# SEQTaRget v1.3.5

- The `hazard_ratio()` function now correctly desscribes the estimate as "Hazard ratio"
- The bootstrapping now collects the log hazard ratio instead of the hazard ratio because the log hazard ratio has better normality properties.
- The `covariates()` function now returns more nicely formatted output (with spaces around `~` and `+` symbols in the model formulae)

# SEQTaRget v1.3.4

- Implemented some code optimizations
  - Replace a `table()` call with data.table's `.N`
  - Remove all `gc()` calls
  - Use a keyed index in bootstrapping
  - Remove some uses of `copy()`

# SEQTaRget v1.3.3

- Found and fixed a bug which caused excused switches to be overwritten.
- Fix excusing override (#115)
- Added visit option (#116)
