# Changelog

## SEQTaRget (development version)

- Removed several [`local()`](https://rdrr.io/r/base/eval.html) wrappers
  and made several code optimizations.
- Improved documentation of the datasets in the package.
- Implement check for perfect separation when fitting logistic
  regression models.
- Fixed a bug in and make some improvements to
  [`internal.weights()`](https://causalinference.github.io/SEQTaRget/dev/reference/internal.weights.md).
- Removed three unused slots in
  [`SEQopts()`](https://causalinference.github.io/SEQTaRget/dev/reference/SEQopts.md).
- Add alt text to figures in vignettes.
- Fixed
  [`SEQuential()`](https://causalinference.github.io/SEQTaRget/dev/reference/SEQuential.md)
  `time.col` validation detecting and repairing non-zero-indexed time.
- Add validation for `eligible.col` values and transition constraint
- Add Paul Madley-Dowd as a co-author
- Add check for overlapping `time_varying.cols` and `fixed.cols`
- Add bounds validation for numeric and integer options in
  [`SEQopts()`](https://causalinference.github.io/SEQTaRget/dev/reference/SEQopts.md)
- Add check for duplicate id/time combinations in input data
- Add check that `treat.level` values exist in the treatment column
- Add validation for `excused.cols` flags
- Add validation for `followup.min`/`max` ordering
- Add binary check for outcome.col in non-hazard analyses
- Add `treat.level` length validation for multinomial and
  non-multinomial analyses
- Add binary validation for `cense.eligible` and `weight.eligible_cols`

## SEQTaRget v1.3.6

CRAN release: 2026-02-16

- Added a [`set.seed()`](https://rdrr.io/r/base/Random.html) call in
  [`internal.hazard()`](https://causalinference.github.io/SEQTaRget/dev/reference/internal.hazard.md)
  to make main estimate reproducible. And also implement fix to ensure
  the bootstrapping, including both standard error and percentiles, is
  deterministic given the seed.

## SEQTaRget v1.3.5

CRAN release: 2026-02-05

- The
  [`hazard_ratio()`](https://causalinference.github.io/SEQTaRget/dev/reference/hazard_ratio.md)
  function now correctly desscribes the estimate as “Hazard ratio”
- The bootstrapping now collects the log hazard ratio instead of the
  hazard ratio because the log hazard ratio has better normality
  properties.
- The
  [`covariates()`](https://causalinference.github.io/SEQTaRget/dev/reference/covariates.md)
  function now returns more nicely formatted output (with spaces around
  `~` and `+` symbols in the model formulae)

## SEQTaRget v1.3.4

CRAN release: 2026-01-23

- Implemented some code optimizations
  - Replace a [`table()`](https://rdrr.io/r/base/table.html) call with
    data.table’s `.N`
  - Remove all [`gc()`](https://rdrr.io/r/base/gc.html) calls
  - Use a keyed index in bootstrapping
  - Remove some uses of `copy()`

## SEQTaRget v1.3.3

CRAN release: 2026-01-08

- Found and fixed a bug which caused excused switches to be overwritten.
- Fix excusing override
  ([\#115](https://github.com/CausalInference/SEQTaRget/issues/115))
- Added visit option
  ([\#116](https://github.com/CausalInference/SEQTaRget/issues/116))
