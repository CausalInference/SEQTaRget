# Changelog

## SEQTaRget v1.3.6.9000

- Removed several [`local()`](https://rdrr.io/r/base/eval.html) wrappers
  and made several code optimizations.
- Improved documentation of the datasets in the package.

## SEQTaRget v1.3.6

CRAN release: 2026-02-16

- Added a [`set.seed()`](https://rdrr.io/r/base/Random.html) call in
  [`internal.hazard()`](https://causalinference.github.io/SEQTaRget/reference/internal.hazard.md)
  to make main estimate reproducible. And also implement fix to ensure
  the bootstrapping, including both standard error and percentiles, is
  deterministic given the seed.

## SEQTaRget v1.3.5

CRAN release: 2026-02-05

- The
  [`hazard_ratio()`](https://causalinference.github.io/SEQTaRget/reference/hazard_ratio.md)
  function now correctly desscribes the estimate as “Hazard ratio”
- The bootstrapping now collects the log hazard ratio instead of the
  hazard ratio because the log hazard ratio has better normality
  properties.
- The
  [`covariates()`](https://causalinference.github.io/SEQTaRget/reference/covariates.md)
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
- Fix excusing override (#115)
- Added visit option (#116)
