# Changelog

## SEQTaRget v1.3.5

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
