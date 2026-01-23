# Changelog

## SEQTaRget v1.3.4

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
