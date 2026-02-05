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
