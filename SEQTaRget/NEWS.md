# SEQTaRget (development version)

* Remove mention of units from time in docs.
* Improve memory usage in the bootstrapping.
* Fix off-by-one labeling in survival output so that `followup = k` correctly represents survival after `k` intervals, adding a row at `followup = survival.max + 1` for the final interval's estimate.

# SEQTaRget v1.4.1

- Strip row-level vectors from fastglm objects to reduce weight.statistics memory usage and use a new internal function to print the coefficient table.
- Strip row-level vectors from outcome models before storing in `@outcome.model`
- Fix clean_fastglm to strip row-level vectors from nested multinomial weight models
- No longer store survival.curve ggplot object; regenerate on demand via `km_curve()`
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
- Move selection.random before expansion to reduce peak memory usage
- Replace `cbind()` with `:=` in expansion chain to avoid intermediate copy
- Replace `merge()` with data.table native join in expansion data_list combine step
- Replace rbind weight construction with copy+in-place to reduce peak memory
- Drop wt and tmp columns immediately after weight is computed in all code paths
- Remove redundant setDF calls in fast_model_matrix
- Free WDT before bootstrap loop when data.return is `FALSE`
- Use `match(TRUE, ...)` instead of `which(...)[1]` to find first switch/event per group
- Replace `sapply` loop with single matrix multiply in multinomial prediction
- Vectorise survival curve predictions into a single inline.pred call per treatment level
- Free result list after extraction in internal_survival.R to reduce peak memory during bootstrap
- Free analytic list after subgroup loop in SEQuential.R to reduce peak memory during survival curve computation
- Avoid `copy()` in data_all construction and free data list in internal_survival.R to reduce peak memory during bootstrap
- Filter to `followup==0` before adding trialID in internal.survival to avoid copying entire expanded dataset
- Trim base_DT to only prediction-needed columns before replication in internal.survival to reduce peak memory
- Remove unnecessary copy(weight) for model.data in internal.weights since it is never modified in-place
- Free baseDT after bootstrap loop in internal.survival to reduce peak memory during survival curve computation
- Fix multinomial.summary: replace vcov() with fastglm $se field and add missing Coefficient column to prevent rbind mismatch
- Add test_coverage.R with tests targeting uncovered code paths to increase coverage
- Remove some no longer used variables and dead code
- Further memory reduction optimizations

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
