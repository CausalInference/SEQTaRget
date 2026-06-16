# SEQTaRget (development version)

* Fix "Risk Differerence" typo in `risk.times` output
* Report follow-up per treatment arm in the `@info` slot as `info$followup.unique` and `info$followup.nonunique` (per subgroup, mirroring `info$outcome.unique` / `info$outcome.nonunique`). Both are grouped by baseline treatment over expanded-data rows with an observed (non-`NA`) outcome - the person-time the outcome model is fit on. The non-unique table counts follow-up intervals (so the non-unique outcome counts divided by these give per-arm event rates); the unique table counts the distinct subjects contributing follow-up to each arm. Also shown in the printed diagnostic tables.
* Speed up the hazard ratio calculation by fitting the Cox model with the survival C fitters directly on a prebuilt design matrix instead of `coxph(formula, data)`, avoiding the `model.frame`/`model.matrix` rebuild on every bootstrap iteration: `survival::coxph.fit()` for the non-competing-event model and `survival::agreg.fit()` for the competing-event Fine-Gray (counting-process) model. The hazard ratio and CIs are unchanged.
* Fix the competing-event Fine-Gray hazard fit to use the `finegray()` case weights (`fgwt`), which are required for a valid subdistribution-hazard estimate and were previously omitted. This is a no-op for the current hazard simulation (which has only administrative censoring, so all `fgwt` are 1) but corrects the estimate should the simulated data ever carry random censoring.
* Report competing events per treatment arm in the `@info` slot as `info$compevent.unique` and `info$compevent.nonunique`, mirroring the structure of `info$outcome.unique` / `info$outcome.nonunique`. Both are grouped by baseline treatment; the non-unique table counts all competing event occurrences in the expanded data and the unique table counts distinct subjects who experienced the competing event. Both are `NA` when no `compevent` is specified.
* From a `SEQuential()` fit, populate `weight.statistics` and `outcome.model` when `hazard = TRUE`.
* Warn when the `numerator` and `denominator` weight models are given identical covariates. In that case the stabilized weights all equal 1 (i.e., no weighting), which is usually a typo in the `denominator` argument.
* Improve the `SEQuential()` helpfile by adding a per-protocol example.
* Add behavioural tests that `selection.random = TRUE` retains all treated trial-starts, subsamples control trial-starts to the requested `selection.prob` fraction, and is reproducible under a fixed seed; rename the previous smoke test that did not actually exercise the feature.
* Add behavioural tests for; `weight.lower`/`weight.upper` truncation, `weight.p99` truncation, `followup.include`/`trial.include`, `followup.class`, `weight.lag_condition`, `followup.min`/`followup.max`, and `weight.eligible_cols`.
* Fix `numerator()` and `denominator()` returning `NULL` for every weighted model; they now return the fitted per-arm numerator/denominator weight models.
* Document that weight truncation applies only to the outcome-model fit.
* Pass the formula cache to `inline.pred()` in the weight models
* Fix `SEQOpts()` argument ordering.
* Bump codecov/codecov-action to v7
* Report the censored/uncensored split in verbose expansion output
* Fix `params@data` divergence so expansion uses the checked and repaired data
* Fix off-by-one bootstrap model pairing in `internal.hazard()`
* Fix the serial bootstrap seed in `internal.survival()` so each replicate standardizes over the same resample its outcome model was fit on
* Fix "incorrect number of dimensions" error when a user column shares a name with an internal data.table variable (e.g. an outcome column named `out`), by hoisting row-index computations out of `DT[i]` expressions where columns shadow local variables
* Fix `fastglm.method` validation to accept fastglm's full range 0-5 (0, column-pivoted QR, is fastglm's own default and was previously rejected; 4 is the full-pivoted QR and 5 the Bidiagonal Divide and Conquer SVD; all six have been supported since fastglm 0.0.1) and update the documentation accordingly
* Fix the default `seed` so unseeded runs are genuinely random. Previously the full `.Random.seed` vector was stored and `set.seed()` silently used only its first element - the RNG kind code, a constant - so every unseeded run drew identical bootstrap resamples. The default is now a single random integer drawn when `SEQopts()` is called.
* Fix a spurious "Maximum followup for survival curves" warning when `followup.max` is set while `survival.max` is left at its `Inf` default. The check now runs after the `Inf` defaults are resolved, which also catches the previously missed case of a finite `survival.max` exceeding the data-derived `followup.max`.
* Fix `show()` erroring on a `SEQoutput` object when both `subgroup` and `compevent` are specified (the competing event section passed the model list rather than the subgroup name to `cat()`); empty competing event model entries are now also skipped instead of printing `NULL`.
* Fix silent merging of bootstrap copies for large numeric subject IDs. The arithmetic relabeling (`orig_id * multiplier + copy index`) exceeds the 2^53 exact-integer range of doubles for roughly 8+ digit IDs, where consecutive copy indices round to the same value and distinct copies of a subject collapse together under by-ID grouping (corrupting cumulative-product weights). Relabeling now falls back to string concatenation whenever the arithmetic could overflow, or when IDs are negative or non-integer.
* Fix the hazard bootstrap dropping resampling multiplicity. The hazard bootstrap sampler did not relabel bootstrap copies, so the identical copies of any subject drawn more than once collapsed under the simulation's by-(id, trial) grouping, making each replicate behave like a subsample rather than a bootstrap and understating the hazard ratio CI width. Copies are now relabeled uniquely; point estimates are unchanged.
* Fix a hardcoded `"_sq"` suffix in the pre-expansion weight data so a custom `indicator.squared` no longer fails with "column not found" in the weight models.
* Fix `method = "dose-response"` erroring with a custom `indicator.squared`. The expansion created `dose_sq`/`trial_sq` with hardcoded names (and excluded only the literal `dose_sq` from expansion variables) while the default covariates referenced `paste0("dose", indicator.squared)`, so any non-default indicator failed in `SEQexpand()`. All internally generated squared columns (`dose`, `trial`, on both the expanded data and the survival prediction grid) now follow `indicator.squared`, matching the existing convention for `followup`, `trial` and time.
* Remove dead `trialID` construction from the survival-curve standardization. The per-row `paste0()` label was built on the full standardization population (and again on every bootstrap resample) but never read: predictions there are row-wise with no by-ID grouping, so bootstrap multiplicity is carried by the duplicated rows themselves. Results are unchanged.
* Vectorise the survival-curve CI clamping with `pmax()`/`pmin()` instead of evaluating scalar `max()`/`min()` once per row via `by = .I`. Results are unchanged.
* Narrow the per-iteration copy in `internal.weights()` to the columns the weight models actually use (ids, structure, treatment and its baseline copy, formula covariates, censoring/visit/eligibility indicators, excused flags) instead of copying every column of the expanded (post-expansion weighting) or input (pre-expansion weighting) table on every bootstrap iteration. Results are unchanged.
* Drop unmatched rows at the time-varying covariate join in `SEQexpand()` with `nomatch = NULL` (and remove a no-op `.SDcols` there). Original-data rows with no expansion-grid match - possible under `followup.min > 0` or `selection.random` - were carried as NA-trial rows through the squared-column computation only to be discarded by the subsequent inner join with the baseline table. Results are unchanged.
* Replace `seq.int(1:.N)` with `seq_len(.N)` in the hazard simulation's per-trial follow-up construction, avoiding a double allocation per (id, trial) group; the column is now integer, matching the expansion's convention. Results are unchanged.
* Document that with `glm.package = "parglm"` and `bootstrap = TRUE` only the main fit uses parglm: the bootstrap refits always use fastglm, warm-started from the main fit's coefficients, which is faster per resample than parglm's per-fit thread setup. This was previously a silent switch.
* Clarify unique vs non-unique in diagnostic table labels and docs

# SEQTaRget v1.4.2

* Remove mention of units from time in docs.
* Improve memory usage in the bootstrapping.
* Fix off-by-one labeling in survival output so that `followup = k` correctly represents survival after `k` intervals, adding a row at `followup = survival.max + 1` for the final interval's estimate.
* Fix expansion bug where subjects experiencing the outcome early were incorrectly carried forward with `outcome=0` rows from subsequent periods by truncating each trial at the first event row (thanks, @francescazaccagnino)
* Add `expand.only` option to `SEQopts()`. When `TRUE`, `SEQuential()` returns the expanded `data.table` directly and skips the analysis steps, for users who want to inspect or store the expanded dataset on its own.
* Fix `followup.spline = TRUE` so the basis is genuinely non-linear. Splines are now built into the model formula via `splines::ns()` instead of being applied as a single-column transform of `followup`, and the new `followup.spline.df` option (default `4`) controls the number of basis functions. The treatment-by-followup interaction now uses the same spline basis. Knots are baked from the full expanded `followup` once at fit time so the basis is identical at fit and prediction time across bootstraps and survival grids. Internally, formula column extraction now uses `all.vars()`, so user-supplied covariates may include `ns()`, `bs()`, `I()`, `factor()`, `poly()` etc. without breaking expansion.
* Rename `format.time()` to `format_time()` because it wasn't an S3 method and hence was causing roxygen2 to write incorrect information in its helpfile.
* Add package level helpfile and bump roxygen2 to 8.0.0.
* Add parglm as an alternative GLM fitting backend.
* Add warm starts for bootstrap GLM fits.
* Add dataset size summary to verbose output.
* Fix `selection.random` not being propagated from `SEQopts()` to internal parameters.
* Cap `data.table` to 2 threads during tests and vignette builds, and skip the multisession parallel test on CRAN, to comply with CRAN's 2-core policy for checks.
* Apply the `SEQopts(nthreads = ...)` setting to `data.table` during `SEQuential()`. Previously it was only used by the `parglm` backend and ignored in the default serial `fastglm` path, so `data.table` ran at its global default thread count. The previous global setting is restored when the call finishes.
* Add `risk.times` option to `SEQopts()`. When `km.curves = TRUE`, risk difference and risk ratio (with CIs) are reported at each requested follow-up time, not just at the end of follow-up. Requested times are snapped to the latest available follow-up at or before them, and the final time is always included. The `risk.comparison` and `risk.data` tables gain a `Followup` column.
* Fix `factorize()` to also coerce categorical (character) time-varying covariates - and their baseline (`_bas`) counterparts - to factors with levels fixed from the full data. Previously only fixed and treatment columns were factorized, so a character time-varying covariate could realise different level sets across bootstrap resamples and raise "newdata provided does not match fitted model" (most often in bootstrapped hazard analyses on larger samples or with a smaller `bootstrap.sample`). Numeric time-varying covariates are left unchanged.

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

- The `hazard_ratio()` function now correctly describes the estimate as "Hazard ratio"
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
