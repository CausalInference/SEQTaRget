## Resubmission

This is a second resubmission of SEQTaRget version 1.4.2.

We now skip the longest-running tests (bootstrap, reproducibility and LTFU-weighting tests) on CRAN via `skip_on_cran()`, and stop evaluating the `censoring` vignette's code chunks on CRAN (matching the `ITT` and `dose-response` vignettes), to keep the overall check time within CRAN's limit. These tests and vignette chunks still run in CI and on the pkgdown site.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments from first resubmission

The previous submission's checks on your r-devel-linux-x86_64-debian-gcc machine reported CPU time roughly 11x elapsed time in the tests and in vignette re-building. This was caused by data.table's default multithreading (it uses up to half the available cores).

We have now capped data.table to 2 threads in all tests and in the vignettes, and the single multisession (parallel) test is skipped on CRAN. No check now uses more than 2 cores. The package's default behaviour for users is unchanged.
