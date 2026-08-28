# Account for every trial-period at the end-of-follow-up time

Classifies each trial-period in the analysis data into exactly one of
four mutually exclusive categories, so that the trial-periods
contributing to the estimate can be reconciled against those excluded:

- measured at `k` - contributes, using the measurement at exactly
  `end_of_fup.time`;

- measured in the window - contributes, having no measurement at `k` but
  one within `[k - window, k + window]`;

- excluded, outside the window - has a measurement somewhere, but none
  within the window;

- excluded, no measurement - has no non-missing outcome at any follow-up
  time. Under `method = "censoring"` this includes trial-periods
  artificially censored before any measurement was taken.

## Usage

``` r
endoffup.counts(DT, params, type)
```

## Arguments

- DT:

  expanded data.table, weighted or not - as passed to
  [`endoffup.estimate()`](https://causalinference.github.io/SEQTaRget/reference/endoffup.estimate.md)

- params:

  SEQparams object

- type:

  either `"nonunique"` (trial-periods) or `"unique"` (distinct
  subjects). Trial-period counts are mutually exclusive and so sum to
  `Eligible`; subject counts need not, since one subject can fall into
  different categories for different trials

## Value

named list of data.tables, one element per subgroup, each with a row per
baseline treatment arm

## Details

Counted over the same data the estimate is computed from, so the first
two categories always sum to the contributing trial-periods reported by
[`end_of_fup()`](https://causalinference.github.io/SEQTaRget/reference/end_of_fup.md).
