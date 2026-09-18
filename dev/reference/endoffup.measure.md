# Select the end-of-follow-up outcome measurement for each trial-period

An end-of-follow-up outcome is measured once, at follow-up time
`end_of_fup.time` (`k`), rather than as a time-to-event. For each (id,
trial) this returns the single row the estimate is read from: the
measurement at exactly `k` when one exists, otherwise - if
`end_of_fup.window` is non-zero - the measurement nearest to `k` within
`[k - window, k + window]`, with ties (measurements equally far either
side of `k`) broken toward the later one, so that at least `k` of
follow-up has elapsed. Trial-periods with no measurement anywhere in the
window contribute no row, i.e. they are censored out of the estimate.

## Usage

``` r
endoffup.measure(DT, params)
```

## Arguments

- DT:

  expanded data.table, weighted (carrying a `weight` column) or not

- params:

  SEQparams object

## Value

data.table with one row per contributing (id, trial): the outcome value,
the weight at that time, the baseline treatment arm, and the follow-up
time the measurement was taken at

## Details

Rows carrying a missing outcome are not measurements: under
`method = "censoring"` these are the artificially censored (treatment
switch) rows, so a subject who deviates before `k` is correctly excluded
rather than contributing a carried-forward value.
