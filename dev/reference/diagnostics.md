# Function to return diagnostic tables from a SEQuential object

Function to return diagnostic tables from a SEQuential object

## Usage

``` r
diagnostics(object)
```

## Arguments

- object:

  SEQoutput object

## Value

A named list of diagnostic tables, each broken down by baseline
treatment arm. The "unique" and "non-unique" variants count different
things:

- `outcome.unique` / `outcome.nonunique`: distinct subjects who had the
  outcome vs. the total number of outcome events. These coincide for a
  one-time (terminal) outcome, since each subject contributes at most
  one event row. Both are `NA` for a continuous end-of-follow-up
  outcome, which has no events to count.

- `eof.unique` / `eof.nonunique`: present only when `end_of_fup = TRUE`,
  accounting for every trial-period at the end-of-follow-up time across
  four mutually exclusive categories - measured `At k`, measured
  `In window`, `Excluded (outside window)` and
  `Excluded (no measurement)` - against the `Eligible` total. The
  non-unique (trial-period) counts partition `Eligible`, and `At k` plus
  `In window` equals the trial-periods contributing to the estimate. The
  unique (subject) counts need not sum to `Eligible`, since one subject
  can fall into different categories for different trials. For a
  continuous outcome `eof.summary` additionally reports the N, mean and
  SD of the raw analysed measurements per arm, standing in for the
  suppressed outcome count tables.

- `followup.unique` / `followup.nonunique`: distinct subjects
  contributing follow-up vs. the total number of person-time intervals
  (expanded rows). The non-unique count is much larger because each
  subject contributes one row per follow-up period; it is the
  denominator that, with `outcome.nonunique`, gives the per-arm event
  rate.
