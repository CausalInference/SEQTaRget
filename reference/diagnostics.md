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
  one event row.

- `followup.unique` / `followup.nonunique`: distinct subjects
  contributing follow-up vs. the total number of person-time intervals
  (expanded rows). The non-unique count is much larger because each
  subject contributes one row per follow-up period; it is the
  denominator that, with `outcome.nonunique`, gives the per-arm event
  rate.
