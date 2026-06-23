# Build the function that gives each bootstrap copy of a subject a unique ID

Numeric IDs are relabeled arithmetically
(`orig_id * multiplier + copy index`), but only while every relabeled
value stays within the 2^53 exact-integer range of doubles and the IDs
are non-negative integers: beyond 2^53 consecutive copy indices round to
the same double, so distinct bootstrap copies of a subject silently
merge under by-ID grouping (e.g. 10-digit IDs). All other cases relabel
by string concatenation (`orig_id_b<copy index>`).

## Usage

``` r
bootstrap_id_relabeler(UIDs, n_sample)
```

## Arguments

- UIDs:

  vector of unique subject IDs

- n_sample:

  number of subjects drawn per bootstrap resample

## Value

a function(id_col, idx) returning unique relabeled IDs
