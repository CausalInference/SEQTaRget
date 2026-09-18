# Weighted end-of-follow-up average within each treatment arm

Weight truncation (`weight.lower` / `weight.upper`, including the bounds
`weight.p99` resolves to) is applied here as it is for the outcome
model, since this average is the estimator in `end_of_fup` mode.

## Usage

``` r
endoffup.estimate(DT, params)
```

## Arguments

- DT:

  expanded data.table for one bootstrap iteration

- params:

  SEQparams object

## Value

named list of per-arm data.tables, one element per subgroup

## Details

Alongside the estimate this counts the trial-periods censored for want
of a measurement in the window - those measured at some point but not
within `[k - window, k + window]` - so that share can be reported next
to the estimate they were dropped from. Trial-periods never measured at
all are counted separately rather than folded in, so the analysed,
censored and never-measured counts partition the eligible total.
Trial-periods are the unit because one subject can be analysed in one
trial and censored in another.
