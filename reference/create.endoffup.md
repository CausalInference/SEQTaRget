# Assemble end-of-follow-up estimates and bootstrap confidence intervals

Mirrors
[`create.risk()`](https://causalinference.github.io/SEQTaRget/reference/create.risk.md):
`eof.data` holds the per-arm estimate and `eof.comparison` the pairwise
between-arm contrasts, both with bootstrap confidence intervals when
`bootstrap = TRUE`. Contrasts are paired by bootstrap iteration, so the
interval accounts for the correlation between arms.

## Usage

``` r
create.endoffup(full, boots, params)
```

## Arguments

- full:

  per-arm estimates from the full-data fit

- boots:

  list of per-arm estimates, one per bootstrap iteration

- params:

  SEQparams object

## Value

list with `eof.data` and `eof.comparison` data.tables
