# Bake fixed knots into any `ns(followup, df = N)` term in `params@covariates`

[`splines::ns()`](https://rdrr.io/r/splines/ns.html) recomputes knots
from whatever data it sees, so without fixed knots the basis at
prediction time differs from the basis used at fit time. This helper
rewrites every `ns(followup, df = N)` token in `params@covariates` to an
explicit `ns(followup, knots = c(...), Boundary.knots = c(...))`
computed once from the full expanded `followup` column. The result is a
formula whose `model.matrix` output is invariant to the row subset
passed in.

## Usage

``` r
bake_followup_spline(params)
```

## Details

Returns the original covariates string unchanged when no
`ns(followup, df = ...)` token is present (e.g. user supplied custom
covariates that already specify knots).
