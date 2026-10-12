# Bake fixed knots into every `ns(x, df = N)` term of a formula string

[`splines::ns()`](https://rdrr.io/r/splines/ns.html) recomputes its
knots from whatever data it sees, so without fixed knots the basis built
at prediction time differs from the one used at fit time (the weight and
outcome models are fit on, and predicted over, different row subsets).
This helper rewrites every `ns(x, df = N)` token in `covs` to an
explicit `ns(x, knots = c(...), Boundary.knots = c(...))`, with the
knots computed once from the full `x` column of `data` - the quantiles
`ns()` itself would use. The result is a formula whose `model.matrix`
output is invariant to the row subset passed in, and constant across
bootstrap resamples.

## Usage

``` r
bake_spline_knots(covs, data)
```

## Arguments

- covs:

  character vector of RHS formula strings (may be `NA`)

- data:

  `data.table` holding the columns the model is fit on

## Value

`covs` with every bakeable `ns()` term rewritten

## Details

Tokens are left unchanged when the variable is absent from `data` or is
not numeric, and when the term already carries explicit knots (only the
`df = N` form is rewritten), so user-supplied bases pass through
untouched.
