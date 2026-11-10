# Rescale bootstrap draws to the full sample size

Each resample draws `bootstrap.sample * n` of the `n` subjects (an
m-out-of-n bootstrap), so the spread of the draws reflects a sample of
size `m`, not `n`: for a root-n consistent estimator their standard
deviation is about `1 / sqrt(bootstrap.sample)` times too large.
Shrinking each draw towards the full-data estimate by
`sqrt(bootstrap.sample)` corrects this for both the standard error and
the percentile interval, and keeps draws of a bounded quantity (a risk)
within its bounds. No-op when `bootstrap.sample = 1`.

## Usage

``` r
rescale_boot(draws, estimate, params)
```

## Arguments

- draws:

  numeric vector of bootstrap estimates

- estimate:

  full-data estimate(s), recycled against `draws`

- params:

  SEQparams object

## Value

numeric vector the same length as `draws`
