# Inverse-probability weights for the trials kept by `selection.random`

`selection.random` keeps a proportion `selection.prob` of the trials
starting on the first level of `treat.level` and all other trials.
Selection depends only on baseline treatment, which the outcome model
conditions on, so the model needs no correction, but standardizing over
the kept trials under-represents the first arm's covariate distribution.
Each kept trial of that arm stands for `1 / selection.prob` trials.

## Usage

``` r
selection_weight(tx, params)
```

## Arguments

- tx:

  baseline treatment of each trial

- params:

  SEQparams object

## Value

numeric weights, or `NULL` when no selection was made
