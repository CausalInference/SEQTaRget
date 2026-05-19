# Fit a GLM using the package specified in params@glm.package

Fit a GLM using the package specified in params@glm.package

## Usage

``` r
fit_glm(X, y, family, weights = NULL, params, start = NULL)
```

## Arguments

- X:

  model matrix

- y:

  response vector

- family:

  family object (e.g. quasibinomial())

- weights:

  optional prior weights vector

- params:

  SEQparams object
