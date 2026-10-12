# Stop, naming the terms, when a design matrix has missing or infinite values

Covariates are checked complete on entry, so these come from a term such
as `log(x)` or `sqrt(x)` being undefined for some rows.

## Usage

``` r
check_design(X)
```
