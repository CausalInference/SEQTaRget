# Model matrix that keeps every row

[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) ignores
`na.action` and drops rows where a term is `NA`, so the model frame is
built explicitly to keep them for
[`check_design()`](https://causalinference.github.io/SEQTaRget/dev/reference/check_design.md).

## Usage

``` r
design_matrix(formula, data)
```
