# Check a fitted fastglm model for signs of perfect or quasi-complete separation

Issues a warning when separation is detected. Because `fastglm` uses
IWLS and stops at its iteration limit rather than diverging to `Inf`,
separation is identified by two complementary signals:

1.  Any coefficient with `|coef| > 25` (logit \> 25 implies P \> 1 -
    1e-11, unreachable without separation).

2.  Non-finite coefficients (`Inf`/`-Inf`/`NaN`), which can occur with
    other solvers.

## Usage

``` r
check_separation(model, label = "logistic regression")
```

## Arguments

- model:

  a fastglm model object

- label:

  a short string identifying the model (used in the warning message)
