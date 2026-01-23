# Helper Function to inline predict a fastglm object

Helper Function to inline predict a fastglm object

## Usage

``` r
inline.pred(
  model,
  newdata,
  params,
  type,
  case = "default",
  multi = FALSE,
  target = NULL,
  cache = NULL
)
```

## Arguments

- model:

  a fastglm object

- newdata:

  filler for a .SD from data.table

- params:

  parameter from SEQuential

- type:

  type of prediction

- case:

  case type: "default", "LTFU", "visit", "surv"

- multi:

  multinomial flag

- target:

  target level for multinomial

- cache:

  optional formula cache from init_formula_cache
