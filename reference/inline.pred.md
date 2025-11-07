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
  target = NULL
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
