# Estimate the (very rough) time to run SEQuential analysis on current machine

Estimate the (very rough) time to run SEQuential analysis on current
machine

## Usage

``` r
SEQestimate(
  data,
  id.col,
  time.col,
  eligible.col,
  treatment.col,
  outcome.col,
  time_varying.cols = list(),
  fixed.cols = list(),
  method,
  options,
  verbose = TRUE
)
```

## Arguments

- data:

  data.frame or data.table, if not already expanded with
  [`SEQexpand()`](https://causalinference.github.io/SEQTaRget/dev/reference/SEQexpand.md),
  will preform expansion according to arguments passed to either
  `params` or `...`

- id.col:

  String: column name of the id column

- time.col:

  String: column name of the time column

- eligible.col:

  String: column name of the eligibility column

- treatment.col:

  String: column name of the treatment column

- outcome.col:

  String: column name of the outcome column

- time_varying.cols:

  List: column names for time varying columns

- fixed.cols:

  List: column names for fixed columns

- method:

  String: method of analysis to preform

- options:

  List: optional list of parameters from
  [`SEQopts()`](https://causalinference.github.io/SEQTaRget/dev/reference/SEQopts.md)

- verbose:

  Logical: if `TRUE`, cats progress to console, default is `TRUE`

## Value

A list of (very rough) estimates for the time required for SEQuential
containing:

- `modelTime` estimated time used when running models

- `expansionTime` estimated time used when expanding data

- `totalTime` sum of model and expansion time
