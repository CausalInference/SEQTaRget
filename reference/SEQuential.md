# SEQuential trial emulation

\`SEQuential\` is an all-in-one API to SEQuential analysis, returning a
SEQoutput object of results. More specific examples can be found on
pages at https://causalinference.github.io/SEQTaRget/

## Usage

``` r
SEQuential(
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

  data.frame or data.table, will preform expansion according to
  arguments passed to `options`

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

  List: optional list of parameters from `SEQopts`

- verbose:

  Logical: if TRUE, cats progress to console

## Value

An S4 object of class SEQoutput

## Details

Implemention of sequential trial emulation for the analysis of
observational databases. The SEQuential software accommodates
time-varying treatments and confounders, as well as binary and failure
time outcomes. SEQ allows to compare both static and dynamic strategies,
can be used to estimate observational analogs of intention-to-treat and
per-protocol effects, and can adjust for potential selection bias
induced by losses-to-follow-up.

## Examples

``` r
# \donttest{
data <- SEQdata
model <- SEQuential(data, id.col = "ID", 
                          time.col = "time", 
                          eligible.col = "eligible",
                          treatment.col = "tx_init",
                          outcome.col = "outcome",
                          time_varying.cols = c("N", "L", "P"),
                          fixed.cols = "sex",
                          method = "ITT", 
                          options = SEQopts())
#> Non-required columns provided, pruning for efficiency
#> Pruned
#> Expanding Data...
#> Expansion Successful
#> Moving forward with ITT analysis
#> ITT model created successfully
#> Completed
# }
```
