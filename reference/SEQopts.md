# Parameter Builder for SEQuential Model and Estimates

Parameter Builder for SEQuential Model and Estimates

## Usage

``` r
SEQopts(
  bootstrap = FALSE,
  bootstrap.nboot = 100,
  bootstrap.sample = 0.8,
  bootstrap.CI = 0.95,
  bootstrap.CI_method = "se",
  cense = NA,
  cense.denominator = NA,
  cense.eligible = NA,
  cense.numerator = NA,
  compevent = NA,
  covariates = NA,
  data.return = FALSE,
  denominator = NA,
  deviation = FALSE,
  deviation.col = NA,
  deviation.conditions = c(NA, NA),
  deviation.excused = FALSE,
  deviation.excused_cols = c(NA, NA),
  excused = FALSE,
  excused.cols = c(NA, NA),
  fastglm.method = 2L,
  followup.class = FALSE,
  followup.include = TRUE,
  followup.max = Inf,
  followup.min = -Inf,
  followup.spline = FALSE,
  hazard = FALSE,
  indicator.baseline = "_bas",
  indicator.squared = "_sq",
  km.curves = FALSE,
  multinomial = FALSE,
  ncores = availableCores() - 1,
  nthreads = getDTthreads(),
  numerator = NA,
  parallel = FALSE,
  plot.colors = c("#F8766D", "#00BFC4", "#555555"),
  plot.labels = NA,
  plot.subtitle = NA,
  plot.title = NA,
  plot.type = "survival",
  seed = NULL,
  selection.first_trial = FALSE,
  selection.prob = 0.8,
  selection.random = FALSE,
  subgroup = NA,
  survival.max = Inf,
  treat.level = c(0, 1),
  trial.include = TRUE,
  weight.eligible_cols = c(),
  weight.lower = -Inf,
  weight.lag_condition = TRUE,
  weight.p99 = FALSE,
  weight.preexpansion = TRUE,
  weight.upper = Inf,
  weighted = FALSE
)
```

## Arguments

- bootstrap:

  Logical: defines if SEQuential should run bootstrapping, default is
  FALSE

- bootstrap.nboot:

  Integer: number of bootstraps

- bootstrap.sample:

  Numeric: percentage of data to use when bootstrapping, should in \[0,
  1\], default is 0.8

- bootstrap.CI:

  Numeric: defines the confidence interval after bootstrapping, default
  is 0.95 (95% CI)

- bootstrap.CI_method:

  Character: selects which way to calculate bootstraps confidence
  intervals ("se", "percentile")

- cense:

  String: column name for additional censoring variable, e.g.
  loss-to-follow-up

- cense.denominator:

  String: censoring denominator covariates to the right hand side of a
  formula object

- cense.eligible:

  String: column name for indicator column defining which rows to use
  for censoring model

- cense.numerator:

  String: censoring numerator covariates to the right hand side of a
  formula object

- compevent:

  String: column name for competing event indicator

- covariates:

  String: covariates to the right hand side of a formula object

- data.return:

  Logical: whether to return the expanded dataframe with weighting
  information

- denominator:

  String: denominator covariates to the right hand side of a to formula
  object

- deviation:

  Logical: create switch based on deviation from column `deviation.col`

- deviation.col:

  Character: column name for deviation

- deviation.conditions:

  Character list: RHS evaluations of the same length as `treat.levels`

- deviation.excused:

  Logical: whether deviations should be excused by
  `deviation.excused_cols`

- deviation.excused_cols:

  Character list: excused columns for deviation switches

- excused:

  Logical: in the case of censoring, whether there is an excused
  condition

- excused.cols:

  List: list of column names for treatment switch excuses - should be
  the same length, and ordered the same as `treat.level`

- fastglm.method:

  Integer: decomposition method for fastglm (1-QR, 2-Cholesky, 3-LDLT,
  4-QR.FPIV)

- followup.class:

  Logical: treat followup as a class, e.g. expands every time to it's
  own indicator column

- followup.include:

  Logical: whether or not to include 'followup' and 'followup_squared'
  in the outcome model

- followup.max:

  Numeric: maximum time to expand about, default is Inf (no maximum)

- followup.min:

  Numeric: minimum time to expand aboud, default is -Inf (no minimum)

- followup.spline:

  Logical: treat followup as a cubic spline

- hazard:

  Logical: hazard error calculation instead of survival estimation

- indicator.baseline:

  String: identifier for baseline variables in
  `covariates, numerator, denominator` - intended as an override

- indicator.squared:

  String: identifier for squared variables in
  `covariates, numerator, denominator` - intended as an override

- km.curves:

  Logical: Kaplan-Meier survival curve creation and data return

- multinomial:

  Logical: whether to expect multilevel treatment values

- ncores:

  Integer: number of cores to use in parallel processing, default is one
  less than system max

- nthreads:

  Integer: number of threads to use for data.table processing

- numerator:

  String: numerator covariates to the right hand side of a to formula
  object

- parallel:

  Logical: define if the SEQuential process is run in parallel, default
  is FALSE

- plot.colors:

  Character: Colors for output plot if `km.curves = TRUE`, defaulted to
  ggplot2 defaults

- plot.labels:

  Character: Color labels for output plot if `km.curves = TRUE` in order
  e.g. `c("risk.0", "risk.1")`

- plot.subtitle:

  Character: Subtitle for output plot if `km.curves = TRUE`

- plot.title:

  Character: Title for output plot if `km.curves = TRUE`

- plot.type:

  Character: Type of plot to create if `km.curves = TRUE`, available
  options are 'survival', 'risk', and 'inc' (in the case of censoring)

- seed:

  Integer: starting seed

- selection.first_trial:

  Logical: selects only the first eligible trial in the expanded dataset

- selection.prob:

  Numeric: percent of total IDs to select for `selection.random`, should
  be bound \[0, 1\]

- selection.random:

  Logical: randomly selects IDs with replacement to run analysis

- subgroup:

  Character: Column name to stratify outcome models on

- survival.max:

  Numeric: maximum time for survival curves, default is Inf (no maximum)

- treat.level:

  List: treatment levels to compare

- trial.include:

  Logical: whether or not to include 'trial' and 'trial_squared' in the
  outcome model

- weight.eligible_cols:

  List: list of column names for indicator columns defining which
  weights are eligible for weight models - in order of `treat.level`

- weight.lower:

  Numeric: weights truncated at lower end at this weight

- weight.lag_condition:

  Logical: whether weights should be conditioned on treatment lag value

- weight.p99:

  Logical: forces weight truncation at 1st and 99th percentile weights,
  will override provided `weight.upper` and `weight.lower`

- weight.preexpansion:

  Logical: whether weighting should be done on pre-expanded data

- weight.upper:

  Numeric: weights truncated at upper end at this weight

- weighted:

  Logical: whether or not to preform weighted analysis, default is FALSE

## Value

An object of class 'SEQopts'
