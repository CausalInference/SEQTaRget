# Extract the end-of-follow-up outcome estimates

Available when
[`SEQuential()`](https://causalinference.github.io/SEQTaRget/reference/SEQuential.md)
was run with `end_of_fup = TRUE`. The estimate in each arm is the
weighted average of the outcome measured at `end_of_fup.time`, weighted
by the period-trial-specific weight at the time the measurement was
taken.

## Usage

``` r
end_of_fup(object)
```

## Arguments

- object:

  SEQoutput object

## Value

A named list, one element per subgroup, each a list of two data.tables:

- `estimates`: the weighted proportion (binary) or mean (continuous) in
  each baseline treatment arm, with its bootstrap confidence interval,
  the trial-periods eligible in each arm, partitioned into those
  analysed, those censored for want of a measurement in the window -
  measured at some point but not within `[k - window, k + window]` - and
  those never measured at all, with the censoring also as a percentage
  of the eligible total. `Subjects` counts the distinct contributing
  subjects.

- `comparison`: the pairwise between-arm difference - in proportions for
  a binary outcome, in means for a continuous one - with its bootstrap
  standard error and confidence interval, paired by iteration so the
  interval accounts for the correlation between arms. For a binary
  outcome the ratio of proportions is also given, with a confidence
  interval computed on the log scale and a `log(Ratio) SE` for
  inverse-variance pooling. A ratio is not reported for a continuous
  outcome, where the outcome need not be bounded away from zero.
