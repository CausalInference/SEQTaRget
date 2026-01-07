
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SEQTaRget - Sequentially Nested Target Trial Emulation

<!-- badges: start -->

![CRAN Version](https://www.r-pkg.org/badges/version/SEQTaRget)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SEQTaRget)
[![R-CMD](https://github.com/CausalInference/SEQTaRget/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CausalInference/SEQTaRget/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/CausalInference/SEQTaRget/graph/badge.svg?token=MHEN30AF08)](https://app.codecov.io/gh/CausalInference/SEQTaRget)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

<img src="https://github.com/CausalInference/SEQTaRget/blob/main/SEQ.png" align="right" style="float" width="200"/>

Implementation of sequential trial emulation for the analysis of
observational databases. The ‘SEQTaRget’ software accommodates
time-varying treatments and confounders, as well as binary and failure
time outcomes. ‘SEQTaRget’ allows to compare both static and dynamic
strategies, can be used to estimate observational analogs of
intention-to-treat and per-protocol effects, and can adjust for
potential selection bias <br/> <br/>

## Installation

You can install the development version of SEQ from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("CausalInference/SEQTaRget", subdir = "SEQTaRget")
```

## Setting up your Analysis

`SEQuential` uses R’s S4 object class system to handle function
input/output. From the user side, this amounts to calling a helpful
constructor `SEQopts` and then feeding that into `SEQuential`.
`SEQestimate` can also take the provided options and return a (very
rough) estimated time for analysis.

``` r
library(SEQTaRget)
data <- SEQdata

model <- SEQuential(data, id.col = "ID", time.col = "time",
                    eligible.col = "eligible", treatment.col = "tx_init", 
                    outcome.col = "outcome", 
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "ITT",
                    options = SEQopts(km.curves = TRUE))

outcome(model)                       # Returns a list of all outcome models as S3 fastglm objects over the course of bootstrapping
diagnostics <- diagnostics(model)    # Returns a list of diagnostic tables
diagnostics$outcome.unique[[1]]      # Returns a table of unique outcomes
diagnostics$outcome.nonunique[[1]]   # Returns a table of nonunique outcomes

# Plotting
km_curve(model)[[1]]                 # Returns the Survival curve
km_data(model)[[1]]                  # Returns a dataframe of survival data in long-format for other analysis/plotting
```

### Assumptions

This package places several assumptions onto the input data and
unexpected results and errors may arise if these are not followed-

1.  User provided `time.col` begins at 0 per unique `id.col` entries, we
    also assume that the column contains only integers and continues by
    1 for every time step. e.g. (0, 1, 2, 3, …) is allowed and (0, 1, 2,
    2.5, …) or (0, 1, 2, 4, 5, …) are not.
    - Provided `time.col` entries may be out of order as a sort is
      enforced at the beginning of the function, e.g. (0, 2, 1, 4, 3, …)
      is valid because it begins at 0 and is continuously increasing by
      increments of 1, even though it is not ordered.
2.  `eligible` and column names provided to types of `excused.cols` are
    once one only one (with respect to `time.col`) flag variables

## Return

The primary function, `SEQuential`, returns an S4 object of class
`SEQoutput` with slots:

1.  params - the SEQparams object created through the SEQuential process
2.  DT - Expanded data table with weighting information
3.  outcome - outcome covariates
4.  numerator - numerator covariates when weighting
5.  denominator - denominator covariates when weighting
6.  outcome.model - fastglm model objects
7.  hazard - the hazard ratio
8.  weight.statistics - weighting information
9.  survival.curve - ggplot survival curve
10. survival.data - survival and risk data for all points of followup
11. risk.comparison - data table of comparisons between risks at end of
    followup bootstrapped
12. risk.data - data table of risks for every level of treatment at each
    time point
13. time - elapsed time for the SEQuential analysis
14. info - list of diagnostic tables
15. ce.model - Competing event model objects

These can be handily and easily printed to the terminal with by calling
the object as `model` (if continuing the example above). While this this
the shape of the output object, not all slots will always be filled,
e.g. if a user providers `hazard = TRUE`, then the survival curves,
data, and associated risks will return `NA`.

## Imports

- data.table
- doFuture
- doRNG
- future
- future.apply
- ggplot2
- fastglm
- methods
- stringr
- survival
- parallelly
- knitr

## Finding More Information and Examples

Further information on utilizing this package or developing it further
is available with the [SEQTaRget
Pages](https://causalinference.github.io/SEQTaRget/) as a part of this
repository. If you are unable to find solutions or answers there, please
feel free to open a discussion.

## Contributing to the package

Community members are welcome to contribute to this package through
several different avenues-

- Asking/Answering questions about the package via [GitHub
  Discussions](https://github.com/CausalInference/SEQTaRget/discussions/categories/q-a).
  These can be questions about analysis methods, future planned
  developments for the package, or requests for clarity on package
  internals.
- Contributing to [GitHub
  Issues](https://github.com/CausalInference/SEQTaRget/issues) if a bug
  is found. We have a guided bug report to help us resolve unintended
  pests quickly.
- Adding content to the package
  - If you intend to add to the package, we would prefer you to branch
    and then pull-request. This PR will need to:
    1.  Pass current unit-tests to ensure nothing is being broken
        backwards.
    2.  Add tests to added portions of code if they are not already
        covered in existing tests
    3.  Pass R-CMD-Check (initiated on PR) with 0-0-0 status
