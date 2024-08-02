# SEQuential
<!-- badges: start -->
![CRAN Version](https://www.r-pkg.org/badges/version/SEQuential)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SEQuential)
[![R-CMD-check](https://github.com/CausalInference/SEQuential-private/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CausalInference/SEQuential-private/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/CausalInference/SEQuential/graph/badge.svg?token=MHEN30AF08)](https://codecov.io/gh/CausalInference/SEQuential)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->



<img align="right" src="SEQuential/SEQuential.png" style="float" width="200">

A package to estimate the observational analogs of the intention-to-treat and per-protocol effects of hypothetical treatment strategies. Built from the [INITIATORS SAS macro](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3613145/) built by Roger Logan, Goodarz Danaei, and Miguel Hernan, this tool is designed to analyze observational longitudinal data to estimate the effect of interventions sustained over time. The premise is to emulate the design and analysis of a hypothetical randomized trial. This software is capable of conducting the observational analogs of intention-to-treat, per-protocol, and as-treated analyses. All analyses are conducted using pooled logistic regression to approximate the hazard ratio from a proportional hazard Cox model.


## Setting up your Analysis
`SEQuential` uses R's S4 object class system to handle function input/output. From the user side, this amounts to calling a helpful constructor `SEQopts` and then feeding that into `SEQuential`
```r
data <- SEQdata
myOptions <- SEQopts(excused = TRUE, nboot = 100, bootstrap = TRUE,
                     excused.col1 = "ExcusedOne", excused.col0 = "ExcusedZero")

model <- SEQuential(data, id.col = "ID", time.col = "time",
                    eligible.col = "eligible", treatment.col = "treatment", 
                    outcome.col = "outcome", 
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "race",
                    method = "censoring",
                    options = myOptions)

#Look at 50th bootstrap iteration
boot50 <- explore(model, 50)
show(boot50)
```
### Assumptions
This package places several assumptions onto the input data and unexpected results and errors may arrise if these are not followed - 
1. User provided `time.col` begins at 0 per unique `id.col` entries, we also assume that the column contains only integers and doest continues by 1 for every time step. e.g. (0, 1, 2, 3, ...) is allowed and (0, 1, 2, 2.5, ...) or (0, 1, 2, 4, 5, ...) are not.
    - Provided `time.col` entries may be out of order as a sort is enforced at the beginning of the function, e.g. (0, 2, 1, 4, 3, ...) is valid because it begins at 0 and is continuously increasing by increments of 1, even though it is not ordered.
2. `eligible`, `excused.col1`, and `excused.col0` are once one only one (with respect to `time.col`) flag variables 

## Return
The primary function, `SEQuential`, returns an S4 object of class `SEQoutput` with slots:
1. bootstrap - TRUE/FALSE dependent if bootstrapping was done
2. boot.sample - sample of data used in each bootstrap
3. boot.slice - used in method `show()`
4. seed - seeding for bootstrap sampling
5. nboot - number of bootstraps
6. outcome - outcome covariates
7. numerator - numerator covariates when weighting
8. denominator - denominator covariates when weighting
9. survival_curve - ggplot survival curve
10. survival_data - survival data
11. risk_difference - risk difference
12. risk_ratio - risk ratio
13. elapsed_time - elapsed time for the SEQuential analysis

These can be handily and easily printed to the terminal with `show(.)`

## Dependencies
- data.table
- doFuture
- doRNG
- future
- future.apply
- ggplot2
- speedglm
- methods

## Contributing to the package
Community members are welcome to contribute to this package through several different avenues- 
- Asking/Answering questions about the package via [Github Discussions](https://github.com/CausalInference/SEQuential/discussions/categories/q-a). These can be questions about analysis methods, future planned developments for the package, or requests for clarity on package internals.
- Contributing to [Github Issues](https://github.com/CausalInference/SEQuential/issues) if a bug is found. We have a guided bug report to help us resolve unintended pests quickly.
- Adding content to the package
    - If you intend to add to the package, we would prefer you to branch and then pull-request. This PR will need to:
        1. Pass current unit-tests to ensure nothing is being broken backwards.
        2. Add tests to added portions of code if they are not already covered in existing tests
        3. Pass R-CMD-Check (initiated on PR) with 0-0-0 status
        4. Be styled using `styler::style_pkg`