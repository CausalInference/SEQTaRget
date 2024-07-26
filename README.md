# SEQuential
<!-- badges: start -->
![CRAN Version](https://www.r-pkg.org/badges/version/SEQuential)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/SEQuential)
[![R-CMD-check](https://github.com/CausalInference/SEQuential-private/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CausalInference/SEQuential-private/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->



<img align="right" src="SEQuential.png" style="float" width="200">
A package to estimate the observational analogs of the intention-to-treat and per-protocol effects of hypothetical treatment strategies. Built from the [INITIATORS SAS macro](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3613145/) built by Roger Logan, Goodarz Danaei, and Miguel Hernan, this tool is designed to analyze observational longitudinal data to estimate the effect of interventions sustained over time. The premise is to emulate the design and analysis of a hypothetical randomized trial. This software is capable of conducting the obervational analogs of intention-to-treat, per-protocol, and as-treated analyses. All analyses are conducted using pooled logistic regression to approximate the hazard ratio from a proportional hazard Cox model.

## Setting up your Analysis

The function, `SEQuential`, requires the user to supply `data`, and column names for `id`, `eligible`, `time`, and `outcome`, along with the `method` of analysis. For changing default parameter values, you can either pass a list to `params` or arguments to `...`. `SEQuential` has a large parameter space; to break up documentation into more digestible chunks, we have broken the parameters into helper functions. Information on all the parameters and defaults is available through `?SEQopts`. When changing parameters from their defaults, building a parameter list to pass to `params` is functionally the same as passing the arguments to `...`. In the case of conflict of arguments supplied, the function will return an error.

```r         
# Using the parameter builder
myparams <- SEQopts(max = 23, nboot = 20)
SEQuential(data, "ID", "eligible", "time", "tx_start", method = "ITT", params = myparams)

# Is functionally the same as
expanded_data <- SEQexpand(data)
SEQuential(expanded_data, "ID", "eligible", "time", "tx_start", method = "ITT", max = 23, nboot = 20)
```

## Output
At present moment, `SEQuential` returns a list containing:
1. bootparams: A list of Bootstrap parameters (`NA` if `bootstrap = FALSE`):
    1. nboot = Number of bootstrap samples
    2. seed = Starting seed for bootstrapping
    3. sample = Percentage of data to sample from when bootstrapping
    4. return = return format for bootstrapped samples
2. model: A list containing the output models, or list of list of output models if `boostrap = TRUE` and `nboot > 1`
    1. result: The outcome model summary (when bootstrapping, dependent on `return`)
    2. weighted_stats: A list containing information about the weights placed on the outcome model
        1. weighted_data: A list containing information about the weights of the data
        2. coef.n0: coeficients of covariates used in the numerator-0 model
        3. coef.n1: coeficients of covariates used in the numerator-1 model
        4. coef.d0: coeficients of covariates used in the denominator-0 model
        5. coef.d1: coeficients of covariates used in the denominator-1 model
        6. min: minimum weight
        7. max: maximum weight
        8. sd: weight standard deviation
        9. p01, p25, p50, p75, p99: percentiles of weights, named accordingly
3. survival_curve: The Kaplan-Meier survival curve
4. survival_data: The survival data drawn from the Kaplan-Meier survival curve
5. risk_difference: The estimated risk difference
6. risk_ratio: The estimated risk ratio
7. elapsed_time: The amount of time taken to run the function in minutes

## Using SEQuential
Sequential is in it's early development stage, while it is stable enough to simply call:
```r
myModel <- SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome",
                    method = "censoring", fixed.cols = "sex", 
                    time.cols = c("N", "L", "P"), weighted = TRUE, 
                    pre.expansion = TRUE, excused = TRUE, 
                    excused.col0 = "excusedZero", excused.col1 = "excusedOne")
```
It is not immediately stable enough to move around many of the options, especially bootstrapping, where much of the return functionality will need to be tweeked.

## Debug Mode
Debug 'mode' is not so much of a mode as it is directly interacting with many of the function internals. It's not good, nor pretty, and I apologize. I believe the problem with Excused is either 
1. In the function `internal_weights` defined in `internal_models.R`- this function generates the weights on the pre-expanded data (`WT$weighted_data`) as defined by
```r
    kept <- c("denominator", time.col, id.col)
    denominator0 <- speedglm::speedglm(paste0(treatment.col, "~", opts$denominator),
                                       data = weight[tx_lag == 0 & get(opts$excused.col0) ==0, ],
                                       family = binomial("logit"))

    denominator1 <- speedglm::speedglm(paste0(treatment.col, "~", opts$denominator),
                                       data = weight[tx_lag == 1 & get(opts$excused.col1) == 0, ],
                                       family = binomial("logit"))

    out <- weight[tx_lag == 0, denominator := predict(denominator0, newdata = .SD, type = "response")
                  ][tx_lag == 0 & get(treatment.col) == 0, denominator := 1 - denominator
                    ][tx_lag == 1, denominator := predict(denominator1, newdata = .SD, type = "response")
                      ][tx_lag == 1 & get(treatment.col) == 0, denominator := 1 - denominator
                        ][get(opts$excused.col0) == 1 | get(opts$excused.col1) == 1, denominator := 1
                          ][, ..kept]
    setnames(out, time.col, "period")
```

Going through the definition of `out`-
1. Where treatment lag = 0, predict denominator from d0 model
2. Where treatment lag = 0 AND treatment = 1, 1 - prediction (step 1)
3. Where treatment lag = 1, predict denominator from d1 model
4. Where treatment lag = 1 AND treatment = 1, 1 - prediction (step 3)
5. Where either excused cols are = 1, denominator = 1
    1. This is changed in the Excused-Fix branch (discussed at the bottom of this README), but wanted to keep things where we had prior
6. Limit to necessary cols

Or the other problem could be in `internal_analysis.R` within `handler` in `lines15-22`
```r
    time.col <- "period"
    WDT <- DT[WT$weighted_data, on = c(id.col, time.col), nomatch = NULL
            ][get(time.col) == 0 & trial == 0, `:=` (numerator = 1,
                                                    denominator = 1)
              ][is.na(numerator), numerator := 1
                ][, `:=` (cprod.Numerator = cumprod(numerator),
                          cprod.Denominator = cumprod(denominator)), by = c(id.col, "trial")
                  ][, weight := cprod.Numerator/cprod.Denominator]
```
Going through the assignment of WDT (Weight Data Table)
1. Left-join WT on id and period
2. Where period AND trial = 0, numerator and denominator are 1
3. Where numerator is NA, assign 1 (only occurs in excused case, where no numerator is ported)
4. Take the cumulative product of the numerator and denominators by id and trial
5. Assign weight as cumprod-numerator / cumprod-denominator

### Setup
1. In `datafab.R` You will need the packages loaded in `line1`, and data pull-in from `line62`. You will also need to intake `opts` and necessary environment variables from `Lines72-73`.\
2. In `internal_misc.R` You will need the functions `create.default.covariates` and `create.default.weight.covariates`. Functionally speaking, using `_bas` or `_sq` behind a variable name (within your dataframe, or created from the function) will force `SEQexpand` to create either the baseline or squared variables respectively. For additional debugging, you can override these default creations by specifying anything other than `NA` to `opts$covariates`, `opts$denominator`, or `opts$numerator`. E.g. the default creation might be `N+L+P`, but if you wanted these baseline variables, you could force `N_bas+L_bas+P_bas` to force a transformation to their baseline counterparts. This has not been tested with excused, since we decided at some point not to allow the user this level of freedom, so please take with a grain of salt.\
3. With these now loaded into your environment, you can now step through `SEQuential` - the only parts that matter are running `line38-42` to enact default weight creation and `line47` to enforce expansion.
4. You can now step through `internal_analysis.R`! - I would stick to `handler` defined within internal analysis, as this is the 'driver' for the bootstraps later - stepping through handler is effectively the same as `nboot=1` or `bootstrap=FALSE`.

## Branch Excused-Fix
Everything in Excused fix is the same, with exception to the last portion of weight creation:
Recall from main branch:
```r
[get(opts$excused.col0) == 1 | get(opts$excused.col1) == 1, denominator := 1
                          ]
```

This is changed to 
```r
[, denominator := ifelse((get(treatment.col) != get(treatment.col)[1] &
                        ((get(treatment.col)[1] == 1 & get(opts$excused.col1) == 1) |
                        (get(treatment.col)[1] == 0 & get(opts$excused.col0) == 1))),
                        1, denominator), by = id.col]
```
This changes the denominator 'override' from: 'where either excused column = 1, override' to more complicated logic:\
(BY ID)
> IF treatment at current cell is NOT the same as the initial treatment AND
> ({IF initial treatment IS 1 AND excused1 = 1} OR {IF initial treatment is 0 AND excused0 = 1})
> THEN override the denominator to 1
> ELSE keep denominator
