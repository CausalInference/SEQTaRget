# SEQuential trial emulation

`SEQuential` is an all-in-one API to SEQuential analysis, returning a
SEQoutput object of results. More specific examples can be found on
pages at <https://causalinference.github.io/SEQTaRget/>

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

  data.frame or data.table, will perform expansion according to
  arguments passed through the `options` argument

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

  String: method of analysis to perform; should be one of `"ITT"`,
  `"dose-response"`, or `"censoring"`

- options:

  List: optional list of parameters from
  [`SEQopts()`](https://causalinference.github.io/SEQTaRget/reference/SEQopts.md)

- verbose:

  Logical: if TRUE, cats progress to console, default is `TRUE`

## Value

An S4 object of class SEQoutput. If
`options = SEQopts(expand.only = TRUE)`, returns the expanded
`data.table` directly, with analysis steps skipped.

## Details

Implementation of sequential trial emulation for the analysis of
observational databases. The SEQuential software accommodates
time-varying treatments and confounders, as well as binary and failure
time outcomes. `SEQuential` allows to compare both static and dynamic
strategies, can be used to estimate observational analogs of
intention-to-treat and per-protocol effects, and can adjust for
potential selection bias induced by losses-to-follow-up.

## Examples

``` r
# \donttest{
data <- SEQdata

# Intention-to-treat (ITT) effect: subjects are assigned to the treatment
# arm defined by their baseline treatment and followed regardless of any later
# treatment changes, so no weighting is required.
SEQuential(data, id.col = "ID",
           time.col = "time",
           eligible.col = "eligible",
           treatment.col = "tx_init",
           outcome.col = "outcome",
           time_varying.cols = c("N", "L", "P"),
           fixed.cols = "sex",
           method = "ITT",
           options = SEQopts())
#> 
#> Full dataset: 12,180 observations, 11 variables
#> 
#> Non-required columns provided, pruning for efficiency
#> 
#> Pruned
#> 
#> Original dataset (eligible subjects): 9,203 observations, 9 variables
#> 
#> Expanding Data...
#> 
#> Pre-filter expansion: 310,080 observations
#> 
#> Expanded dataset: 248,485 observations, 13 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 248,485 observations, 13 variables
#> 
#> Moving forward with ITT analysis
#> 
#> ITT model created successfully
#> 
#> Completed
#> SEQuential process completed in 1.6 seconds :
#> Initialized with:
#> Outcome covariates: outcome~tx_init_bas+followup+followup_sq+trial+trial_sq+sex+N_bas+L_bas+P_bas 
#> Numerator covariates: NA 
#> Denominator covariates: NA 
#> 
#> Full Model Information ========================================== 
#> 
#> Outcome Model ==================================================== 
#> Coefficients and Weighting:
#>                  Estimate   Std. Error
#> (Intercept)  -6.828506036 5.668367e-01
#> tx_init_bas1  0.189350031 3.005120e-02
#> followup      0.033715157 3.276751e-03
#> followup_sq  -0.000146912 6.466141e-05
#> trial         0.044566166 1.124702e-02
#> trial_sq      0.000578777 1.001866e-04
#> sex1          0.127172410 2.595630e-02
#> N_bas         0.003290667 2.589182e-03
#> L_bas        -0.013392420 1.428081e-02
#> P_bas         0.200724099 5.956440e-02
#> 
#> Diagnostic Tables ================================================== 
#> Unique Outcome Table (distinct subjects who had the outcome): 
#> 
#> |tx_init_bas | outcome|   n|
#> |:-----------|-------:|---:|
#> |0           |       1| 203|
#> |1           |       1| 185|
#> 
#> Non-Unique Outcome Table (total outcome events): 
#> 
#> |tx_init_bas | outcome|    n|
#> |:-----------|-------:|----:|
#> |0           |       1| 1928|
#> |1           |       1| 4432|
#> 
#> Unique Follow-up Table (distinct subjects contributing follow-up): 
#> 
#> |tx_init_bas |   n|
#> |:-----------|---:|
#> |0           | 300|
#> |1           | 273|
#> 
#> Non-Unique Follow-up Table (person-time intervals): 
#> 
#> |tx_init_bas |      n|
#> |:-----------|------:|
#> |0           |  93151|
#> |1           | 155334|

# Per-protocol effect via artificial censoring: subjects are censored when they
# deviate from their assigned strategy, and inverse-probability-of-censoring
# weights adjust for the resulting selection bias. The denominator models the
# probability of remaining uncensored given the time-varying confounders, while
# the numerator uses only the baseline covariates to stabilize the weights (so
# the two formulas must differ - identical formulas give weights of 1).
SEQuential(data, id.col = "ID",
           time.col = "time",
           eligible.col = "eligible",
           treatment.col = "tx_init",
           outcome.col = "outcome",
           time_varying.cols = c("N", "L", "P"),
           fixed.cols = "sex",
           method = "censoring",
           options = SEQopts(weighted = TRUE,
                             numerator = "sex",
                             denominator = "N + L + P + sex"))
#> 
#> Full dataset: 12,180 observations, 11 variables
#> 
#> Non-required columns provided, pruning for efficiency
#> 
#> Pruned
#> 
#> Original dataset (eligible subjects): 9,203 observations, 9 variables
#> 
#> Expanding Data...
#> 
#> Pre-filter expansion: 310,080 observations
#> 
#> Expanded dataset (pre-censoring): 248,485 observations, 15 variables
#> 
#> Expanded dataset (post-censoring): 102,749 observations, 15 variables
#>   entering outcome model (uncensored): 96,251
#>   artificially censored (treatment switch): 6,498
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 102,749 observations, 15 variables
#> 
#> Moving forward with censoring analysis
#> 
#> censoring model created successfully
#> 
#> Completed
#> SEQuential process completed in 1.28 seconds :
#> Initialized with:
#> Outcome covariates: outcome~tx_init_bas+followup+followup_sq+trial+trial_sq+sex 
#> Numerator covariates: tx_init~sex 
#> Denominator covariates: tx_init~N+L+P+sex 
#> 
#> Full Model Information ========================================== 
#> 
#> Outcome Model ==================================================== 
#> Coefficients and Weighting:
#>                   Estimate   Std. Error
#> (Intercept)  -4.9134317258 0.0925102561
#> tx_init_bas1  0.5051368699 0.0741355065
#> followup      0.0281212245 0.0058811408
#> followup_sq   0.0001084173 0.0001476455
#> trial        -0.0132858466 0.0057384370
#> trial_sq      0.0011377569 0.0001049305
#> sex1          0.0660151461 0.0422912172
#> 
#> Weight Information ============================================= 
#> Treatment Lag = 0 Treatment = 0 Model ====================================
#> Numerator ========================== 
#>                Estimate Std. Error
#> (Intercept) -1.63699131 0.06159802
#> sex1         0.03738753 0.08644011
#> Denominator ========================== 
#>                 Estimate  Std. Error
#> (Intercept) -1.332290880 0.247265065
#> N            0.009930812 0.008667517
#> L           -0.011516315 0.038851934
#> P           -0.056782905 0.026287074
#> sex1         0.033299544 0.087312037
#> Treatment Lag = 1 Treatment = 1 Model ====================================
#> Numerator ========================== 
#>               Estimate Std. Error
#> (Intercept)  3.0276301 0.07259087
#> sex1        -0.1467371 0.10125464
#> Denominator ========================== 
#>                 Estimate Std. Error
#> (Intercept)  2.654431030 0.29534161
#> N            0.001573135 0.01033761
#> L            0.050900387 0.03934269
#> P            0.046316350 0.03824053
#> sex1        -0.136773596 0.10169194
#> Weights:
#> Min:  0.7359596 
#> Max:  1.823553 
#> StDev:  0.06287385 
#> P01:  0.8731989 
#> P25:  0.984536 
#> P50:  0.9995707 
#> P75:  1.021913 
#> P99:  1.257297 
#> 
#> 
#> Diagnostic Tables ================================================== 
#> Unique Outcome Table (distinct subjects who had the outcome): 
#> 
#> |tx_init_bas | outcome|   n|
#> |:-----------|-------:|---:|
#> |1           |       1| 153|
#> |0           |       1|  50|
#> 
#> Non-Unique Outcome Table (total outcome events): 
#> 
#> |tx_init_bas | outcome|    n|
#> |:-----------|-------:|----:|
#> |1           |       1| 2157|
#> |0           |       1|  222|
#> 
#> Unique Follow-up Table (distinct subjects contributing follow-up): 
#> 
#> |tx_init_bas |   n|
#> |:-----------|---:|
#> |0           | 300|
#> |1           | 273|
#> 
#> Non-Unique Follow-up Table (person-time intervals): 
#> 
#> |tx_init_bas |     n|
#> |:-----------|-----:|
#> |0           | 16260|
#> |1           | 79991|
#> 
#> Unique Switch Table: 
#> 
#> |switch |    n|
#> |:------|----:|
#> |FALSE  | 8399|
#> |TRUE   |  804|
#> 
#> Non-Unique Switch Table: 
#> 
#> |switch |     n|
#> |:------|-----:|
#> |FALSE  | 96251|
#> |TRUE   |  6498|

# End-of-follow-up outcome: rather than a time-to-event, the outcome is read
# once per trial-period at follow-up time 12 and averaged within each baseline
# arm, weighted by the weight at that time. Trial-periods with no measurement
# at exactly 12 fall back to the nearest one within 12 +/- 3; any with none
# in that window are censored out of the average. Use end_of_fup.type =
# "continuous" for a continuous outcome, which is reported as a mean.
eof <- SEQuential(data, id.col = "ID",
                  time.col = "time",
                  eligible.col = "eligible",
                  treatment.col = "tx_init",
                  outcome.col = "outcome",
                  time_varying.cols = c("N", "L", "P"),
                  fixed.cols = "sex",
                  method = "ITT",
                  options = SEQopts(end_of_fup = TRUE,
                                    end_of_fup.time = 12,
                                    end_of_fup.type = "binary",
                                    end_of_fup.window = 3,
                                    bootstrap = TRUE,
                                    bootstrap.nboot = 5))
#> 
#> Full dataset: 12,180 observations, 11 variables
#> 
#> Non-required columns provided, pruning for efficiency
#> 
#> Pruned
#> 
#> Original dataset (eligible subjects): 9,203 observations, 9 variables
#> 
#> Expanding Data...
#> 
#> Pre-filter expansion: 310,080 observations
#> 
#> Expanded dataset: 248,485 observations, 13 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 248,485 observations, 13 variables
#> 
#> Moving forward with ITT analysis
#> 
#> Bootstrapping with 80% of 300 subjects (240 subjects, ~198,788 observations per resample) 5 times
#> 
#> Estimating end-of-follow-up outcome at follow-up time 12 
#> 
#> Completed
end_of_fup(eof)
#> $`1`
#> $`1`$estimates
#>      Type  Time      A Proportion Trial-periods (Eligible)
#>    <char> <num> <fctr>      <num>                    <int>
#> 1: binary    12      0 0.07570353                     2946
#> 2: binary    12      1 0.10247494                     6257
#>    Trial-periods (Analysed) Trial-periods (Censored)
#>                       <int>                    <int>
#> 1:                     2523                      423
#> 2:                     4889                     1368
#>    Trial-periods (No measurement) % Censored Subjects          SE    95% LCI
#>                             <int>      <num>    <int>       <num>      <num>
#> 1:                              0   14.35845      274 0.007991871 0.06003975
#> 2:                              0   21.86351      248 0.005724500 0.09125513
#>       95% UCI
#>         <num>
#> 1: 0.09136731
#> 2: 0.11369476
#> 
#> $`1`$comparison
#> Key: <A_x, A_y>
#>     Time    A_x    A_y  Difference Difference 95% LCI Difference 95% UCI
#>    <num> <fctr> <fctr>       <num>              <num>              <num>
#> 1:    12      0      1  0.02677142        0.001615037        0.051927796
#> 2:    12      1      0 -0.02677142       -0.051927796       -0.001615037
#>    Difference SE     Ratio Ratio 95% LCI Ratio 95% UCI log(Ratio) SE
#>            <num>     <num>         <num>         <num>         <num>
#> 1:    0.01283512 1.3536350     1.0163289     1.8028884     0.1462254
#> 2:    0.01283512 0.7387516     0.5546655     0.9839334     0.1462254
#> 
#> 
# }
```
