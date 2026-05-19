# Introduction to SEQuential

## Setting up your Analysis

There are some assumptions which must be met to avoid unintended errors
when using SEQuential. These are:

1.  User provided `time.col` begins at 0 per unique `id.col` entries, we
    also assume that the column contains only integers and continues by
    1 for every time step. e.g. (0, 1, 2, 3, …) is allowed and (0, 1, 2,
    2.5, …) or (0, 1, 2, 4, 5, …) are not.
2.  Provided `time.col` entries may be out of order as a sort is
    enforced at the beginning of the function, e.g. (0, 2, 1, 4, 3, …)
    is valid because it begins at 0 and is continuously increasing by
    increments of 1, even though it is not ordered.
3.  `eligible` and column names provided to `excused.cols` are binary
    (0/1) flag variables (with respect to `time.col`)

### Step 1 - Defining your options

In your R script, you will always start by defining your options object,
through the
[`SEQopts()`](https://causalinference.github.io/SEQTaRget/reference/SEQopts.md)
helper. There are many defaults which allow you to target exactly how
you would like to change your analysis. Through this wiki there are
specific pages dedicated to each causal contrast and the parameters
which affect them, but for simplicity let’s start with an
intention-to-treat analysis with 20 bootstrap samples.

``` r

library(SEQTaRget)

options <- SEQopts(km.curves = TRUE, #asks the function to return survival and risk estimates
                   bootstrap = TRUE, #asks the model to perform bootstrapping
                   bootstrap.nboot = 20) #asks the model for 20 bootstrap samples
```

In general, options will be in the form `{option}.{parameter}` - here
you may notice that we use `bootstrap.nboot` indicating that this
parameter affects the `bootstrap`

### Step 2 - Running the Primary Function

The next step is running the primary R function,
[`SEQuential()`](https://causalinference.github.io/SEQTaRget/reference/SEQuential.md).
Here you will give your options, data, and data-level information. We
provide some small simulated datasets to test on.

``` r

data <- SEQdata
model <- SEQuential(data, id.col = "ID", 
                          time.col = "time", 
                          eligible.col = "eligible",
                          treatment.col = "tx_init",
                          outcome.col = "outcome",
                          time_varying.cols = c("N", "L", "P"),
                          fixed.cols = "sex",
                          method = "ITT", options = options)
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
#> Bootstrapping with 80% of 300 subjects (240 subjects, ~198,788 observations per resample) 20 times
#> 
#> ITT model created successfully
#> 
#> Creating Survival curves
#> 
#> Completed
```

We provide some print statements to help track where the
[`SEQuential()`](https://causalinference.github.io/SEQTaRget/reference/SEQuential.md)
function is processing at any given point in time.

### Step 3 - Recovering your results

[`SEQuential()`](https://causalinference.github.io/SEQTaRget/reference/SEQuential.md)
produces a lot of internal diagnostics, models, and dataframes out of
its main function in an S4 class. We provide a few different methods to
handle obtaining your results.

``` r

outcome(model)     # Returns a list of only the outcome models 
#> $`1`
#> $`1`[[1]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -6.8593155              0.2253094              0.0353817  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001599              0.0447179              0.0005762  
#>                  sex1                  N_bas                  L_bas  
#>             0.1270458              0.0032867             -0.0138509  
#>                 P_bas  tx_init_bas1:followup  
#>             0.2009289             -0.0017040  
#> 
#> $`1`[[2]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -5.7066694              0.1333013              0.0378736  
#>           followup_sq                  trial               trial_sq  
#>            -0.0003567              0.0264401              0.0008604  
#>                  sex1                  N_bas                  L_bas  
#>             0.3600135              0.0014529             -0.1905843  
#>                 P_bas  tx_init_bas1:followup  
#>             0.1129515             -0.0026792  
#> 
#> $`1`[[3]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -9.171e+00              2.283e-01              5.484e-02  
#>           followup_sq                  trial               trial_sq  
#>            -4.069e-04              1.057e-01             -4.088e-05  
#>                  sex1                  N_bas                  L_bas  
#>            -4.201e-02              2.069e-03             -5.236e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             4.159e-01             -4.541e-03  
#> 
#> $`1`[[4]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.272e+01              1.479e-01              2.798e-02  
#>           followup_sq                  trial               trial_sq  
#>            -5.011e-05              1.588e-01             -1.250e-04  
#>                  sex1                  N_bas                  L_bas  
#>             1.773e-01              1.929e-03             -8.508e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             8.379e-01              5.302e-03  
#> 
#> $`1`[[5]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -4.1261880              0.3143350              0.0443780  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001616             -0.0026676              0.0007760  
#>                  sex1                  N_bas                  L_bas  
#>             0.1813576              0.0024025              0.0650653  
#>                 P_bas  tx_init_bas1:followup  
#>            -0.0958337             -0.0089258  
#> 
#> $`1`[[6]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -4.610e+00             -2.562e-02              3.134e-02  
#>           followup_sq                  trial               trial_sq  
#>            -6.088e-05              1.870e-04              9.596e-04  
#>                  sex1                  N_bas                  L_bas  
#>             1.985e-01              3.879e-03             -5.589e-04  
#>                 P_bas  tx_init_bas1:followup  
#>            -5.636e-02              8.048e-03  
#> 
#> $`1`[[7]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.465e+01             -7.521e-03              4.968e-02  
#>           followup_sq                  trial               trial_sq  
#>            -2.538e-04              2.094e-01             -6.022e-04  
#>                  sex1                  N_bas                  L_bas  
#>            -3.234e-02              6.306e-03             -8.748e-03  
#>                 P_bas  tx_init_bas1:followup  
#>             9.858e-01             -6.308e-04  
#> 
#> $`1`[[8]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -9.3907160              0.2022729              0.0273171  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001233              0.0993802              0.0001453  
#>                  sex1                  N_bas                  L_bas  
#>            -0.1218886             -0.0002785             -0.0469558  
#>                 P_bas  tx_init_bas1:followup  
#>             0.5198547             -0.0010301  
#> 
#> $`1`[[9]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>             2.584e+00              1.053e-01              3.241e-02  
#>           followup_sq                  trial               trial_sq  
#>            -2.687e-05             -1.237e-01              1.648e-03  
#>                  sex1                  N_bas                  L_bas  
#>            -1.376e-01             -1.833e-04             -4.806e-03  
#>                 P_bas  tx_init_bas1:followup  
#>            -7.772e-01              1.554e-04  
#> 
#> $`1`[[10]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -7.4390414              0.4148385              0.0315778  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001275              0.0530073              0.0003986  
#>                  sex1                  N_bas                  L_bas  
#>             0.3099405              0.0025396             -0.0324756  
#>                 P_bas  tx_init_bas1:followup  
#>             0.2463787             -0.0039027  
#> 
#> $`1`[[11]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.373e+01              4.321e-01              4.998e-02  
#>           followup_sq                  trial               trial_sq  
#>            -4.029e-04              1.734e-01             -2.296e-04  
#>                  sex1                  N_bas                  L_bas  
#>             2.815e-01              1.852e-03             -6.080e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             9.013e-01             -5.501e-03  
#> 
#> $`1`[[12]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -6.2981049              0.0917023              0.0310665  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001029              0.0333808              0.0007679  
#>                  sex1                  N_bas                  L_bas  
#>             0.2385835              0.0052174             -0.0882575  
#>                 P_bas  tx_init_bas1:followup  
#>             0.1292543              0.0046821  
#> 
#> $`1`[[13]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -7.748e+00              2.743e-01              3.375e-02  
#>           followup_sq                  trial               trial_sq  
#>            -5.512e-05              5.890e-02              4.715e-04  
#>                  sex1                  N_bas                  L_bas  
#>             2.132e-02              5.390e-03              2.154e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             2.843e-01              9.728e-04  
#> 
#> $`1`[[14]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.052e+01              2.413e-01              3.371e-02  
#>           followup_sq                  trial               trial_sq  
#>            -2.304e-04              1.176e-01             -3.245e-05  
#>                  sex1                  N_bas                  L_bas  
#>             1.947e-02              4.864e-03             -1.934e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             5.975e-01             -1.383e-03  
#> 
#> $`1`[[15]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -8.041e+00              1.576e-01              2.916e-02  
#>           followup_sq                  trial               trial_sq  
#>            -3.588e-05              5.609e-02              6.009e-04  
#>                  sex1                  N_bas                  L_bas  
#>             2.908e-01              4.536e-03              4.007e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             3.231e-01              5.960e-03  
#> 
#> $`1`[[16]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.016e+01              3.886e-02              1.417e-02  
#>           followup_sq                  trial               trial_sq  
#>             3.882e-05              1.093e-01              1.063e-04  
#>                  sex1                  N_bas                  L_bas  
#>             2.804e-01              8.414e-03             -1.058e-01  
#>                 P_bas  tx_init_bas1:followup  
#>             5.972e-01              1.598e-03  
#> 
#> $`1`[[17]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -9.726e+00              2.467e-01              3.850e-02  
#>           followup_sq                  trial               trial_sq  
#>            -2.444e-04              1.017e-01              4.584e-05  
#>                  sex1                  N_bas                  L_bas  
#>            -2.015e-01              1.906e-03              4.453e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             5.117e-01             -7.608e-03  
#> 
#> $`1`[[18]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -3.7054991              0.4101070              0.0420871  
#>           followup_sq                  trial               trial_sq  
#>            -0.0004309             -0.0193794              0.0008780  
#>                  sex1                  N_bas                  L_bas  
#>             0.1318014              0.0094076              0.0608878  
#>                 P_bas  tx_init_bas1:followup  
#>            -0.1264856             -0.0086920  
#> 
#> $`1`[[19]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -3.6385704              0.0836745              0.0312981  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001682             -0.0213489              0.0011680  
#>                  sex1                  N_bas                  L_bas  
#>             0.1934608              0.0021443              0.0115250  
#>                 P_bas  tx_init_bas1:followup  
#>            -0.1165771              0.0027137  
#> 
#> $`1`[[20]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -4.9502880              0.4197268              0.0341601  
#>           followup_sq                  trial               trial_sq  
#>            -0.0002781              0.0094859              0.0008115  
#>                  sex1                  N_bas                  L_bas  
#>            -0.0009308              0.0049774             -0.1895223  
#>                 P_bas  tx_init_bas1:followup  
#>             0.0357164             -0.0110042  
#> 
#> $`1`[[21]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -3.8516231              0.0899331              0.0326567  
#>           followup_sq                  trial               trial_sq  
#>            -0.0002281             -0.0144394              0.0008749  
#>                  sex1                  N_bas                  L_bas  
#>             0.4701161              0.0052241              0.1225678  
#>                 P_bas  tx_init_bas1:followup  
#>            -0.1168174             -0.0011088
km_curve(model)    # Prints the survival curve
```

![Survival curve by treatment
group.](SEQuential_files/figure-html/outcome-1.png)

``` r

risk_data(model)
#>    Method      A      Risk   95% LCI   95% UCI         SE
#>    <char> <char>     <num>     <num>     <num>      <num>
#> 1:    ITT      0 0.8372582 0.7738757 0.9006407 0.03233859
#> 2:    ITT      1 0.8744359 0.8135710 0.9353007 0.03105406
risk_comparison(model)
#>       A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI Risk Differerence  RD 95% LCI
#>    <fctr> <fctr>      <num>      <num>      <num>             <num>       <num>
#> 1: risk_0 risk_1  1.0444041  0.9794216   1.113698        0.03717768 -0.01764957
#> 2: risk_1 risk_0  0.9574838  0.8979094   1.021011       -0.03717768 -0.09200493
#>    RD 95% UCI
#>         <num>
#> 1: 0.09200493
#> 2: 0.01764957
```
