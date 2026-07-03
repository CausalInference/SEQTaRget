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
[`SEQopts()`](https://causalinference.github.io/SEQTaRget/dev/reference/SEQopts.md)
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
[`SEQuential()`](https://causalinference.github.io/SEQTaRget/dev/reference/SEQuential.md).
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
[`SEQuential()`](https://causalinference.github.io/SEQTaRget/dev/reference/SEQuential.md)
function is processing at any given point in time.

### Step 3 - Recovering your results

[`SEQuential()`](https://causalinference.github.io/SEQTaRget/dev/reference/SEQuential.md)
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
#>            -5.1917949              0.2138155              0.0268031  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001253              0.0094144              0.0007377  
#>                  sex1                  N_bas                  L_bas  
#>             0.3415499              0.0066648              0.0327849  
#>                 P_bas  tx_init_bas1:followup  
#>             0.0282247              0.0005433  
#> 
#> $`1`[[3]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -2.961e+00              2.190e-01              3.144e-02  
#>           followup_sq                  trial               trial_sq  
#>            -4.660e-05             -3.476e-02              1.314e-03  
#>                  sex1                  N_bas                  L_bas  
#>             2.908e-01              3.530e-03             -1.467e-01  
#>                 P_bas  tx_init_bas1:followup  
#>            -2.287e-01              4.059e-05  
#> 
#> $`1`[[4]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -9.075e+00              2.802e-01              4.014e-02  
#>           followup_sq                  trial               trial_sq  
#>            -2.142e-04              8.476e-02              5.371e-04  
#>                  sex1                  N_bas                  L_bas  
#>             5.815e-02              1.189e-05             -1.225e-01  
#>                 P_bas  tx_init_bas1:followup  
#>             4.210e-01             -1.606e-03  
#> 
#> $`1`[[5]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -6.4193939              0.1987765              0.0344510  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001026              0.0295477              0.0007441  
#>                  sex1                  N_bas                  L_bas  
#>             0.3380944             -0.0038997              0.0247087  
#>                 P_bas  tx_init_bas1:followup  
#>             0.1497100             -0.0008964  
#> 
#> $`1`[[6]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -8.5713230              0.4375908              0.0376961  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001699              0.0733460              0.0003134  
#>                  sex1                  N_bas                  L_bas  
#>            -0.1445591              0.0036777              0.0445167  
#>                 P_bas  tx_init_bas1:followup  
#>             0.3784758             -0.0057545  
#> 
#> $`1`[[7]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.9454759              0.4054962              0.0668470  
#>           followup_sq                  trial               trial_sq  
#>            -0.0004881             -0.0575593              0.0013747  
#>                  sex1                  N_bas                  L_bas  
#>             0.0244046              0.0064814              0.1516499  
#>                 P_bas  tx_init_bas1:followup  
#>            -0.3976841             -0.0093021  
#> 
#> $`1`[[8]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -6.5992636              0.5404326              0.0442982  
#>           followup_sq                  trial               trial_sq  
#>            -0.0002736              0.0315852              0.0006899  
#>                  sex1                  N_bas                  L_bas  
#>             0.3406119              0.0034334             -0.0828339  
#>                 P_bas  tx_init_bas1:followup  
#>             0.1389118             -0.0090328  
#> 
#> $`1`[[9]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -3.1412782             -0.0335680              0.0483112  
#>           followup_sq                  trial               trial_sq  
#>            -0.0003257             -0.0097116              0.0008772  
#>                  sex1                  N_bas                  L_bas  
#>             0.0017989              0.0030678              0.0103008  
#>                 P_bas  tx_init_bas1:followup  
#>            -0.2216942              0.0053997  
#> 
#> $`1`[[10]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -3.4239123              0.1856021              0.0329491  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001788             -0.0140938              0.0008658  
#>                  sex1                  N_bas                  L_bas  
#>             0.0129788              0.0002282             -0.0294013  
#>                 P_bas  tx_init_bas1:followup  
#>            -0.1334241             -0.0034121  
#> 
#> $`1`[[11]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -7.4415862              0.2425934              0.0546454  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001933              0.0594890              0.0003971  
#>                  sex1                  N_bas                  L_bas  
#>             0.3481986              0.0008652              0.1021379  
#>                 P_bas  tx_init_bas1:followup  
#>             0.1864135             -0.0013004  
#> 
#> $`1`[[12]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -4.1350666              0.4427235              0.0467969  
#>           followup_sq                  trial               trial_sq  
#>            -0.0003048             -0.0123856              0.0011763  
#>                  sex1                  N_bas                  L_bas  
#>             0.1711764              0.0037520             -0.1486645  
#>                 P_bas  tx_init_bas1:followup  
#>            -0.1049486             -0.0059308  
#> 
#> $`1`[[13]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -6.6131676              0.0709139              0.0357118  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001389              0.0424103              0.0008050  
#>                  sex1                  N_bas                  L_bas  
#>             0.0755352              0.0035227             -0.0699716  
#>                 P_bas  tx_init_bas1:followup  
#>             0.1785358              0.0078700  
#> 
#> $`1`[[14]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -8.497e+00              8.983e-02              2.471e-02  
#>           followup_sq                  trial               trial_sq  
#>            -5.818e-05              7.526e-02              2.603e-04  
#>                  sex1                  N_bas                  L_bas  
#>             1.391e-01              5.211e-03              2.298e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             3.750e-01              1.838e-03  
#> 
#> $`1`[[15]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.035e+01              1.610e-01              2.720e-02  
#>           followup_sq                  trial               trial_sq  
#>            -1.003e-04              1.194e-01             -1.737e-05  
#>                  sex1                  N_bas                  L_bas  
#>             2.605e-03              3.526e-03             -9.449e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             6.084e-01              1.318e-03  
#> 
#> $`1`[[16]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -9.713e+00             -1.622e-01              2.763e-02  
#>           followup_sq                  trial               trial_sq  
#>            -6.866e-05              1.043e-01              2.366e-04  
#>                  sex1                  N_bas                  L_bas  
#>            -2.591e-01              6.600e-03              5.568e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             5.192e-01              1.220e-02  
#> 
#> $`1`[[17]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -6.3622960              0.2681109              0.0535158  
#>           followup_sq                  trial               trial_sq  
#>            -0.0003208              0.0328384              0.0008411  
#>                  sex1                  N_bas                  L_bas  
#>             0.0604727              0.0001008             -0.0156759  
#>                 P_bas  tx_init_bas1:followup  
#>             0.1077737             -0.0004064  
#> 
#> $`1`[[18]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -6.0133538              0.4334731              0.0348728  
#>           followup_sq                  trial               trial_sq  
#>            -0.0002265              0.0119547              0.0008208  
#>                  sex1                  N_bas                  L_bas  
#>            -0.0705135              0.0085622              0.1244049  
#>                 P_bas  tx_init_bas1:followup  
#>             0.1336921             -0.0027683  
#> 
#> $`1`[[19]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.118e+01              1.582e-01              3.449e-02  
#>           followup_sq                  trial               trial_sq  
#>            -1.855e-04              1.280e-01             -5.071e-05  
#>                  sex1                  N_bas                  L_bas  
#>            -2.862e-02              7.598e-03              5.807e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             6.667e-01              1.608e-03  
#> 
#> $`1`[[20]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -5.0893119              0.0368604              0.0405044  
#>           followup_sq                  trial               trial_sq  
#>            -0.0002072              0.0201730              0.0008274  
#>                  sex1                  N_bas                  L_bas  
#>             0.0768989              0.0011696             -0.0736585  
#>                 P_bas  tx_init_bas1:followup  
#>             0.0237280              0.0021279  
#> 
#> $`1`[[21]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.076e+01              3.120e-01              2.902e-02  
#>           followup_sq                  trial               trial_sq  
#>            -1.353e-04              1.069e-01              2.284e-04  
#>                  sex1                  N_bas                  L_bas  
#>             3.399e-01              1.873e-04             -2.830e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             6.125e-01             -5.477e-03
km_curve(model)    # Prints the survival curve
```

![Survival curve by treatment
group.](SEQuential_files/figure-html/outcome-1.png)

``` r

risk_data(model)
#> Index: <Followup>
#>    Method Followup      A      Risk   95% LCI   95% UCI         SE
#>    <char>    <num> <char>     <num>     <num>     <num>      <num>
#> 1:    ITT       60      0 0.8372582 0.7723191 0.9021973 0.03313281
#> 2:    ITT       60      1 0.8744359 0.8137729 0.9350989 0.03095109
risk_comparison(model)
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60 risk_0 risk_1  1.0444041  1.0030635  1.0874485 0.02060634
#> 2:       60 risk_1 risk_0  0.9574838  0.9195838  0.9969459 0.02060634
#>    Risk Difference   RD 95% LCI   RD 95% UCI      RD SE
#>              <num>        <num>        <num>      <num>
#> 1:      0.03717768  0.002812309  0.071543050 0.01753367
#> 2:     -0.03717768 -0.071543050 -0.002812309 0.01753367
```
