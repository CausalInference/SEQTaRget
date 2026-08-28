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
                   seed = 1636, #fixes the resamples so the intervals are reproducible
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
#>            -7.5686664              0.0572035              0.0481829  
#>           followup_sq                  trial               trial_sq  
#>            -0.0002398              0.0694546              0.0002991  
#>                  sex1                  N_bas                  L_bas  
#>             0.3623319              0.0012608              0.0078599  
#>                 P_bas  tx_init_bas1:followup  
#>             0.2267346             -0.0008032  
#> 
#> $`1`[[3]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.068e+01              2.771e-01              3.246e-02  
#>           followup_sq                  trial               trial_sq  
#>            -1.191e-04              1.086e-01              2.596e-04  
#>                  sex1                  N_bas                  L_bas  
#>             1.961e-01              4.332e-03             -1.251e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             5.994e-01             -1.063e-05  
#> 
#> $`1`[[4]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -0.9669277             -0.0847453              0.0222841  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001863             -0.0594655              0.0011550  
#>                  sex1                  N_bas                  L_bas  
#>             0.3406604              0.0067373              0.0408462  
#>                 P_bas  tx_init_bas1:followup  
#>            -0.3909486              0.0071088  
#> 
#> $`1`[[5]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -7.5351394              0.4268098              0.0373349  
#>           followup_sq                  trial               trial_sq  
#>            -0.0002502              0.0526084              0.0005318  
#>                  sex1                  N_bas                  L_bas  
#>             0.5449350              0.0047661             -0.1495825  
#>                 P_bas  tx_init_bas1:followup  
#>             0.2469561             -0.0042417  
#> 
#> $`1`[[6]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -7.6406240             -0.1156940              0.0389073  
#>           followup_sq                  trial               trial_sq  
#>            -0.0002795              0.0742193              0.0004675  
#>                  sex1                  N_bas                  L_bas  
#>             0.3281369              0.0083933             -0.1784712  
#>                 P_bas  tx_init_bas1:followup  
#>             0.2912902              0.0030521  
#> 
#> $`1`[[7]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -9.0018102              0.3579004              0.0317738  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001707              0.0776669              0.0004174  
#>                  sex1                  N_bas                  L_bas  
#>             0.0955384              0.0056463             -0.0122759  
#>                 P_bas  tx_init_bas1:followup  
#>             0.4311688             -0.0042654  
#> 
#> $`1`[[8]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>             -4.217360               0.224570               0.047272  
#>           followup_sq                  trial               trial_sq  
#>             -0.000149              -0.009824               0.001069  
#>                  sex1                  N_bas                  L_bas  
#>              0.275748               0.004007               0.065621  
#>                 P_bas  tx_init_bas1:followup  
#>             -0.122678              -0.004013  
#> 
#> $`1`[[9]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -4.536e+00              1.509e-01              3.540e-02  
#>           followup_sq                  trial               trial_sq  
#>            -6.786e-05              1.980e-03              8.863e-04  
#>                  sex1                  N_bas                  L_bas  
#>             1.457e-01              7.400e-03             -3.109e-02  
#>                 P_bas  tx_init_bas1:followup  
#>            -6.592e-02             -4.370e-03  
#> 
#> $`1`[[10]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -7.123e+00              8.728e-02              3.766e-02  
#>           followup_sq                  trial               trial_sq  
#>            -9.976e-07              5.440e-02              6.127e-04  
#>                  sex1                  N_bas                  L_bas  
#>             1.821e-02              8.177e-05             -2.843e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             2.211e-01              4.661e-03  
#> 
#> $`1`[[11]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -2.201e+00              8.844e-02              2.223e-02  
#>           followup_sq                  trial               trial_sq  
#>             8.478e-05             -4.554e-02              1.403e-03  
#>                  sex1                  N_bas                  L_bas  
#>             9.105e-02              3.699e-03             -1.426e-01  
#>                 P_bas  tx_init_bas1:followup  
#>            -2.769e-01              8.158e-03  
#> 
#> $`1`[[12]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -8.099e+00              4.642e-02              2.612e-02  
#>           followup_sq                  trial               trial_sq  
#>            -2.194e-05              6.413e-02              6.372e-04  
#>                  sex1                  N_bas                  L_bas  
#>             1.690e-01              5.351e-03             -4.436e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             3.349e-01              7.994e-03  
#> 
#> $`1`[[13]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -9.0555175              0.6478047              0.0542168  
#>           followup_sq                  trial               trial_sq  
#>            -0.0004703              0.0799784              0.0001404  
#>                  sex1                  N_bas                  L_bas  
#>             0.2240107              0.0037200              0.0968174  
#>                 P_bas  tx_init_bas1:followup  
#>             0.3840352             -0.0119769  
#> 
#> $`1`[[14]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.046e+01              6.029e-01              5.089e-02  
#>           followup_sq                  trial               trial_sq  
#>            -3.486e-04              1.072e-01              1.740e-04  
#>                  sex1                  N_bas                  L_bas  
#>             1.096e-01              4.194e-03             -5.617e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             5.530e-01             -1.541e-02  
#> 
#> $`1`[[15]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -9.4339042              0.3195517              0.0446209  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001962              0.0984771              0.0001429  
#>                  sex1                  N_bas                  L_bas  
#>             0.0860115              0.0016125             -0.0215202  
#>                 P_bas  tx_init_bas1:followup  
#>             0.4390679             -0.0090178  
#> 
#> $`1`[[16]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -2.5128968              0.2105845              0.0342825  
#>           followup_sq                  trial               trial_sq  
#>            -0.0002374             -0.0369591              0.0008865  
#>                  sex1                  N_bas                  L_bas  
#>             0.1569439              0.0085729              0.1120179  
#>                 P_bas  tx_init_bas1:followup  
#>            -0.2619360             -0.0023137  
#> 
#> $`1`[[17]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -6.124e+00              7.368e-02              2.219e-02  
#>           followup_sq                  trial               trial_sq  
#>             3.226e-06              2.556e-02              6.103e-04  
#>                  sex1                  N_bas                  L_bas  
#>             2.588e-01              5.984e-03              1.137e-01  
#>                 P_bas  tx_init_bas1:followup  
#>             1.317e-01              6.115e-03  
#> 
#> $`1`[[18]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -6.3051565              0.4621448              0.0446341  
#>           followup_sq                  trial               trial_sq  
#>            -0.0001721              0.0389188              0.0006612  
#>                  sex1                  N_bas                  L_bas  
#>            -0.0825751              0.0035661             -0.1198744  
#>                 P_bas  tx_init_bas1:followup  
#>             0.1311997             -0.0090016  
#> 
#> $`1`[[19]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -8.471e+00              2.708e-01              4.048e-02  
#>           followup_sq                  trial               trial_sq  
#>            -8.973e-05              6.878e-02              4.768e-04  
#>                  sex1                  N_bas                  L_bas  
#>             1.661e-01              2.092e-03              6.857e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             3.510e-01             -5.788e-03  
#> 
#> $`1`[[20]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -1.009e+01              1.894e-01              3.772e-02  
#>           followup_sq                  trial               trial_sq  
#>            -1.252e-04              1.117e-01             -6.498e-05  
#>                  sex1                  N_bas                  L_bas  
#>             2.295e-01              2.054e-03              4.281e-02  
#>                 P_bas  tx_init_bas1:followup  
#>             5.282e-01             -2.651e-03  
#> 
#> $`1`[[21]]
#> 
#> Call:  fastglm.default(x = X, y = y, family = family, start = start, 
#>     method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)           tx_init_bas1               followup  
#>            -8.9528151              0.3514119              0.0584130  
#>           followup_sq                  trial               trial_sq  
#>            -0.0005134              0.0986973             -0.0000760  
#>                  sex1                  N_bas                  L_bas  
#>             0.2732790              0.0026618              0.0094163  
#>                 P_bas  tx_init_bas1:followup  
#>             0.3964909             -0.0164729
km_curve(model)    # Prints the survival curve
```

![Survival curve by treatment
group.](SEQuential_files/figure-html/outcome-1.png)

``` r

risk_data(model)
#> Index: <Followup>
#>    Method Followup      A      Risk   95% LCI   95% UCI         SE
#>    <char>    <num> <char>     <num>     <num>     <num>      <num>
#> 1:    ITT       60      0 0.8372582 0.7663552 0.9081612 0.03617568
#> 2:    ITT       60      1 0.8744359 0.8180477 0.9308241 0.02877002
risk_comparison(model)
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60 risk_0 risk_1  1.0444041  0.9654981   1.129759 0.04008114
#> 2:       60 risk_1 risk_0  0.9574838  0.8851448   1.035735 0.04008114
#>    Risk Difference  RD 95% LCI RD 95% UCI      RD SE
#>              <num>       <num>      <num>      <num>
#> 1:      0.03717768 -0.02931187 0.10366723 0.03392386
#> 2:     -0.03717768 -0.10366723 0.02931187 0.03392386
```
