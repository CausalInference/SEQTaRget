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
                   bootstrap = TRUE, #asks the model to preform bootstrapping
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
#> Non-required columns provided, pruning for efficiency
#> Pruned
#> Expanding Data...
#> Expansion Successful
#> Moving forward with ITT analysis
#> Bootstrapping with 80 % of data 20 times
#> ITT model created successfully
#> Creating Survival curves
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
#> [[1]]
#> [[1]][[1]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -6.85931555            0.22530938            0.03538172 
#>           followup_sq                 trial              trial_sq 
#>           -0.00015987            0.04471790            0.00057617 
#>                  sex1                 N_bas                 L_bas 
#>            0.12704583            0.00328671           -0.01385088 
#>                 P_bas tx_init_bas1:followup 
#>            0.20092890           -0.00170402 
#> 
#> [[1]][[2]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -5.70666923            0.13330124            0.03787358 
#>           followup_sq                 trial              trial_sq 
#>           -0.00035674            0.02644011            0.00086039 
#>                  sex1                 N_bas                 L_bas 
#>            0.36001344            0.00145292           -0.19058426 
#>                 P_bas tx_init_bas1:followup 
#>            0.11295147           -0.00267922 
#> 
#> [[1]][[3]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -9.1712e+00            2.2831e-01            5.4841e-02 
#>           followup_sq                 trial              trial_sq 
#>           -4.0688e-04            1.0570e-01           -4.0883e-05 
#>                  sex1                 N_bas                 L_bas 
#>           -4.2007e-02            2.0687e-03           -5.2355e-02 
#>                 P_bas tx_init_bas1:followup 
#>            4.1592e-01           -4.5406e-03 
#> 
#> [[1]][[4]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -1.2717e+01            1.4785e-01            2.7985e-02 
#>           followup_sq                 trial              trial_sq 
#>           -5.0107e-05            1.5879e-01           -1.2503e-04 
#>                  sex1                 N_bas                 L_bas 
#>            1.7735e-01            1.9286e-03           -8.5079e-02 
#>                 P_bas tx_init_bas1:followup 
#>            8.3786e-01            5.3018e-03 
#> 
#> [[1]][[5]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -4.12618796            0.31433497            0.04437797 
#>           followup_sq                 trial              trial_sq 
#>           -0.00016160           -0.00266764            0.00077599 
#>                  sex1                 N_bas                 L_bas 
#>            0.18135760            0.00240254            0.06506529 
#>                 P_bas tx_init_bas1:followup 
#>           -0.09583373           -0.00892581 
#> 
#> [[1]][[6]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -4.6104e+00           -2.5621e-02            3.1344e-02 
#>           followup_sq                 trial              trial_sq 
#>           -6.0884e-05            1.8695e-04            9.5960e-04 
#>                  sex1                 N_bas                 L_bas 
#>            1.9846e-01            3.8789e-03           -5.5887e-04 
#>                 P_bas tx_init_bas1:followup 
#>           -5.6365e-02            8.0484e-03 
#> 
#> [[1]][[7]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -1.4647e+01           -7.5208e-03            4.9683e-02 
#>           followup_sq                 trial              trial_sq 
#>           -2.5384e-04            2.0944e-01           -6.0223e-04 
#>                  sex1                 N_bas                 L_bas 
#>           -3.2339e-02            6.3060e-03           -8.7478e-03 
#>                 P_bas tx_init_bas1:followup 
#>            9.8581e-01           -6.3076e-04 
#> 
#> [[1]][[8]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -9.39071557            0.20227298            0.02731706 
#>           followup_sq                 trial              trial_sq 
#>           -0.00012328            0.09938015            0.00014525 
#>                  sex1                 N_bas                 L_bas 
#>           -0.12188854           -0.00027847           -0.04695586 
#>                 P_bas tx_init_bas1:followup 
#>            0.51985467           -0.00103007 
#> 
#> [[1]][[9]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>            2.5837e+00            1.0534e-01            3.2412e-02 
#>           followup_sq                 trial              trial_sq 
#>           -2.6869e-05           -1.2370e-01            1.6482e-03 
#>                  sex1                 N_bas                 L_bas 
#>           -1.3761e-01           -1.8333e-04           -4.8060e-03 
#>                 P_bas tx_init_bas1:followup 
#>           -7.7720e-01            1.5539e-04 
#> 
#> [[1]][[10]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -7.43904169            0.41483864            0.03157776 
#>           followup_sq                 trial              trial_sq 
#>           -0.00012752            0.05300735            0.00039862 
#>                  sex1                 N_bas                 L_bas 
#>            0.30994058            0.00253962           -0.03247561 
#>                 P_bas tx_init_bas1:followup 
#>            0.24637872           -0.00390270 
#> 
#> [[1]][[11]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -1.3729e+01            4.3212e-01            4.9985e-02 
#>           followup_sq                 trial              trial_sq 
#>           -4.0294e-04            1.7345e-01           -2.2957e-04 
#>                  sex1                 N_bas                 L_bas 
#>            2.8150e-01            1.8517e-03           -6.0796e-02 
#>                 P_bas tx_init_bas1:followup 
#>            9.0134e-01           -5.5006e-03 
#> 
#> [[1]][[12]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -6.29810490            0.09170225            0.03106654 
#>           followup_sq                 trial              trial_sq 
#>           -0.00010286            0.03338081            0.00076787 
#>                  sex1                 N_bas                 L_bas 
#>            0.23858347            0.00521740           -0.08825751 
#>                 P_bas tx_init_bas1:followup 
#>            0.12925425            0.00468215 
#> 
#> [[1]][[13]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -7.7480e+00            2.7433e-01            3.3747e-02 
#>           followup_sq                 trial              trial_sq 
#>           -5.5125e-05            5.8901e-02            4.7147e-04 
#>                  sex1                 N_bas                 L_bas 
#>            2.1317e-02            5.3902e-03            2.1536e-02 
#>                 P_bas tx_init_bas1:followup 
#>            2.8434e-01            9.7276e-04 
#> 
#> [[1]][[14]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -1.0517e+01            2.4134e-01            3.3708e-02 
#>           followup_sq                 trial              trial_sq 
#>           -2.3036e-04            1.1756e-01           -3.2449e-05 
#>                  sex1                 N_bas                 L_bas 
#>            1.9467e-02            4.8640e-03           -1.9338e-02 
#>                 P_bas tx_init_bas1:followup 
#>            5.9746e-01           -1.3830e-03 
#> 
#> [[1]][[15]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -8.0412e+00            1.5762e-01            2.9162e-02 
#>           followup_sq                 trial              trial_sq 
#>           -3.5884e-05            5.6090e-02            6.0089e-04 
#>                  sex1                 N_bas                 L_bas 
#>            2.9079e-01            4.5356e-03            4.0074e-02 
#>                 P_bas tx_init_bas1:followup 
#>            3.2306e-01            5.9596e-03 
#> 
#> [[1]][[16]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -1.0158e+01            3.8860e-02            1.4169e-02 
#>           followup_sq                 trial              trial_sq 
#>            3.8820e-05            1.0926e-01            1.0628e-04 
#>                  sex1                 N_bas                 L_bas 
#>            2.8044e-01            8.4144e-03           -1.0576e-01 
#>                 P_bas tx_init_bas1:followup 
#>            5.9723e-01            1.5975e-03 
#> 
#> [[1]][[17]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -9.7258e+00            2.4666e-01            3.8501e-02 
#>           followup_sq                 trial              trial_sq 
#>           -2.4445e-04            1.0169e-01            4.5839e-05 
#>                  sex1                 N_bas                 L_bas 
#>           -2.0150e-01            1.9064e-03            4.4528e-02 
#>                 P_bas tx_init_bas1:followup 
#>            5.1168e-01           -7.6077e-03 
#> 
#> [[1]][[18]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -3.70549908            0.41010698            0.04208706 
#>           followup_sq                 trial              trial_sq 
#>           -0.00043094           -0.01937940            0.00087804 
#>                  sex1                 N_bas                 L_bas 
#>            0.13180142            0.00940757            0.06088783 
#>                 P_bas tx_init_bas1:followup 
#>           -0.12648565           -0.00869196 
#> 
#> [[1]][[19]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -3.63857072            0.08367455            0.03129809 
#>           followup_sq                 trial              trial_sq 
#>           -0.00016821           -0.02134892            0.00116805 
#>                  sex1                 N_bas                 L_bas 
#>            0.19346081            0.00214429            0.01152500 
#>                 P_bas tx_init_bas1:followup 
#>           -0.11657712            0.00271366 
#> 
#> [[1]][[20]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -4.95028803            0.41972678            0.03416014 
#>           followup_sq                 trial              trial_sq 
#>           -0.00027814            0.00948588            0.00081151 
#>                  sex1                 N_bas                 L_bas 
#>           -0.00093079            0.00497737           -0.18952225 
#>                 P_bas tx_init_bas1:followup 
#>            0.03571644           -0.01100419 
#> 
#> [[1]][[21]]
#> 
#> Call:
#> fastglm.default(x = X, y = y, family = quasibinomial(), method = params@fastglm.method)
#> 
#> Coefficients:
#>           (Intercept)          tx_init_bas1              followup 
#>           -3.85162313            0.08993308            0.03265672 
#>           followup_sq                 trial              trial_sq 
#>           -0.00022807           -0.01443944            0.00087488 
#>                  sex1                 N_bas                 L_bas 
#>            0.47011613            0.00522408            0.12256776 
#>                 P_bas tx_init_bas1:followup 
#>           -0.11681741           -0.00110875
km_curve(model)    # Prints the survival curve
```

![Survival curve by treatment
group.](SEQuential_files/figure-html/outcome-1.png)

``` r
risk_data(model)
#> [[1]]
#>    Method      A      Risk   95% LCI   95% UCI         SE
#>    <char> <char>     <num>     <num>     <num>      <num>
#> 1:    ITT      0 0.8372582 0.7738757 0.9006407 0.03233859
#> 2:    ITT      1 0.8744359 0.8135711 0.9353007 0.03105406
risk_comparison(model)
#> [[1]]
#>       A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI Risk Differerence  RD 95% LCI
#>    <fctr> <fctr>      <num>      <num>      <num>             <num>       <num>
#> 1: risk_0 risk_1  1.0444041  0.9794216   1.113698        0.03717768 -0.01764957
#> 2: risk_1 risk_0  0.9574838  0.8979095   1.021011       -0.03717768 -0.09200493
#>    RD 95% UCI
#>         <num>
#> 1: 0.09200493
#> 2: 0.01764957
```
