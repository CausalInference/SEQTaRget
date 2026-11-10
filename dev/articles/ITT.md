# Intention-To-Treat Analysis

Here, we’ll go over some examples of using ITT. First we need to load
the library before getting in to some sample use cases.

``` r

library(SEQTaRget)
```

## ITT With 5 bootstrap samples

``` r

options <- SEQopts(# tells SEQuential to create Kaplan-Meier curves
                   km.curves = TRUE,
                   # tells SEQuential to bootstrap
                   bootstrap = TRUE,
                   seed = 1636,
                   # tells SEQuential to run bootstraps 5 times
                   bootstrap.nboot = 5)

# use example data
data <- SEQdata                             
model <- SEQuential(data, id.col = "ID", 
                          time.col = "time", 
                          eligible.col = "eligible", 
                          treatment.col = "tx_init", 
                          outcome.col = "outcome", 
                          time_varying.cols = c("N", "L", "P"), 
                          fixed.cols = "sex",
                          method = "ITT", 
                          options = options)
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
#> ITT model created successfully
#> 
#> Creating Survival curves
#> 
#> Completed

km_curve(model, plot.type = "risk")        # retrieve risk plot
```

![](ITT_files/figure-html/unnamed-chunk-2-1.png)

``` r

risk_data(model)
#> Index: <Followup>
#>    Method Followup      A      Risk   95% LCI   95% UCI         SE
#>    <char>    <num> <char>     <num>     <num>     <num>      <num>
#> 1:    ITT       60      0 0.8372582 0.7609485 0.9135679 0.03893423
#> 2:    ITT       60      1 0.8744359 0.8491316 0.8997402 0.01291061
risk_comparison(model)
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60 risk_0 risk_1  1.0444041  0.9729523   1.121103 0.03615714
#> 2:       60 risk_1 risk_0  0.9574838  0.8919786   1.027800 0.03615714
#>    Risk Difference  RD 95% LCI RD 95% UCI      RD SE
#>              <num>       <num>      <num>      <num>
#> 1:      0.03717768 -0.02102223 0.09537759 0.02969438
#> 2:     -0.03717768 -0.09537759 0.02102223 0.02969438
```

## ITT with 5 bootstrap samples and losses-to-followup

``` r

options <- SEQopts(km.curves = TRUE,               
                   bootstrap = TRUE,                
                   seed = 1636,
                   bootstrap.nboot = 5,
                   # tells SEQuential to expect LTFU as the censoring column
                   cense = "LTFU",
                   # tells SEQuential to treat this column as the 
                   # censoring eligibility column
                   cense.eligible = "eligible_cense")

# use example data for LTFU
data <- SEQdata.LTFU
model <- SEQuential(data, id.col = "ID", 
                          time.col = "time", 
                          eligible.col = "eligible", 
                          treatment.col = "tx_init", 
                          outcome.col = "outcome", 
                          time_varying.cols = c("N", "L", "P"), 
                          fixed.cols = "sex",
                          method = "ITT", 
                          options = options)
#> 
#> Full dataset: 54,687 observations, 13 variables
#> 
#> Non-required columns provided, pruning for efficiency
#> 
#> Pruned
#> 
#> Original dataset (eligible subjects): 29,624 observations, 11 variables
#> 
#> Expanding Data...
#> 
#> Pre-filter expansion: 1,609,859 observations
#> 
#> Expanded dataset: 1,119,229 observations, 18 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 1,119,229 observations, 18 variables
#> 
#> Moving forward with ITT analysis
#> 
#> Bootstrapping with 80% of 1,000 subjects (800 subjects, ~895,383 observations per resample) 5 times
#> 
#> ITT model created successfully
#> 
#> Creating Survival curves
#> 
#> Completed

km_curve(model, plot.type = "risk")
```

![](ITT_files/figure-html/unnamed-chunk-3-1.png)

``` r

risk_data(model)
#> Index: <Followup>
#>    Method Followup      A       Risk 95% LCI    95% UCI         SE
#>    <char>    <num> <char>      <num>   <num>      <num>      <num>
#> 1:    ITT       60      0 0.02374360       0 0.05226056 0.01454974
#> 2:    ITT       60      1 0.02614576       0 0.06765504 0.02117859
risk_comparison(model)
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60 risk_0 risk_1  1.1011710  0.8417202   1.440595  0.1370851
#> 2:       60 risk_1 risk_0  0.9081242  0.6941578   1.188043  0.1370851
#>    Risk Difference  RD 95% LCI RD 95% UCI       RD SE
#>              <num>       <num>      <num>       <num>
#> 1:     0.002402164 -0.01289278 0.01769710 0.007803684
#> 2:    -0.002402164 -0.01769710 0.01289278 0.007803684
```

## ITT with 5 bootstrap samples and competing events

``` r

options <- SEQopts(km.curves = TRUE,               
                   bootstrap = TRUE,                
                   seed = 1636,
                   bootstrap.nboot = 5,
                   # Using LTFU as our competing event
                   compevent = "LTFU")

data <- SEQdata.LTFU
model <- SEQuential(data, id.col = "ID", 
                          time.col = "time", 
                          eligible.col = "eligible", 
                          treatment.col = "tx_init", 
                          outcome.col = "outcome", 
                          time_varying.cols = c("N", "L", "P"), 
                          fixed.cols = "sex",
                          method = "ITT", 
                          options = options)
#> 
#> Full dataset: 54,687 observations, 13 variables
#> 
#> Non-required columns provided, pruning for efficiency
#> 
#> Pruned
#> 
#> Original dataset (eligible subjects): 29,624 observations, 10 variables
#> 
#> Expanding Data...
#> 
#> Pre-filter expansion: 1,609,859 observations
#> 
#> Expanded dataset: 1,119,229 observations, 14 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 1,119,229 observations, 14 variables
#> 
#> Moving forward with ITT analysis
#> 
#> Bootstrapping with 80% of 1,000 subjects (800 subjects, ~895,383 observations per resample) 5 times
#> 
#> ITT model created successfully
#> 
#> Creating Survival curves
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> 
#> Completed

km_curve(model, plot.type = "risk")
```

![](ITT_files/figure-html/unnamed-chunk-4-1.png)

``` r

risk_data(model)
#> Index: <Followup>
#>    Method Followup      A  Risk 95% LCI 95% UCI    SE
#>    <char>    <num> <char> <num>   <num>   <num> <num>
#> 1:    ITT       60      0    NA      NA      NA    NA
#> 2:    ITT       60      1    NA      NA      NA    NA
risk_comparison(model)
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60  inc_0  inc_1         NA         NA         NA         NA
#> 2:       60  inc_1  inc_0         NA         NA         NA         NA
#>    Risk Difference RD 95% LCI RD 95% UCI RD SE
#>              <num>      <num>      <num> <num>
#> 1:              NA         NA         NA    NA
#> 2:              NA         NA         NA    NA
```

## ITT hazard ratio with 5 bootstrap samples and competing events

``` r

options <- SEQopts(# km.curves must be set to FALSE to turn on hazard 
                   # ratio creation
                   km.curves = FALSE,
                   # set hazard to TRUE for hazard ratio creation
                   hazard = TRUE,
                   bootstrap = TRUE,                
                   seed = 1636,
                   bootstrap.nboot = 5,     
                   compevent = "LTFU")

data <- SEQdata.LTFU                          
model <- SEQuential(data, id.col = "ID", 
                          time.col = "time", 
                          eligible.col = "eligible", 
                          treatment.col = "tx_init", 
                          outcome.col = "outcome", 
                          time_varying.cols = c("N", "L", "P"), 
                          fixed.cols = "sex",
                          method = "ITT", 
                          options = options)
#> 
#> Full dataset: 54,687 observations, 13 variables
#> 
#> Non-required columns provided, pruning for efficiency
#> 
#> Pruned
#> 
#> Original dataset (eligible subjects): 29,624 observations, 10 variables
#> 
#> Expanding Data...
#> 
#> Pre-filter expansion: 1,609,859 observations
#> 
#> Expanded dataset: 1,119,229 observations, 14 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 1,119,229 observations, 14 variables
#> 
#> Moving forward with ITT analysis
#> 
#> Bootstrapping with 80% of 1,000 subjects (800 subjects, ~895,383 observations per resample) 5 times
#> 
#> Completed

# retrieve hazard ratios
hazard_ratio(model)
#> Hazard ratio          LCI          UCI 
#>    1.1241440    0.8317355    1.5193527
```

## ITT with 5 bootstrap samples and competing events in subgroups defined by sex

``` r

options <- SEQopts(km.curves = TRUE,               
                   bootstrap = TRUE,                
                   seed = 1636,
                   bootstrap.nboot = 5,     
                   compevent = "LTFU",
                   # define the subgroup
                   subgroup = "sex")

data <- SEQdata.LTFU
model <- SEQuential(data, id.col = "ID", 
                          time.col = "time", 
                          eligible.col = "eligible", 
                          treatment.col = "tx_init", 
                          outcome.col = "outcome", 
                          time_varying.cols = c("N", "L", "P"), 
                          fixed.cols = "sex",
                          method = "ITT", 
                          options = options)
#> 
#> Full dataset: 54,687 observations, 13 variables
#> 
#> Non-required columns provided, pruning for efficiency
#> 
#> Pruned
#> 
#> Original dataset (eligible subjects): 29,624 observations, 10 variables
#> 
#> Expanding Data...
#> 
#> Pre-filter expansion: 1,609,859 observations
#> 
#> Expanded dataset: 1,119,229 observations, 14 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 1,119,229 observations, 14 variables
#> 
#> Moving forward with ITT analysis
#> 
#> Bootstrapping with 80% of 1,000 subjects (800 subjects, ~895,383 observations per resample) 5 times
#> 
#> ITT model created successfully
#> 
#> Creating Survival Curves for sex_0
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> 
#> Creating Survival Curves for sex_1
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: Cholesky factorization is singular or ill-conditioned; the model was
#> fit with pivoted QR (method = 0) instead.
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> Warning: prediction is not estimable for some rows of the rank-deficient design
#> 
#> Completed

km_curve(model, plot.type = "risk")
#> $sex_0
```

![](ITT_files/figure-html/unnamed-chunk-6-1.png)

    #> 
    #> $sex_1

![](ITT_files/figure-html/unnamed-chunk-6-2.png)

``` r

risk_data(model)
#> $sex_0
#> Index: <Followup>
#>    Method Followup      A  Risk 95% LCI 95% UCI    SE
#>    <char>    <num> <char> <num>   <num>   <num> <num>
#> 1:    ITT       60      0    NA      NA      NA    NA
#> 2:    ITT       60      1    NA      NA      NA    NA
#> 
#> $sex_1
#> Index: <Followup>
#>    Method Followup      A  Risk 95% LCI 95% UCI    SE
#>    <char>    <num> <char> <num>   <num>   <num> <num>
#> 1:    ITT       60      0    NA      NA      NA    NA
#> 2:    ITT       60      1    NA      NA      NA    NA
risk_comparison(model)
#> $sex_0
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60  inc_0  inc_1         NA         NA         NA         NA
#> 2:       60  inc_1  inc_0         NA         NA         NA         NA
#>    Risk Difference RD 95% LCI RD 95% UCI RD SE
#>              <num>      <num>      <num> <num>
#> 1:              NA         NA         NA    NA
#> 2:              NA         NA         NA    NA
#> 
#> $sex_1
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60  inc_0  inc_1         NA         NA         NA         NA
#> 2:       60  inc_1  inc_0         NA         NA         NA         NA
#>    Risk Difference RD 95% LCI RD 95% UCI RD SE
#>              <num>      <num>      <num> <num>
#> 1:              NA         NA         NA    NA
#> 2:              NA         NA         NA    NA
```
