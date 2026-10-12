# Per-Protocol: Dose-Response Analysis

Here, we’ll go over some examples of using dose-response. First we need
to load the library before getting in to some sample use cases.

``` r

library(SEQTaRget)
```

Currently, dose-response analysis through SEQuential only supports
binary treatment values. Therefore; running multinomial models will lead
to errors.

## Dose-response With 5 bootstrap samples

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
                          method = "dose-response", 
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
#> Expanded dataset: 248,485 observations, 15 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 248,485 observations, 15 variables
#> 
#> Moving forward with dose-response analysis
#> 
#> Bootstrapping with 80% of 300 subjects (240 subjects, ~198,788 observations per resample) 5 times
#> 
#> dose-response model created successfully
#> 
#> Creating Survival curves
#> 
#> Completed

km_curve(model, plot.type = "risk")        # retrieve risk plot
```

![](doseresponse_files/figure-html/unnamed-chunk-2-1.png)

``` r

risk_data(model)
#> Index: <Followup>
#>           Method Followup      A      Risk    95% LCI   95% UCI         SE
#>           <char>    <num> <char>     <num>      <num>     <num>      <num>
#> 1: dose-response       60      0 0.5282782 0.03851259 1.0000000 0.24988501
#> 2: dose-response       60      1 0.8949096 0.85228977 0.9375295 0.02174522
risk_comparison(model)
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60 risk_0 risk_1  1.6940120  0.7612927   3.769479  0.4080876
#> 2:       60 risk_1 risk_0  0.5903146  0.2652887   1.313555  0.4080876
#>    Risk Difference RD 95% LCI RD 95% UCI     RD SE
#>              <num>      <num>      <num>     <num>
#> 1:       0.3666314 -0.1084919  0.8417547 0.2424143
#> 2:      -0.3666314 -0.8417547  0.1084919 0.2424143
```

## Dose-response with 5 bootstrap samples and losses-to-followup

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
                          method = "dose-response", 
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
#> Expanded dataset: 1,119,229 observations, 17 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 1,119,229 observations, 17 variables
#> 
#> Moving forward with dose-response analysis
#> 
#> Bootstrapping with 80% of 1,000 subjects (800 subjects, ~895,383 observations per resample) 5 times
#> 
#> dose-response model created successfully
#> 
#> Creating Survival curves
#> 
#> Completed

km_curve(model, plot.type = "risk")
```

![](doseresponse_files/figure-html/unnamed-chunk-3-1.png)

``` r

risk_data(model)
#> Index: <Followup>
#>           Method Followup      A        Risk 95% LCI    95% UCI         SE
#>           <char>    <num> <char>       <num>   <num>      <num>      <num>
#> 1: dose-response       60      0 0.007952468       0 0.04501433 0.01890946
#> 2: dose-response       60      1 0.018718195       0 0.04142307 0.01158433
risk_comparison(model)
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60 risk_0 risk_1  2.3537592 0.21044019  26.326637   1.231945
#> 2:       60 risk_1 risk_0  0.4248523 0.03798434   4.751944   1.231945
#>    Risk Difference  RD 95% LCI RD 95% UCI      RD SE
#>              <num>       <num>      <num>      <num>
#> 1:      0.01076573 -0.01819694 0.03972839 0.01477714
#> 2:     -0.01076573 -0.03972839 0.01819694 0.01477714
```

## Dose-response with 5 bootstrap samples and competing events

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
                          method = "dose-response", 
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
#> Expanded dataset: 1,119,229 observations, 16 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 1,119,229 observations, 16 variables
#> 
#> Moving forward with dose-response analysis
#> 
#> Bootstrapping with 80% of 1,000 subjects (800 subjects, ~895,383 observations per resample) 5 times
#> 
#> dose-response model created successfully
#> 
#> Creating Survival curves
#> 
#> Completed

km_curve(model, plot.type = "risk")
```

![](doseresponse_files/figure-html/unnamed-chunk-4-1.png)

``` r

risk_data(model)
#> Index: <Followup>
#>           Method Followup      A        Risk 95% LCI    95% UCI         SE
#>           <char>    <num> <char>       <num>   <num>      <num>      <num>
#> 1: dose-response       60      0 0.007586789       0 0.04126782 0.01718451
#> 2: dose-response       60      1 0.003641046       0 0.03315852 0.01506021
risk_comparison(model)
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60  inc_0  inc_1  0.4799192 0.03776639   6.098608   1.297064
#> 2:       60  inc_1  inc_0  2.0836841 0.16397183  26.478569   1.297064
#>    Risk Difference  RD 95% LCI RD 95% UCI      RD SE
#>              <num>       <num>      <num>      <num>
#> 1:    -0.003945743 -0.03928796 0.03139647 0.01803207
#> 2:     0.003945743 -0.03139647 0.03928796 0.01803207
```

## Dose-response hazard ratio with 5 bootstrap samples and competing events

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
                          method = "dose-response", 
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
#> Expanded dataset: 1,119,229 observations, 16 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 1,119,229 observations, 16 variables
#> 
#> Moving forward with dose-response analysis
#> 
#> Bootstrapping with 80% of 1,000 subjects (800 subjects, ~895,383 observations per resample) 5 times
#> 
#> Completed

# retrieve hazard ratios
hazard_ratio(model)
#> Hazard ratio          LCI          UCI 
#>    1.0922586    0.9723626    1.2269383
```

## Dose-response with 5 bootstrap samples and competing events in subgroups defined by sex

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
                          method = "dose-response", 
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
#> Expanded dataset: 1,119,229 observations, 16 variables
#> 
#> Expansion Successful
#> 
#> Final analysis dataset: 1,119,229 observations, 16 variables
#> 
#> Moving forward with dose-response analysis
#> 
#> Bootstrapping with 80% of 1,000 subjects (800 subjects, ~895,383 observations per resample) 5 times
#> 
#> dose-response model created successfully
#> 
#> Creating Survival Curves for sex_0 
#> 
#> Creating Survival Curves for sex_1 
#> 
#> Completed

km_curve(model, plot.type = "risk")
#> $sex_0
```

![](doseresponse_files/figure-html/unnamed-chunk-6-1.png)

    #> 
    #> $sex_1

![](doseresponse_files/figure-html/unnamed-chunk-6-2.png)

``` r

risk_data(model)
#> $sex_0
#> Index: <Followup>
#>           Method Followup      A       Risk 95% LCI    95% UCI         SE
#>           <char>    <num> <char>      <num>   <num>      <num>      <num>
#> 1: dose-response       60      0 0.01063229       0 0.10408278 0.04767970
#> 2: dose-response       60      1 0.02718814       0 0.07167468 0.02269763
#> 
#> $sex_1
#> Index: <Followup>
#>           Method Followup      A        Risk 95% LCI    95% UCI          SE
#>           <char>    <num> <char>       <num>   <num>      <num>       <num>
#> 1: dose-response       60      0 0.006494985       0 0.01409034 0.003875252
#> 2: dose-response       60      1 0.005925412       0 0.02610814 0.010297499
risk_comparison(model)
#> $sex_0
#>    Followup    A_x    A_y Risk Ratio  RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>       <num>      <num>      <num>
#> 1:       60  inc_0  inc_1  2.5571299 0.006840718   955.8812   3.022376
#> 2:       60  inc_1  inc_0  0.3910634 0.001046155   146.1835   3.022376
#>    Risk Difference  RD 95% LCI RD 95% UCI      RD SE
#>              <num>       <num>      <num>      <num>
#> 1:      0.01655585 -0.06846066 0.10157237 0.04337657
#> 2:     -0.01655585 -0.10157237 0.06846066 0.04337657
#> 
#> $sex_1
#>    Followup    A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI log(RR) SE
#>       <num> <fctr> <fctr>      <num>      <num>      <num>      <num>
#> 1:       60  inc_0  inc_1  0.9123056  0.1358191   6.128016  0.9717787
#> 2:       60  inc_1  inc_0  1.0961239  0.1631850   7.362734  0.9717787
#>    Risk Difference  RD 95% LCI RD 95% UCI      RD SE
#>              <num>       <num>      <num>      <num>
#> 1:   -0.0005695735 -0.02407448 0.02293534 0.01199252
#> 2:    0.0005695735 -0.02293534 0.02407448 0.01199252
```
