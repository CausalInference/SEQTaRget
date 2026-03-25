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
#> Non-required columns provided, pruning for efficiency
#> Pruned
#> Expanding Data...
#> Expansion Successful
#> Moving forward with dose-response analysis
#> Bootstrapping with 80 % of data 5 times
#> dose-response model created successfully
#> Creating Survival curves
#> Completed

km_curve(model, plot.type = "risk")        # retrieve risk plot
```

![](doseresponse_files/figure-html/unnamed-chunk-2-1.png)

``` r
risk_data(model)
#> [[1]]
#>           Method      A      Risk   95% LCI   95% UCI         SE
#>           <char> <char>     <num>     <num>     <num>      <num>
#> 1: dose-response      0 0.5282782 0.1234894 0.9330671 0.20652872
#> 2: dose-response      1 0.8949096 0.8385037 0.9513155 0.02877903
risk_comparison(model)
#> [[1]]
#>       A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI Risk Differerence  RD 95% LCI
#>    <fctr> <fctr>      <num>      <num>      <num>             <num>       <num>
#> 1: risk_0 risk_1  1.6940120  0.7764767   3.695766         0.3666314 -0.07309331
#> 2: risk_1 risk_0  0.5903146  0.2705799   1.287869        -0.3666314 -0.80635612
#>    RD 95% UCI
#>         <num>
#> 1: 0.80635612
#> 2: 0.07309331
```

## dose-response with 5 bootstrap samples and losses-to-followup

``` r
options <- SEQopts(km.curves = TRUE,               
                   bootstrap = TRUE,                
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
#> Non-required columns provided, pruning for efficiency
#> Pruned
#> Expanding Data...
#> Expansion Successful
#> Moving forward with dose-response analysis
#> Bootstrapping with 80 % of data 5 times
#> dose-response model created successfully
#> Creating Survival curves
#> Completed

km_curve(model, plot.type = "risk")
```

![](doseresponse_files/figure-html/unnamed-chunk-3-1.png)

``` r
risk_data(model)
#> [[1]]
#>           Method      A        Risk     95% LCI    95% UCI          SE
#>           <char> <char>       <num>       <num>      <num>       <num>
#> 1: dose-response      0 0.007847443 0.000000000 0.02684988 0.009695299
#> 2: dose-response      1 0.018827788 0.001997953 0.03565762 0.008586808
risk_comparison(model)
#> [[1]]
#>       A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI Risk Differerence  RD 95% LCI
#>    <fctr> <fctr>      <num>      <num>      <num>             <num>       <num>
#> 1: risk_0 risk_1  2.3992259 0.44856850  12.832566        0.01098034 -0.01349983
#> 2: risk_1 risk_0  0.4168011 0.07792674   2.229314       -0.01098034 -0.03546052
#>    RD 95% UCI
#>         <num>
#> 1: 0.03546052
#> 2: 0.01349983
```

## dose-response with 5 bootstrap samples and competing events

``` r
options <- SEQopts(km.curves = TRUE,               
                   bootstrap = TRUE,                
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
#> Non-required columns provided, pruning for efficiency
#> Pruned
#> Expanding Data...
#> Expansion Successful
#> Moving forward with dose-response analysis
#> Bootstrapping with 80 % of data 5 times
#> dose-response model created successfully
#> Creating Survival curves
#> Completed

km_curve(model, plot.type = "risk")
```

![](doseresponse_files/figure-html/unnamed-chunk-4-1.png)

``` r
risk_data(model)
#> [[1]]
#>           Method      A        Risk 95% LCI    95% UCI         SE
#>           <char> <char>       <num>   <num>      <num>      <num>
#> 1: dose-response      0 0.007586789       0 0.25972299 0.12864328
#> 2: dose-response      1 0.012004295       0 0.03730288 0.01290768
risk_comparison(model)
#> [[1]]
#>       A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI Risk Differerence RD 95% LCI
#>    <fctr> <fctr>      <num>      <num>      <num>             <num>      <num>
#> 1:  inc_0  inc_1  1.5822630 0.07792450   32.12797       0.004417506 -0.2369653
#> 2:  inc_1  inc_0  0.6320062 0.03112552   12.83294      -0.004417506 -0.2458004
#>    RD 95% UCI
#>         <num>
#> 1:  0.2458004
#> 2:  0.2369653
```

## dose-response hazard ratio with 5 bootstrap samples and competing events

``` r
options <- SEQopts(# km.curves must be set to FALSE to turn on hazard 
                   # ratio creation
                   km.curves = FALSE,
                   # set hazard to TRUE for hazard ratio creation
                   hazard = TRUE,
                   bootstrap = TRUE,                
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
#> Non-required columns provided, pruning for efficiency
#> Pruned
#> Expanding Data...
#> Expansion Successful
#> Moving forward with dose-response analysis
#> Bootstrapping with 80 % of data 5 times
#> Completed

# retrieve hazard ratios
hazard_ratio(model)
#> [[1]]
#> Hazard ratio          LCI          UCI 
#>    0.9582143    0.7107252    1.2918841
```

## dose-response with 5 bootstrap samples and competing events in subgroups defined by sex

``` r
options <- SEQopts(km.curves = TRUE,               
                   bootstrap = TRUE,                
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
#> Non-required columns provided, pruning for efficiency
#> Pruned
#> Expanding Data...
#> Expansion Successful
#> Moving forward with dose-response analysis
#> Bootstrapping with 80 % of data 5 times
#> dose-response model created successfully
#> Creating Survival Curves for sex_0 
#> Creating Survival Curves for sex_1 
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
#>           Method      A       Risk 95% LCI    95% UCI         SE
#>           <char> <char>      <num>   <num>      <num>      <num>
#> 1: dose-response      0 0.01125753       0 0.04037236 0.01485477
#> 2: dose-response      1 0.01869016       0 0.06389465 0.02306394
#> 
#> $sex_1
#>           Method      A       Risk 95% LCI    95% UCI         SE
#>           <char> <char>      <num>   <num>      <num>      <num>
#> 1: dose-response      0 0.00659838       0 0.47302574 0.23797751
#> 2: dose-response      1 0.01221464       0 0.04633477 0.01740855
risk_comparison(model)
#> $sex_0
#>       A_x    A_y Risk Ratio   RR 95% LCI RR 95% UCI Risk Differerence
#>    <fctr> <fctr>      <num>        <num>      <num>             <num>
#> 1:  inc_0  inc_1  1.6602358 3.328249e-05   82817.80       0.007432626
#> 2:  inc_1  inc_0  0.6023241 1.207470e-05   30045.82      -0.007432626
#>     RD 95% LCI RD 95% UCI
#>          <num>      <num>
#> 1: -0.04444686 0.05931211
#> 2: -0.05931211 0.04444686
#> 
#> $sex_1
#>       A_x    A_y Risk Ratio RR 95% LCI RR 95% UCI Risk Differerence RD 95% LCI
#>    <fctr> <fctr>      <num>      <num>      <num>             <num>      <num>
#> 1:  inc_0  inc_1  1.8511568 0.06372359   53.77571       0.005616256 -0.4304995
#> 2:  inc_1  inc_0  0.5402028 0.01859576   15.69278      -0.005616256 -0.4417320
#>    RD 95% UCI
#>         <num>
#> 1:  0.4417320
#> 2:  0.4304995
```
