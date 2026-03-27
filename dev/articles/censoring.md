# Per-Protocol: Censoring Analysis

Here, we’ll go over some examples of using per-protocol, censoring.
First we need to load the library before getting in to some sample use
cases.

``` r
library(SEQTaRget)
```

## Per-protocol, censoring, weights in pre-expanded data and no truncation, no excused conditions (i.e. static interventions)

``` r
options <- SEQopts(# tells SEQuential to create Kaplan-Meier curves
                   km.curves = TRUE,
                   # tells SEQuential to weight the outcome model
                   weighted = TRUE, 
                   # tells SEQuential to build weights from the pre-expanded data
                   weight.preexpansion = TRUE)

# use some example data in the package
data <- SEQdata                                
model <- SEQuential(data, 
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)
#> Non-required columns provided, pruning for efficiency
#> Pruned
#> Expanding Data...
#> Expansion Successful
#> Moving forward with censoring analysis
#> censoring model created successfully
#> Creating Survival curves
#> Completed

# retrieve risk plot
km_curve(model, plot.type = "risk")
```

![Risk plot by treatment
group.](censoring_files/figure-html/unnamed-chunk-2-1.png)

``` r
# retrieve survival and risk data
survival_data <- km_data(model)
risk_data(model)
#> $`1`
#>       Method      A      Risk
#>       <char> <char>     <num>
#> 1: censoring      0 0.6596589
#> 2: censoring      1 0.9243520
risk_comparison(model)
#> $`1`
#>       A_x    A_y Risk Ratio Risk Difference
#>    <fctr> <fctr>      <num>           <num>
#> 1: risk_0 risk_1  1.4012576       0.2646931
#> 2: risk_1 risk_0  0.7136447      -0.2646931
```

## Per-protocol, censoring, weights in post-expanded data and no truncation, no excused conditions (i.e. static interventions)

``` r
options <- SEQopts(km.curves = TRUE,
                   weighted = TRUE, 
                   # tells SEQuential to build weights from the post-expanded data
                   weight.preexpansion = FALSE)

data <- SEQdata                                 
model <- SEQuential(data, 
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)
#> Non-required columns provided, pruning for efficiency
#> Pruned
#> Expanding Data...
#> Expansion Successful
#> Moving forward with censoring analysis
#> censoring model created successfully
#> Creating Survival curves
#> Completed

km_curve(model, plot.type = "risk")
```

![Risk plot by treatment
group.](censoring_files/figure-html/unnamed-chunk-3-1.png)

``` r
risk_data(model)
#> $`1`
#>       Method      A      Risk
#>       <char> <char>     <num>
#> 1: censoring      0 0.6533049
#> 2: censoring      1 0.9281893
risk_comparison(model)
#> $`1`
#>       A_x    A_y Risk Ratio Risk Difference
#>    <fctr> <fctr>      <num>           <num>
#> 1: risk_0 risk_1  1.4207598       0.2748844
#> 2: risk_1 risk_0  0.7038488      -0.2748844
```

## Per-protocol, censoring, weights in pre-expanded data and no truncation, excused conditions for initiators and non-initiators (i.e. dynamic interventions)

``` r
options <- SEQopts(km.curves = TRUE,
                   weighted = TRUE,
                   weight.preexpansion = TRUE,
                   # tells SEQuential to run a dynamic intervention
                   excused = TRUE,                               
                   # tells SEQuential to use columns excusedOne and 
                   # excusedZero as excused conditions for treatment switches
                   excused.cols = c("excusedZero", "excusedOne"), 
                   # tells SEQuential to expect treatment levels 0, 1
                   # (mapping to the same positions as the list in excused.cols)
                   treat.level = c(0, 1))
data <- SEQdata                                
model <- SEQuential(data, 
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)
#> Expanding Data...
#> Expansion Successful
#> Moving forward with censoring analysis
#> censoring model created successfully
#> Creating Survival curves
#> Completed

km_curve(model, plot.type = "risk")
```

![Risk plot by treatment
group.](censoring_files/figure-html/unnamed-chunk-4-1.png)

``` r
risk_data(model)
#> $`1`
#>       Method      A      Risk
#>       <char> <char>     <num>
#> 1: censoring      0 0.9647942
#> 2: censoring      1 0.9627635
risk_comparison(model)
#> $`1`
#>       A_x    A_y Risk Ratio Risk Difference
#>    <fctr> <fctr>      <num>           <num>
#> 1: risk_0 risk_1  0.9978953    -0.002030621
#> 2: risk_1 risk_0  1.0021092     0.002030621
```

## Per-protocol, censoring, weights in post-expanded data and no truncation, excused conditions for initiators and non-initiators (i.e. dynamic interventions)

``` r
options <- SEQopts(km.curves = TRUE,
                   weighted = TRUE,
                   weight.preexpansion = FALSE,
                   excused = TRUE,                               
                   excused.cols = c("excusedZero", "excusedOne"), 
                   treat.level = c(0, 1),
                   weight.p99 = TRUE)
data <- SEQdata                                
model <- SEQuential(data, 
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)
#> Expanding Data...
#> Expansion Successful
#> Moving forward with censoring analysis
#> censoring model created successfully
#> Creating Survival curves
#> Completed

km_curve(model, plot.type = "risk")
```

![Risk plot by treatment
group.](censoring_files/figure-html/unnamed-chunk-5-1.png)

``` r
risk_data(model)
#> $`1`
#>       Method      A      Risk
#>       <char> <char>     <num>
#> 1: censoring      0 0.6371076
#> 2: censoring      1 0.9909442
risk_comparison(model)
#> $`1`
#>       A_x    A_y Risk Ratio Risk Difference
#>    <fctr> <fctr>      <num>           <num>
#> 1: risk_0 risk_1  1.5553797       0.3538366
#> 2: risk_1 risk_0  0.6429298      -0.3538366
```

## Per-protocol, censoring, weights in post-expanded data and no truncation, excused conditions for initiators and non-initiators (i.e. dynamic interventions) and a competing event

``` r
options <- SEQopts(km.curves = TRUE,
                   weighted = TRUE,
                   weight.preexpansion = FALSE,
                   excused = TRUE,                               
                   excused.cols = c("excusedZero", "excusedOne"), 
                   treat.level = c(0, 1),
                   # add a competing event
                   compevent = "LTFU")

data <- SEQdata.LTFU                                
model <- SEQuential(data, 
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)
#> Non-required columns provided, pruning for efficiency
#> Pruned
#> Expanding Data...
#> Expansion Successful
#> Moving forward with censoring analysis
#> censoring model created successfully
#> Creating Survival curves
#> Completed

km_curve(model, plot.type = "risk")
```

![Risk plot by treatment
group.](censoring_files/figure-html/unnamed-chunk-6-1.png)

``` r
risk_data(model)
#> $`1`
#>       Method      A       Risk
#>       <char> <char>      <num>
#> 1: censoring      0 0.02542994
#> 2: censoring      1 0.01646542
risk_comparison(model)
#> $`1`
#>       A_x    A_y Risk Ratio Risk Difference
#>    <fctr> <fctr>      <num>           <num>
#> 1:  inc_0  inc_1  0.6474815    -0.008964524
#> 2:  inc_1  inc_0  1.5444456     0.008964524
```

## Per-protocol, censoring, weights in post-expanded data and no truncation, excused conditions for initiators and non-initiators (i.e. dynamic interventions) and hazard ratio

``` r
options <- SEQopts(# tell SEQuential to run hazard ratios
                   hazard = TRUE,
                   weighted = TRUE,
                   weight.preexpansion = FALSE,
                   excused = TRUE,                               
                   excused.cols = c("excusedZero", "excusedOne"),
                   weight.p99 = TRUE)

data <- SEQdata                              
model <- SEQuential(data,
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)
#> Expanding Data...
#> Expansion Successful
#> Moving forward with censoring analysis
#> Completed
hazard_ratio(model)
#> $`1`
#> Hazard ratio          LCI          UCI 
#>     3.005085           NA           NA
```
