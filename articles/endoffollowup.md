# End-of-Follow-up Outcomes

``` r

library(SEQTaRget)
```

## What this is for

The rest of the `SEQTaRget` package estimates survival outcomes: a
binary event that may occur at any point during follow-up, summarised
through risks, survival curves or a hazard ratio.

An end-of-follow-up outcome is different. It is measured once, at a
single follow-up time chosen by the user, e.g., a biomarker at 12
months, disease status at two years, a questionnaire score at the end of
the trial. There is no time-to-event to model, and the quantity of
interest is simply the average outcome in each treatment arm.

The
[`SEQopts()`](https://causalinference.github.io/SEQTaRget/reference/SEQopts.md)
option `end_of_fup = TRUE` switches to that estimand. For each
trial-period the outcome is read at the requested follow-up time and
averaged within each baseline treatment arm, weighted by the
period-trial-specific weight at the time the measurement was taken. For
a binary outcome this is the weighted proportion in each arm; for a
continuous outcome, the weighted mean.

Because there is no outcome model, `end_of_fup` cannot be combined with
`km.curves`, `hazard`, `compevent`, or the dose-response method.

## A minimal example

`end_of_fup.time` is the follow-up time `k` at which the outcome is
evaluated, counted in follow-up periods since trial enrollment (not
calendar time).

``` r

options <- SEQopts(end_of_fup = TRUE,
                   # evaluate the outcome 12 follow-up periods after enrollment
                   end_of_fup.time = 12,
                   # "binary" reports a proportion, "continuous" a mean
                   end_of_fup.type = "binary",
                   bootstrap = TRUE,
                   # fixes the bootstrap resamples, so the intervals below are
                   # reproducible; without it each run draws a fresh seed
                   seed = 1636,
                   bootstrap.nboot = 20)

model <- SEQuential(SEQdata, id.col = "ID",
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
#> Bootstrapping with 80% of 300 subjects (240 subjects, ~198,788 observations per resample) 20 times
#> 
#> Estimating end-of-follow-up outcome at follow-up time 12 
#> 
#> Completed

end_of_fup(model)
#> $`1`
#> $`1`$estimates
#>      Type  Time      A Proportion Trial-periods (Eligible)
#>    <char> <num> <fctr>      <num>                    <int>
#> 1: binary    12      0 0.01986475                     2946
#> 2: binary    12      1 0.02725648                     6257
#>    Trial-periods (Analysed) Trial-periods (Censored)
#>                       <int>                    <int>
#> 1:                     2366                      580
#> 2:                     4476                     1781
#>    Trial-periods (No measurement) % Censored Subjects          SE    95% LCI
#>                             <int>      <num>    <int>       <num>      <num>
#> 1:                              0   19.68771      266 0.003081317 0.01382548
#> 2:                              0   28.46412      236 0.001560669 0.02419762
#>       95% UCI
#>         <num>
#> 1: 0.02590402
#> 2: 0.03031533
#> 
#> $`1`$comparison
#> Key: <A_x, A_y>
#>     Time    A_x    A_y   Difference Difference 95% LCI Difference 95% UCI
#>    <num> <fctr> <fctr>        <num>              <num>              <num>
#> 1:    12      0      1  0.007391728      -5.362035e-05       1.483708e-02
#> 2:    12      1      0 -0.007391728      -1.483708e-02       5.362035e-05
#>    Difference SE     Ratio Ratio 95% LCI Ratio 95% UCI log(Ratio) SE
#>            <num>     <num>         <num>         <num>         <num>
#> 1:   0.003798717 1.3721028     0.9653172      1.950308      0.179413
#> 2:   0.003798717 0.7288084     0.5127395      1.035929      0.179413
```

[`end_of_fup()`](https://causalinference.github.io/SEQTaRget/reference/end_of_fup.md)
returns, per subgroup, two tables: `estimates` gives the weighted
proportion (or mean) in each arm with its bootstrap confidence interval,
and an account of how much of the arm it rests on.
`Trial-periods (Eligible)` is every trial-period that reached the
follow-up time, and the next three partition it: `(Analysed)` contribute
to the estimate, `(Censored)` were measured at some point but not within
the window, and `(No measurement)` were never measured at all - so the
three sum back to the eligible total. `% Censored` gives the censored
share of that total. `Subjects` counts the distinct people behind the
analysed trial-periods; one subject contributes several trial-periods
and can be analysed in some and censored in others. `comparison` gives
the pairwise between-arm contrast: the difference in proportions here,
or the difference in means for a continuous outcome, with its standard
error and confidence interval. For a binary outcome the ratio of
proportions is reported alongside it, with an interval computed on the
log scale and a `log(Ratio) SE` for inverse-variance pooling. Contrasts
are paired by bootstrap iteration, so their intervals account for the
correlation between arms.

## Missing measurements and the time window

Outcomes measured at particular visits are rarely available for everyone
at exactly time `k`. Encode “not measured at this time” as `NA` in the
outcome column - `end_of_fup` is the one mode that accepts missing
outcomes, precisely because missingness is meaningful here. Every other
column must still be complete.

`end_of_fup.window` sets the half-width of a window used when a
trial-period has no measurement at exactly `k`:

``` r

options <- SEQopts(end_of_fup = TRUE,
                   end_of_fup.time = 12,
                   # accept a measurement anywhere in [9, 15] when there is none at 12
                   end_of_fup.window = 3,
                   bootstrap = TRUE,
                   seed = 1636,
                   bootstrap.nboot = 20)

windowed <- SEQuential(SEQdata, id.col = "ID",
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
#> Bootstrapping with 80% of 300 subjects (240 subjects, ~198,788 observations per resample) 20 times
#> 
#> Estimating end-of-follow-up outcome at follow-up time 12 
#> 
#> Completed

end_of_fup(windowed)[[1]]$estimates
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
#> 1:                              0   14.35845      274 0.011306853 0.05354250
#> 2:                              0   21.86351      248 0.005941862 0.09082911
#>       95% UCI
#>         <num>
#> 1: 0.09786455
#> 2: 0.11412078
end_of_fup(windowed)[[1]]$comparison
#> Key: <A_x, A_y>
#>     Time    A_x    A_y  Difference Difference 95% LCI Difference 95% UCI
#>    <num> <fctr> <fctr>       <num>              <num>              <num>
#> 1:    12      0      1  0.02677142        -0.00118542         0.05472825
#> 2:    12      1      0 -0.02677142        -0.05472825         0.00118542
#>    Difference SE     Ratio Ratio 95% LCI Ratio 95% UCI log(Ratio) SE
#>            <num>     <num>         <num>         <num>         <num>
#> 1:    0.01426395 1.3536350     0.9693496      1.890265     0.1703723
#> 2:    0.01426395 0.7387516     0.5290263      1.031620     0.1703723
```

`comparison` is where the treatment effect lives: `Difference` is the
difference in proportions between the two arms, and `Ratio` their ratio.
Both directions of each arm pair are reported, so the row you want is
the one whose `A_x` is your reference arm.

The selection rule, applied to each trial-period independently, is:

1.  If there is a measurement at exactly `k`, use it.
2.  Otherwise, use the measurement *nearest* to `k` within
    `[k - window, k + window]`. Where two measurements are equally far
    either side of `k`, the *later* one is taken, so that at least `k`
    of follow-up has elapsed.
3.  If there is no measurement anywhere in the window, the trial-period
    is *censored* - it contributes nothing to the average.

The weight used is always the weight at the time the chosen measurement
was taken, not the weight at `k`.

A window is not free. Widening it recovers trial-periods that would
otherwise be dropped, but the measurements it recovers are taken further
from the time you actually care about, and the trial-periods it recovers
are not a random subset - a trial-period with no measurement at `k` is
often one whose follow-up ended early. Treat the window as a trade-off
between precision and how literally the estimate answers “the outcome at
time `k`”, and check how much of the estimate rests on it using the
accounting table below.

## Checking what contributed

[`diagnostics()`](https://causalinference.github.io/SEQTaRget/reference/diagnostics.md)
reports where every trial-period went. `eof.nonunique` counts
trial-periods and `eof.unique` counts distinct subjects:

``` r

diagnostics(windowed)$eof.nonunique
#> [[1]]
#> Key: <tx_init_bas>
#>    tx_init_bas Eligible  At k In window Excluded (outside window)
#>         <fctr>    <int> <int>     <int>                     <int>
#> 1:           0     2946  2366       157                       423
#> 2:           1     6257  4476       413                      1368
#>    Excluded (no measurement)
#>                        <int>
#> 1:                         0
#> 2:                         0
```

The four categories are mutually exclusive, so the trial-period counts
partition `Eligible`:

- *At k* - contributed, using a measurement at exactly `k`.
- *In window* - contributed, having fallen back to the window.
- *Excluded (outside window)* - measured at some point, but not within
  the window.
- *Excluded (no measurement)* - never measured at any follow-up time.
  Under `method = "censoring"` this also picks up trial-periods
  artificially censored before any measurement was taken.

`At k` plus `In window` is exactly the number of trial-periods behind
the estimate, so the two tables always reconcile. The subject counts in
`eof.unique` need *not* sum to `Eligible`, because one subject can fall
into different categories for different trials.

If `In window` is large relative to `At k`, or `Excluded` dominates, the
estimate is resting on much less - or much more indirect - data than the
arm totals alone suggest.

## Continuous outcomes

Set `end_of_fup.type = "continuous"` for an outcome that is not 0/1. The
estimate becomes a weighted mean, reported in a `Mean` column rather
than `Proportion`, and its confidence interval is not clamped to
`[0, 1]`. The between-arm contrast is the difference in means; no ratio
is reported, since a continuous outcome need not be bounded away from
zero.

``` r

data <- data.table::copy(SEQdata)
set.seed(42)
data[, biomarker := 10 + 2 * as.numeric(as.character(tx_init)) + N + rnorm(.N)]

continuous <- SEQuential(data, id.col = "ID",
                               time.col = "time",
                               eligible.col = "eligible",
                               treatment.col = "tx_init",
                               outcome.col = "biomarker",
                               time_varying.cols = c("N", "L", "P"),
                               fixed.cols = "sex",
                               method = "ITT",
                               options = SEQopts(end_of_fup = TRUE,
                                                 end_of_fup.time = 12,
                                                 end_of_fup.type = "continuous",
                                                 end_of_fup.window = 3,
                                                 bootstrap = TRUE,
                                                 seed = 1636,
                                                 bootstrap.nboot = 20))
#> 
#> Full dataset: 12,180 observations, 12 variables
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
#> Estimating end-of-follow-up outcome at follow-up time 12 
#> 
#> Completed

end_of_fup(continuous)[[1]]$estimates
#>          Type  Time      A     Mean Trial-periods (Eligible)
#>        <char> <num> <fctr>    <num>                    <int>
#> 1: continuous    12      0 21.68154                     2946
#> 2: continuous    12      1 21.73905                     6257
#>    Trial-periods (Analysed) Trial-periods (Censored)
#>                       <int>                    <int>
#> 1:                     2523                      423
#> 2:                     4889                     1368
#>    Trial-periods (No measurement) % Censored Subjects        SE  95% LCI
#>                             <int>      <num>    <int>     <num>    <num>
#> 1:                              0   14.35845      274 0.1169507 21.45233
#> 2:                              0   21.86351      248 0.1023680 21.53841
#>     95% UCI
#>       <num>
#> 1: 21.91076
#> 2: 21.93969
end_of_fup(continuous)[[1]]$comparison
#> Key: <A_x, A_y>
#>     Time    A_x    A_y  Difference Difference 95% LCI Difference 95% UCI
#>    <num> <fctr> <fctr>       <num>              <num>              <num>
#> 1:    12      0      1  0.05750665         -0.2283656          0.3433789
#> 2:    12      1      0 -0.05750665         -0.3433789          0.2283656
#>    Difference SE
#>            <num>
#> 1:     0.1458559
#> 2:     0.1458559
```

Here `Difference` is the difference in means, and there is no `Ratio`
column.

Note that the usual outcome diagnostic tables count `outcome == 1` rows,
which has no meaning for a continuous outcome, so they are reported as
`NA`. In their place
[`diagnostics()`](https://causalinference.github.io/SEQTaRget/reference/diagnostics.md)
reports the N, mean and SD of the raw analysed measurements per arm in
`eof.summary`; the follow-up and end-of-follow-up tables remain
available.

``` r

diagnostics(continuous)$eof.summary
#> [[1]]
#>         A     N     Mean       SD
#>    <fctr> <int>    <num>    <num>
#> 1:      0  2523 21.68154 5.082334
#> 2:      1  4889 21.73905 5.182840
```

## Per-protocol effects

`end_of_fup` composes with weighting in the usual way, so a per-protocol
end-of-follow-up effect is the censoring method with `weighted = TRUE`:

``` r

perprotocol <- SEQuential(SEQdata, id.col = "ID",
                                   time.col = "time",
                                   eligible.col = "eligible",
                                   treatment.col = "tx_init",
                                   outcome.col = "outcome",
                                   time_varying.cols = c("N", "L", "P"),
                                   fixed.cols = "sex",
                                   method = "censoring",
                                   options = SEQopts(weighted = TRUE,
                                                     numerator = "sex",
                                                     denominator = "N + L + P + sex",
                                                     end_of_fup = TRUE,
                                                     end_of_fup.time = 12,
                                                     end_of_fup.window = 3,
                                                     bootstrap = TRUE,
                                                     seed = 1636,
                                                     bootstrap.nboot = 20))
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
#> Bootstrapping with 80% of 300 subjects (240 subjects, ~82,199 observations per resample) 20 times
#> 
#> Estimating end-of-follow-up outcome at follow-up time 12 
#> 
#> Completed

end_of_fup(perprotocol)[[1]]$estimates
#>      Type  Time      A Proportion Trial-periods (Eligible)
#>    <char> <num> <fctr>      <num>                    <int>
#> 1: binary    12      0 0.04506949                     2946
#> 2: binary    12      1 0.08873317                     6257
#>    Trial-periods (Analysed) Trial-periods (Censored)
#>                       <int>                    <int>
#> 1:                      502                     2444
#> 2:                     3154                     3103
#>    Trial-periods (No measurement) % Censored Subjects          SE      95% LCI
#>                             <int>      <num>    <int>       <num>        <num>
#> 1:                              0   82.95995       86 0.022835744 0.0003122527
#> 2:                              0   49.59246      211 0.008721064 0.0716402018
#>       95% UCI
#>         <num>
#> 1: 0.08982673
#> 2: 0.10582614
end_of_fup(perprotocol)[[1]]$comparison
#> Key: <A_x, A_y>
#>     Time    A_x    A_y  Difference Difference 95% LCI Difference 95% UCI
#>    <num> <fctr> <fctr>       <num>              <num>              <num>
#> 1:    12      0      1  0.04366368       -0.002914227        0.090241593
#> 2:    12      1      0 -0.04366368       -0.090241593        0.002914227
#>    Difference SE     Ratio Ratio 95% LCI Ratio 95% UCI log(Ratio) SE
#>            <num>     <num>         <num>         <num>         <num>
#> 1:    0.02376468 1.9688080     0.6334967      6.118746     0.5785458
#> 2:    0.02376468 0.5079215     0.1634322      1.578540     0.5785458
```

Subjects who deviate from their assigned strategy are artificially
censored at the point of deviation, and their outcome is missing from
then on. A trial-period that deviates before `k` therefore has no
measurement to contribute and is excluded rather than carried forward -
it appears under `Excluded (no measurement)` or
`Excluded (outside window)` in the accounting table, depending on what
it had measured earlier.
