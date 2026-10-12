# One row per trial-period that can reach the end-of-follow-up window

Flags what each trial-period has measured. Trial-periods starting too
late to reach `k - window` before the data end cannot be measured in the
window, so they are left out rather than counted as censored.

## Usage

``` r
endoffup.periods(DT, params)
```
