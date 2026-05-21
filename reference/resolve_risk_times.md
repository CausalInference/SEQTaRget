# Resolve the follow-up times at which to report risk difference / ratio

Each requested time in `risk.times` is snapped to the latest available
follow-up value at or before it (cumulative incidence is a
right-continuous step function). The maximum follow-up time is always
included. A `NA` `risk.times` reports only the maximum follow-up time.

## Usage

``` r
resolve_risk_times(grid, risk.times)
```

## Arguments

- grid:

  numeric vector of available follow-up values

- risk.times:

  numeric vector of requested follow-up times, or `NA`

## Value

sorted unique numeric vector of grid follow-up values to report at
