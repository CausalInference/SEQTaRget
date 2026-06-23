# Function to return risk information from a SEQuential object

Function to return risk information from a SEQuential object

## Usage

``` r
risk_comparison(object)
```

## Arguments

- object:

  SEQoutput object

## Value

A data frame of risk comparison information at the reported follow-up
time(s): risk ratios and risk differences, and when bootstrapped their
confidence intervals and standard errors. The bootstrap standard errors
are reported regardless of `bootstrap.CI_method`: `RD SE` is the
standard error of the risk difference (natural scale) and `log(RR) SE`
is the standard error of the log risk ratio. For an
inverse-variance-weighted meta-analysis across samples, pool
`Risk Difference` with `RD SE`, and `log(`Risk Ratio`)` with
`log(RR) SE` (then exponentiate the pooled ratio).
