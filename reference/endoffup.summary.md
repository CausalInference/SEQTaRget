# Per-arm summary of the analysed end-of-follow-up measurements

N, mean and SD of the raw selected measurements per baseline arm - the
unweighted analogue of the outcome count tables, reported for continuous
outcomes where event counts have no meaning.

## Usage

``` r
endoffup.summary(DT, params)
```

## Arguments

- DT:

  expanded data.table, weighted or not - as passed to
  [`endoffup.estimate()`](https://causalinference.github.io/SEQTaRget/reference/endoffup.estimate.md)

- params:

  SEQparams object

## Value

named list of data.tables, one element per subgroup
