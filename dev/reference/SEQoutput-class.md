# An S4 class used to hold the outputs for the SEQuential process

An S4 class used to hold the outputs for the SEQuential process

## Slots

- `params`:

  SEQparams object

- `outcome`:

  outcome covariates

- `numerator`:

  numerator covariates

- `denominator`:

  denominator covariates

- `outcome.model`:

  list of length `bootstrap.nboot` containing outcome coefficients

- `hazard`:

  hazard ratio

- `survival.curve`:

  ggplot object for the survival curves

- `survival.data`:

  data.table of survival data

- `risk.difference`:

  risk difference calculated from survival data

- `risk.ratio`:

  risk ratio calculated from survival data

- `time`:

  time used for the SEQuential process

- `weight.statistics`:

  information from the weighting process, containing weight coefficients
  and weight statistics

- `info`:

  list of diagnostic tables (outcome, follow-up, switch, and
  competing-event counts where applicable), each split by baseline
  treatment arm. The "unique" tables count distinct subjects; the
  "non-unique" tables count rows: total outcome events for the outcome
  tables, and total person-time intervals for the follow-up tables. See
  [`diagnostics()`](https://causalinference.github.io/SEQTaRget/dev/reference/diagnostics.md).

- `ce.model`:

  list of competing event models if `compevent` is specified, NA
  otherwise
