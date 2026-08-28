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

- `eof.data`:

  end-of-follow-up estimates per treatment arm when `end_of_fup = TRUE`,
  empty otherwise. See
  [`end_of_fup()`](https://causalinference.github.io/SEQTaRget/reference/end_of_fup.md)

- `eof.comparison`:

  pairwise between-arm contrasts of the end-of-follow-up estimates when
  `end_of_fup = TRUE`, empty otherwise

- `weight.statistics`:

  information from the weighting process, containing weight coefficients
  and weight statistics

- `info`:

  list of diagnostic tables (outcome, follow-up, switch, and
  competing-event counts where applicable), each split by baseline
  treatment arm. The "unique" tables count distinct subjects; the
  "non-unique" tables count rows: total outcome events for the outcome
  tables, and total person-time intervals for the follow-up tables. The
  outcome tables are `NA` for a continuous end-of-follow-up outcome,
  which has no events to count. See
  [`diagnostics()`](https://causalinference.github.io/SEQTaRget/reference/diagnostics.md).

- `ce.model`:

  list of competing event models if `compevent` is specified, NA
  otherwise
