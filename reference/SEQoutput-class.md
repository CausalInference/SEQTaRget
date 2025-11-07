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

  time in minutes used for the SEQuential process

- `weight.statistics`:

  information from the weighting process, containing weight coefficients
  and weight statistics

- `info`:

  list of outcome and switch information (if applicable)

- `ce.model`:

  list of competing event models if `compevent` is specified, NA
  otherwise
