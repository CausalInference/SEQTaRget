# Simulated observational example data for SEQuential

Simulated observational example data for SEQuential

## Usage

``` r
SEQdata
```

## Format

A data frame with 54,687 rows and 13 columns:

- ID:

  Integer: Unique ID emulating individual patients

- time:

  Integer: Time of observation, always begins at 0, max time of 59.
  Should be continuous

- eligible:

  Binary: eligibility criteria for timepoints

- outcome:

  Binary: If an outcome is observed at this time point

- tx_init:

  Binary: If treatment is observed at this time point

- sex:

  Binary: Sex of the emulated patient

- N:

  Numeric: Normal random variable from N\\10,5\\

- L:

  Numeric: 4% continuously increase from U\\0, 1\\

- P:

  Numeric: 2% continuously decrease from U\\9, 10\\

- excusedOne:

  Binary: Once one, always one variable emulating an excuse for
  treatment switch

- excusedZero:

  Binary: Once zero, always zero variable emulating an excuse for
  treatment switch
