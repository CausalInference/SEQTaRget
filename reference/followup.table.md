# Count follow-up per treatment arm

Summarises follow-up in each treatment arm, grouped by the baseline
treatment value, restricted to expanded-data rows with an observed
(non-`NA`) outcome - the person-time the outcome/survival model is fit
on. Rows censored under `method = "censoring"` (which carry
`outcome = NA`) are excluded, matching the analysis.

## Usage

``` r
followup.table(params, type, filter = NA)
```

## Arguments

- params:

  SEQparams object (uses the expanded `params@DT`)

- type:

  either `"unique"` (distinct subjects) or `"nonunique"` (intervals)

- filter:

  subgroup value to restrict to, or `NA` for no subgroup

## Value

data.table with the baseline-treatment column and `n` (count)

## Details

`type = "nonunique"` counts follow-up intervals (rows), i.e. total
person-time; dividing the non-unique outcome counts by these gives the
per-arm event rate. `type = "unique"` counts the distinct subjects
contributing follow-up to the arm (a subject contributing several trials
to the same arm is counted once; a subject who appears in both arms is
counted in each).
