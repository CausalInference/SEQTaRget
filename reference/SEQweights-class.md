# An internal S4 class to help transfer weight statistics out of `internal_weights`

An internal S4 class to help transfer weight statistics out of
`internal_weights`

## Slots

- `weights`:

  a data.table containing the estimated weights, either pre or post
  expansion

- `coef.n0`:

  numerator zero model

- `coef.n1`:

  numerator one model

- `coef.d0`:

  denominator zero model

- `coef.d1`:

  denominator one model

- `coef.ncense`:

  numerator censoring model

- `coef.dcense`:

  denominator censoring model
