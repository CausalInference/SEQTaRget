# Extract underlying column names from RHS formula strings

Wraps `all.vars(as.formula(...))` so that function-wrapped terms in
formula strings (e.g. `ns(followup, df = 4)`, `I(x^2)`, `factor(grp)`)
resolve to their underlying variable names rather than being treated as
raw column names.

## Usage

``` r
formula_vars(x)
```

## Arguments

- x:

  character vector of RHS formula strings (may contain `+`, `*`, `:`,
  and function wrappers). NA / empty entries are ignored.

## Value

character vector of unique variable names referenced by `x`.
