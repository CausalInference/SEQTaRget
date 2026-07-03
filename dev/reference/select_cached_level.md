# Select the cached weight-model formula for a treatment level

A cache entry made by
[`init_formula_cache()`](https://causalinference.github.io/SEQTaRget/dev/reference/init_formula_cache.md)
is either a single parsed formula (a list with a `$formula` element,
shared across treatment levels) or an unnamed list of parsed formulas,
one per `params@treat.level`. Returns the entry for `level`, or the
shared entry (or `NULL`) unchanged.

## Usage

``` r
select_cached_level(cached, level, params)
```
