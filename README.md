# SEQuential

A package to estimate the observational analogs of the intention-to-treat and per-protocol effects of hypothetical treatment strategies. Built from the [INITIATORS SAS macro](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3613145/) built by Roger Logan, Goodarz Danaei, and Miguel Hernan, this tool is designed to analyze observational longitudinal data to estimate the effect of interventions sustained over time. The premise is to emulate the design and analysis of a hypothetical randomized trial. This software is capable of conducting the obervational analogs of intention-to-treat, per-protocol, and as-treated analyses. All analyses are conducted using pooled logistic regression to approximate the hazard ratio from a proportional hazard Cox model.

## Setting up your Analysis

The function, `SEQuential`, requires the user to supply `data`, and column names for `id`, `eligible`, `time`, and `outcome`, along with the `method` of analysis. For changing default parameter values, you can either pass a list to `params` or arguments to `...`. `SEQuential` has a large parameter space; to break up documentation into more digestible chunks, we have broken the parameters into helper functions. Information on all the parameters and defaults is available through `?SEQopts`. When changing parameters from their defaults, building a parameter list to pass to `params` is functionally the same as passing the arguments to `...`. In the case of conflict of arguments supplied, the function will return an error.

```         
# Using the parameter builder
myparams <- SEQopts(max = 23, nboot = 20)
SEQuential(data, "ID", "eligible", "time", "tx_start", method = "ITT", params = myparams)

# Is functionally the same as
expanded_data <- SEQexpand(data)
SEQuential(expanded_data, "ID", "eligible", "time", "tx_start", method = "ITT", max = 23, nboot = 20)
```

## Using SEQuential

`SEQuential` and `SEQExpand` are the two primary functions of this package. When the user supplies data to `SEQuential` without the attribute `SEQexpanded` it assumes that the data requires expansion and runs `SEQexpand` internally.

## Output
At present moment, `SEQuential` returns a `speedglm` object
