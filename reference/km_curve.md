# Function to print kaplan-meier curves

Function to print kaplan-meier curves

## Usage

``` r
km_curve(
  object,
  plot.type = "survival",
  plot.title,
  plot.subtitle,
  plot.labels,
  plot.colors
)
```

## Arguments

- object:

  SEQoutput object to plot

- plot.type:

  character: type of plot to print

- plot.title:

  character: defines the title of the plot

- plot.subtitle:

  character: plot subtitle

- plot.labels:

  length 2 character: plot labels

- plot.colors:

  length 2 character: plot colors

## Value

ggplot object of plot `plot.type`
