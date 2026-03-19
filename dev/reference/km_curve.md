# Function to print Kaplan-Meier curves

Function to print Kaplan-Meier curves

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

  character: type of plot to print; one of: `"survival"` (default),
  `"risk"`, `"inc"`

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
