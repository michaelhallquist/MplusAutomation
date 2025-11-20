# Create latent profile plots

Creates a profile plot for a single object of class 'mplus.model', or a
faceted plot of profile plots for an object of class 'mplus.model.list'.

## Usage

``` r
plotMixtures(
  modelList,
  variables = NULL,
  coefficients = c("unstandardized", "stdyx.standardized", "stdy.standardized",
    "stdy.standardized"),
  parameter = c("Means", "Intercepts"),
  ci = 0.95,
  bw = FALSE,
  rawdata = FALSE,
  alpha_range = c(0, 0.1)
)
```

## Arguments

- modelList:

  A list of Mplus mixture models, or a single mixture model

- variables:

  A character vectors with the names of the variables (included in the
  Mplus output) to be plotted.

- coefficients:

  Which type of coefficients to plot on the y-axis; default is
  'unstandardized'. Options include:
  `c('stdyx.standardized', 'stdy.standardized', 'std.standardized')`

- parameter:

  Which parameter to plot (from Mplus parameter estimate headings
  included in the output). Defaults to `c('Means', 'Intercepts')`.

- ci:

  What confidence interval should the errorbars span? Defaults to a 95%
  confidence interval. Set to NULL to remove errorbars.

- bw:

  Logical. Should the plot be black and white (for print), or color?

- rawdata:

  Should raw data be plotted in the background? Setting this to TRUE
  might result in long plotting times. Requires including the Mplus
  syntax 'SAVEDATA: FILE IS "filename"; SAVE = cprobabilities' in the
  Mplus input.

- alpha_range:

  The minimum and maximum values of alpha (transparancy) for the raw
  data. Minimum should be 0; lower maximum values of alpha can help
  reduce overplotting.

## Value

An object of class 'ggplot'.

## Author

Caspar J. van Lissa

## Examples

``` r
if (FALSE) { # \dontrun{
res <- createMixtures(classes = 1:2, filename_stem = "cars",
                      model_overall = "wt ON drat;",
                      model_class_specific = "wt;  qsec;",
                      rdata = mtcars,
                      usevariables = c("wt", "qsec", "drat"),
                      OUTPUT = "standardized",
                      run = 1L)
plotMixtures(res, rawdata = TRUE)
} # }
if (FALSE) { # \dontrun{
plotMixtures(res, variables = "wt")
} # }
if (FALSE) { # \dontrun{
plotMixtures(res, coefficients = "stdyx.standardized")
} # }
```
