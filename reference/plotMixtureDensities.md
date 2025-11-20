# Create density plots for mixture models

Creates a density plot for a single object of class 'mplus.model', or a
faceted plot of density plots for an object of class 'mplus.model.list'.
For each variable, a Total density plot will be shown, along with
separate density plots for each latent class, where cases are weighted
by the posterior probability of being assigned to that class.

## Usage

``` r
plotMixtureDensities(
  modelList,
  variables = NULL,
  bw = FALSE,
  conditional = FALSE,
  alpha = 0.2,
  facet_labels = NULL
)
```

## Arguments

- modelList:

  A list object of Mplus models, or a single Mplus model

- variables:

  Which variables to plot. If NULL, plots all variables that are present
  in all Mplus models.

- bw:

  Logical. Whether to make a black and white plot (for print) or a color
  plot. Defaults to FALSE, because these density plots are hard to read
  in black and white.

- conditional:

  Logical. Whether to show a conditional density plot (surface area is
  divided amongst the latent classes), or a classic density plot
  (surface area of the total density plot is equal to one, and is
  subdivided amongst the classes).

- alpha:

  Numeric (0-1). Only used when bw and conditional are FALSE. Sets the
  transparency of geom_density, so that classes with a small number of
  cases remain visible.

- facet_labels:

  Named character vector, the names of which should correspond to the
  facet labels one wishes to rename, and the values of which provide new
  names for these facets. For example, to rename variables, in the
  example with the 'iris' data below, one could specify:
  `facet_labels = c("Pet_leng" = "Petal length")`.

## Value

An object of class 'ggplot'.

## Note

This function returns warnings, indicating that sum(weights) != 1. These
can be ignored. The sum of the "Total" density per variable per model is
equal to 1, and the sum of all of the posterior probabilities is equal
to

1.  This results in a normal density plot for the "Total", which is
    subdivided by the latent classes, in proportion to the posterior
    probabilities of participants being assigned to those clases.

## Author

Caspar J. van Lissa

## Examples

``` r
if (FALSE) { # \dontrun{
results <- createMixtures(classes = 1:3, filename_stem = "iris",
                          rdata = iris, run = 1L)
plotMixtureDensities(results)
} # }
if (FALSE) { # \dontrun{
plotMixtureDensities(results, variables = "PETAL_LE")
} # }
if (FALSE) { # \dontrun{
plotMixtureDensities(results, bw = TRUE)
} # }
if (FALSE) { # \dontrun{
plotMixtureDensities(results, bw = FALSE, conditional = TRUE)
} # }
if (FALSE) { # \dontrun{
plotMixtureDensities(results[[2]], variables = "PETAL_LE")
} # }
```
