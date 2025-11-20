# Extract parameters from a data frame of Mplus estimates

This is a simple convenience function designed to facilitate looking at
specific parameter types by easily return a subset of a data frame with
those types only. It is designed to follow up the results returned from
the
[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)
function.

## Usage

``` r
paramExtract(
  x,
  params = c("regression", "loading", "undirected", "expectation", "variability", "new")
)
```

## Arguments

- x:

  A data frame (specifically the type returned by `readModels`)
  containing parameters. Should be specific such as unstandardized and
  the data frame must have a column called ‘paramHeader’.

- params:

  A character string indicating the types of parameters to be returned.
  Options currently include ‘regression’, ‘loading’, ‘undirected’,
  ‘expectation’, ‘variability’, and ‘new’ for new/additional parameters.
  Regressions include regression of one variable `ON` another. ‘loading’
  include indicator variables (which are assumed caused by the
  underlying latent variable) and variables in latent growth models
  (`BY` or `|`). Undirected paths currently only include covariances,
  indicated by the `WITH` syntax in Mplus. Expectation paths are the
  unconditional or conditional expectations of variables. In other words
  those parameters related to the first moments. For independent
  variables, these are the means, \\E(X)\\ and the conditional means or
  intercepts, \\E(X \| f(\theta))\\ where \\f(\theta)\\ is the model,
  some function of the parameters, \\\theta\\. Finally ‘variability’
  refers to both variances and residual variances, corresponding to the
  second moments. As with the expectations, variances are unconditional
  for variables that are not predicted or conditioned on any other
  variable in the model whereas residual variances are conditional on
  the model. Note that R uses fuzzy matching so that each of these can
  be called via shorthand, ‘r’, ‘l’, ‘u’, ‘e’, and ‘v’.

## Value

A subset data frame with the parameters of interest.

## See also

[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
  test <- mplusObject(
    TITLE = "test the MplusAutomation Package and my Wrapper;",
    MODEL = "
      mpg ON wt hp;
      wt WITH hp;",
    usevariables = c("mpg", "wt", "hp"),
    rdata = mtcars)

  res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)

  # store just the unstandardized parameters in 'd'
  d <- res$results$parameters$unstandardized
  # extract just regression parameters
  paramExtract(d, "regression")
  # extract other types of parameters using shorthand
  paramExtract(d, "u")
  paramExtract(d, "e")
  paramExtract(d, "v")
} # }
```
