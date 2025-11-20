# Extract function to make Mplus output work with the texreg package

This is a method for extracting output in a format suitable for the
texreg package. Uses `coef` for most the work.

## Usage

``` r
extract.mplus.model(
  model,
  summaries = "none",
  cis = FALSE,
  escape.latex = FALSE,
  ...
)

extract.mplusObject(model, summaries = "none", cis = FALSE, ...)

# S4 method for class 'mplus.model'
extract(model, summaries = "none", cis = FALSE, escape.latex = FALSE, ...)

# S4 method for class 'mplusObject'
extract(model, summaries = "none", cis = FALSE, ...)
```

## Arguments

- model:

  An Mplus model object. This typically comes either from
  [`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)
  directly, or indirectly via
  [`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.md).
  The results will have different classes, but extract methods are
  defined for both.

- summaries:

  A character vector which summaries to include. Defaults to “none”.

- cis:

  A logical whether to extract confidence intervals.

- escape.latex:

  A logical value whether to escape dollar signs in coefficient names
  for LaTeX. Defaults to `FALSE`.

- ...:

  Additional arguments passed to
  [`coef.mplus.model`](https://michaelhallquist.github.io/MplusAutomation/reference/coef.mplus.model.md).

## Value

A `texreg` object, or for multiple group models, a list of `texreg`
objects.

## See also

[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)

Other Mplus-Formatting:
[`coef.mplus.model()`](https://michaelhallquist.github.io/MplusAutomation/reference/coef.mplus.model.md),
[`confint.mplus.model()`](https://michaelhallquist.github.io/MplusAutomation/reference/confint.mplus.model.md),
[`print.MplusRstructure()`](https://michaelhallquist.github.io/MplusAutomation/reference/print.MplusRstructure.md),
[`summary.mplusObject()`](https://michaelhallquist.github.io/MplusAutomation/reference/summary.mplusObject.md)

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
# simple example of a model using builtin data
# demonstrates use
test <- mplusObject(
  TITLE = "test the MplusAutomation Package;",
  MODEL = "
    mpg ON wt hp;
    wt WITH hp;",
  OUTPUT = "STANDARDIZED;",
  usevariables = c("mpg", "wt", "hp"),
  rdata = mtcars)

res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)

extract(res$results)
# there is also a method for mplusObject class
extract(res)

# load the texreg package
# to use pretty printing via screenreg
# uncomment to run these examples
# library(texreg)
# screenreg(res)
# screenreg(res, type = 'stdyx')

# screenreg(res, type = 'un', params = 'regression',
#   single.row=TRUE)
# screenreg(res, type = 'un', params = 'regression', summaries = 'CFI',
#   single.row=TRUE)

# remove files
unlink("mtcars.dat")
unlink("model1.inp")
unlink("model1.out")
unlink("Mplus Run Models.log")
} # }
```
