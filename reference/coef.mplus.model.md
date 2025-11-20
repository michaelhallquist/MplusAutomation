# Return coefficients for an mplus.model object

This is a method for returning the coefficients of an mplus.model
object. It works directly on an object stored from `readModels` such as:
`object <- readModels("/path/to/model/model.out")`.

Method that calls `coef.mplus.model`. See further documentation there.

## Usage

``` r
# S3 method for class 'mplus.model'
coef(
  object,
  type = c("un", "std", "stdy", "stdyx"),
  params = c("regression", "loading", "undirected", "expectation", "variability", "new"),
  ...,
  raw = FALSE
)

# S3 method for class 'mplusObject'
coef(object, ...)
```

## Arguments

- object:

  An object of class mplusObject

- type:

  A character vector indicating the type of coefficients to return. One
  of “un”, “std”, “stdy”, or “stdyx”.

- params:

  A character vector indicating what type of parameters to extract. Any
  combination of “regression”, “loading”, “undirected”, “expectation”,
  “variability”, and “new”. A single one can be passed or multiple. By
  default, all are used and all parameters are returned.

- ...:

  Additional arguments to pass on (not currently used)

- raw:

  A logical defaulting to `FALSE` indicating whether to parse and return
  coefficients based on the type (regression, etc.) and relabel using an
  arrow notation, or to return the raw coefficients in a named vector.

## Value

Either a data frame of class ‘mplus.model.coefs’, or in the case of
multiple group models, a list of class ‘mplus.model.coefs’, where each
element of the list is a data frame of class ‘mplus.model.coefs’, or a
named vector of coefficients, if `raw=TRUE`.

## See also

[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)

Other Mplus-Formatting:
[`confint.mplus.model()`](https://michaelhallquist.github.io/MplusAutomation/reference/confint.mplus.model.md),
[`extract()`](https://michaelhallquist.github.io/MplusAutomation/reference/extract.md),
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

# example of the coef method on an mplud.model object
# note that res$results holds the results of readModels()
coef(res$results)
coef(res$results, type = "std")
coef(res$results, type = "stdy")
coef(res$results, type = "stdyx")

# there is also a method for mplusObject class
coef(res)

# remove files
unlink("mtcars.dat")
unlink("model1.inp")
unlink("model1.out")
unlink("Mplus Run Models.log")
} # }
```
