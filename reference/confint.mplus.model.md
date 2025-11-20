# Return confidence intervals for an mplus.model object

This is a method for returning the confidence of an mplus.model object.
It works directly on an object stored from `readModels` such as:
`object <- readModels("/path/to/model/model.out")`.

Method that calls `confint.mplus.model`. See further documentation
there.

## Usage

``` r
# S3 method for class 'mplus.model'
confint(
  object,
  parm,
  level = 0.95,
  type = c("un", "std", "stdy", "stdyx"),
  params = c("regression", "loading", "undirected", "expectation", "variability", "new"),
  ...
)

# S3 method for class 'mplusObject'
confint(object, ...)
```

## Arguments

- object:

  An object of class mplusObject

- parm:

  Included as all [`confint()`](https://rdrr.io/r/stats/confint.html)
  methods must include it. Not used currently for Mplus.

- level:

  A numeric vector indicating the level of confidence interval to
  extract. Options are .95, .90, or .99 as those are all Mplus provides.

- type:

  A character vector indicating the type of confidence intervals to
  return. One of “un”, “std”, “stdy”, or “stdyx”.

- params:

  A character vector indicating what type of parameters to extract. Any
  combination of “regression”, “loading”, “undirected”, “expectation”,
  “variability”, and “new”. A single one can be passed or multiple. By
  default, all are used and all parameters are returned.

- ...:

  Additional arguments to pass on (not currently used)

## Value

A data frame of class ‘mplus.model.cis’, or in the case of multiple
group models, a list of class ‘mplus.model.cis’, where each element of
the list is a data frame of class ‘mplus.model.cis’.

## See also

[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)

Other Mplus-Formatting:
[`coef.mplus.model()`](https://michaelhallquist.github.io/MplusAutomation/reference/coef.mplus.model.md),
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
  OUTPUT = "STANDARDIZED; CINTERVAL;",
  usevariables = c("mpg", "wt", "hp"),
  rdata = mtcars)

res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)

# example of the confint method on an mplus.model object
# note that res$results holds the results of readModels()
confint(res$results)
confint(res$results, type = "std")
confint(res$results, type = "stdy")
confint(res$results, type = "stdyx", level = .99)

# there is also a method for mplusObject class
confint(res)
screenreg(res, cis = TRUE, single.row = TRUE)

# remove files
unlink("mtcars.dat")
unlink("model1.inp")
unlink("model1.out")
unlink("Mplus Run Models.log")
} # }
```
