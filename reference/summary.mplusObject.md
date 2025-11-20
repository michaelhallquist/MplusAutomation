# Summarize an mplusObject

This is a method for summarizing an mplusObject.

## Usage

``` r
# S3 method for class 'mplusObject'
summary(object, verbose = FALSE, ...)
```

## Arguments

- object:

  An object of class mplusObject

- verbose:

  Logical whether to print verbose output. Defaults to `FALSE`.

- ...:

  Additional arguments to pass on (not currently used)

## Value

`NULL` Called for its side effect of printing a model summary to the
console

## See also

Other Mplus-Formatting:
[`coef.mplus.model()`](https://michaelhallquist.github.io/MplusAutomation/reference/coef.mplus.model.md),
[`confint.mplus.model()`](https://michaelhallquist.github.io/MplusAutomation/reference/confint.mplus.model.md),
[`extract()`](https://michaelhallquist.github.io/MplusAutomation/reference/extract.md),
[`print.MplusRstructure()`](https://michaelhallquist.github.io/MplusAutomation/reference/print.MplusRstructure.md)

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
  usevariables = c("mpg", "wt", "hp"),
  rdata = mtcars)

 res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)

# example of the summary method
summary(res)

# example of verbose output
summary(res, verbose=TRUE)

# remove files
unlink("mtcars.dat")
unlink("model1.inp")
unlink("model1.out")
unlink("Mplus Run Models.log")
} # }
```
