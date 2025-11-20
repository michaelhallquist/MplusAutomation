# Plot coefficients for an mplusObject

This is a method for plotting the coefficients of an mplusObject.

## Usage

``` r
# S3 method for class 'mplusObject'
plot(x, y, type = c("stdyx", "un", "std", "stdy"), ...)
```

## Arguments

- x:

  An object of class mplusObject

- y:

  Not currently used

- type:

  A character vector indicating the type of coefficients to return. One
  of “un”, “std”, “stdy”, or “stdyx”. Defaults to “stdyx”.

- ...:

  Additional arguments to pass on (not currently used)

## Value

Nothing. Called for its side effect of plotting the coefficients.

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

# example of the coef method
plot(res)

# remove files
unlink("mtcars.dat")
unlink("model1.inp")
unlink("model1.out")
unlink("Mplus Run Models.log")
} # }
```
