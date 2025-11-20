# Automatically detect variables from an Mplus model object

This is a function to automatically detect the variables used in an
Mplus model object.

## Usage

``` r
detectVariables(object, quiet = TRUE)
```

## Arguments

- object:

  An Mplus model object from `mplusObject`.#'

- quiet:

  optional. If `TRUE`, show status messages in the console.

## Value

A vector of variables from the R dataset to use.

## See also

[`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.md),
[`mplusObject`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusObject.md)

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
example1 <- mplusObject(MODEL = "mpg ON wt;",
  rdata = mtcars, autov = FALSE)
example1$usevariables
#> NULL
MplusAutomation:::detectVariables(example1)
#> [1] "mpg" "wt" 

example2 <- mplusObject(MODEL = "mpg ON wt;",
  rdata = mtcars, autov = TRUE)
example2$usevariables
#> [1] "mpg" "wt" 
example3 <- update(example2,
  MODEL = ~ . + "mpg ON qsec; wt WITH qsec;",
  autov = TRUE)
example3$usevariables
#> [1] "mpg" "wt" 
rm(example1, example2, example3)
```
