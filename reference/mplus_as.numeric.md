# Convert Mplus Number to Numeric

Helper function to convert strings formatted in Mplus Fortran-style
scientific notation using D to indicate double.

## Usage

``` r
mplus_as.numeric(vec, expect_sig = FALSE)
```

## Arguments

- vec:

  A character vector of Mplus numbers to convert to numeric

- expect_sig:

  Whether to expect significance values denoted by asterisk; yields a
  'sig' attribute that will be TRUE/FALSE

## Value

A numeric vector

## Examples

``` r
MplusAutomation:::mplus_as.numeric("3.1D2")
#> [1] 310
```
