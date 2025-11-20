# Trim White Space

Helper function to remove white space from a character vector

## Usage

``` r
trimSpace(string)
```

## Arguments

- string:

  The character vector to trim white space from.

## Value

A character vector with the white space removed.

## Examples

``` r
MplusAutomation:::trimSpace(c("    test", "another    "))
#> [1] "test"    "another"
```
