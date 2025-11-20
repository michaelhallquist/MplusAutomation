# Separate Hyphenated Variable Strings

This code is a simplified form of `expandCmd` from the lavaan package.
It separates hyphenated variable strings into a list of vectors, while
ignoring hyphens that may be used in numbers.

## Usage

``` r
separateHyphens(cmd)
```

## Arguments

- cmd:

  A character string

## Value

The character string if no hyphens, or a list of vectors if there are
hyphens.

## Details

Note that this is an internal function only.

## Author

Michael Hallquist revised by Joshua Wiley

## Examples

``` r
MplusAutomation:::separateHyphens("x1x4")
#> [1] "x1x4"
MplusAutomation:::separateHyphens("x1-x4")
#> [[1]]
#> [1] "x1" "x4"
#> 
MplusAutomation:::separateHyphens("x1-x4; x1*-1; v1-v3;")
#> [[1]]
#> [1] "x1" "x4"
#> 
#> [[2]]
#> [1] "v1" "v3"
#> 
```
