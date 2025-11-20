# Create Mplus syntax for variable names

This is a simple function designed to take a dataset in `R` and
translate it into a set of variable names for Mplus.

## Usage

``` r
createVarSyntax(data)
```

## Arguments

- data:

  An `R` dataset.

## Value

A character vector of the variable names for Mplus

## See also

[`prepareMplusData`](https://michaelhallquist.github.io/MplusAutomation/reference/prepareMplusData.md)

## Examples

``` r
MplusAutomation:::createVarSyntax(mtcars)
#> [1] "NAMES = mpg cyl disp hp drat wt qsec vs am gear carb; \n"
```
