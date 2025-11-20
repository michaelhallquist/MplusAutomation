# Print an Mplus Residual Structure object

This is a method for printing an Mplus Residual Structure object.

## Usage

``` r
# S3 method for class 'MplusRstructure'
print(x, ...)
```

## Arguments

- x:

  An object of class MplusRstructure

- ...:

  Additional arguments to pass on (not currently used)

## Value

`NULL` Called for its side effect of printing the object to the console

## See also

Other Mplus-Formatting:
[`coef.mplus.model()`](https://michaelhallquist.github.io/MplusAutomation/reference/coef.mplus.model.md),
[`confint.mplus.model()`](https://michaelhallquist.github.io/MplusAutomation/reference/confint.mplus.model.md),
[`extract()`](https://michaelhallquist.github.io/MplusAutomation/reference/extract.md),
[`summary.mplusObject()`](https://michaelhallquist.github.io/MplusAutomation/reference/summary.mplusObject.md)

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
# default 'show' uses printing
mplusRcov(c("a", "b", "c"), type = "ar")
#> a b c (e);
#> a WITH b (rho);
#> b WITH c (rho);
#> a WITH c (rho2);
#> MODEL CONSTRAINT: 
#>   rho2 = ((rho/e)^2) * e;
#> 

# also if calling print explicitly
print(mplusRcov(c("a", "b", "c"), type = "ar"))
#> a b c (e);
#> a WITH b (rho);
#> b WITH c (rho);
#> a WITH c (rho2);
#> MODEL CONSTRAINT: 
#>   rho2 = ((rho/e)^2) * e;
#> 

# to see all aspects of the raw/original object
str(mplusRcov(c("a", "b", "c"), type = "ar"))
#> List of 4
#>  $ all        : chr "a b c (e);\na WITH b (rho);\nb WITH c (rho);\na WITH c (rho2);\nMODEL CONSTRAINT: \n  rho2 = ((rho/e)^2) * e;\n"
#>  $ Variances  : chr "a b c (e);"
#>  $ Covariances: chr "a WITH b (rho);\nb WITH c (rho);\na WITH c (rho2);"
#>  $ Constraints: chr "MODEL CONSTRAINT: \n  rho2 = ((rho/e)^2) * e;\n"
#>  - attr(*, "class")= chr "MplusRstructure"
```
