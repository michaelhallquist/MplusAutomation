# Functions to identify the operating system

Functions to identify the operating system

## Usage

``` r
is.windows()

is.macos()

is.linux()

os()
```

## Value

A logical value for the is.\* functions and a character string for os()

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
MplusAutomation:::is.windows()
#> [1] FALSE

MplusAutomation:::is.macos()
#> [1] FALSE

MplusAutomation:::is.linux()
#> [1] TRUE

MplusAutomation:::os()
#> [1] "linux"
```
