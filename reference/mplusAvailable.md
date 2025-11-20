# Check whether Mplus can be found

This is a simple utility to check whether Mplus can be found. Returns 0
if Mplus command can be found by the system. If `silent = FALSE`, prints
a message to the user to help suggest what to do.

## Usage

``` r
mplusAvailable(silent = TRUE)
```

## Arguments

- silent:

  A logical whether to print a message or not. Defaults to `TRUE` for
  silent operation.

## Value

The status of finding Mplus. Per unix conventions, status 0 indicates
Mplus was found (0 problems) and status 1 indicates that Mplus was not
found.

## Author

Joshua Wiley

## Examples

``` r
mplusAvailable(silent = TRUE)
#> [1] 1
mplusAvailable(silent = FALSE)
#> Mplus is either not installed or could not be found
#> Try installing Mplus or if it already is installed,
#> making sure it can be found by adding it to your PATH or adding a symlink
#> 
#> To see directories on your PATH, From a terminal, run:
#> 
#>   echo $PATH
#> 
#> then try something along these lines:
#> 
#>   sudo ln -s /path/to/mplus/on/your/system /directory/on/your/PATH
#> [1] 1
```
