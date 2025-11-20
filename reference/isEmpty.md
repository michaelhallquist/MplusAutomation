# Check whether a useable function argument was provided

This is a simple utility to check whether a function argument is
missing, `NULL`, or has only `NA`s.

## Usage

``` r
isEmpty(arg)
```

## Arguments

- arg:

  A function argument

## Value

Logical vector of length 1.

## Examples

``` r
if (FALSE) { # \dontrun{
f1 <- function(x) {
  if (!isEmpty(x)) return(mean(x, na.rm = TRUE))
  return(NULL)
}

f1()                 #> NULL
f1(x = NA)           #> NULL
f1(x = NULL)         #> NULL
f1(x = c(NA, 1:2))   #> 1.5
} # }
```
