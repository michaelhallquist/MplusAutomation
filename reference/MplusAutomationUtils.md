# Clean data and calculate the md5 hash

Internal utility function, primarily for `prepareMplusData`.

Internal utility function, primarily for `prepareMplusData`.

## Usage

``` r
.cleanHashData(df, keepCols = NULL, dropCols = NULL, imputed = FALSE)

.hashifyFile(filename, hash, useexisting = FALSE)
```

## Arguments

- df:

  The R data.frame to be prepared for Mplus

- keepCols:

  A character vector specifying the variable names within `df` to be
  output to `filename` or a numeric vector of the column indices to be
  output or a logical vector corresponding to the same.

- dropCols:

  A character vector specifying the variable names within `df` to be
  omitted from the data output to `filename` or a numeric vector of the
  column indices not to be output or a logical vector corresponding to
  the same.

- imputed:

  A logical whether data are multiply imputed. Defaults to `FALSE`. If
  `TRUE`, the data should be a list, where each element of the list is a
  multiply imputed dataset.

- filename:

  A character vector containing the filename

- hash:

  A character vector with the hash to use

- useexisting:

  A logical whether to use an existing file name if one is found
  containing the hash. Defaults to `FALSE` in which case the hash is
  added to the user specified filename

## Value

A list of the data and the md5 hash.

A list of the filename (plus hash) and a logical value whether a
filename with the hash already existed or not.

## Examples

``` r
if (FALSE) { # \dontrun{

## basic example
MplusAutomation:::.cleanHashData(mtcars)

## has changes when data changes
MplusAutomation:::.cleanHashData(mtcars[-15,])

## example on a list (e.g., for multiply imputed data)

MplusAutomation:::.cleanHashData(
 list(
   data.frame(a = 1:4),
   data.frame(a = c(2, 2, 3, 4))),
  imputed = TRUE)

} # }
MplusAutomation:::.hashifyFile("testit.dat", "abc")
#> $filename
#> [1] "testit_abc.dat"
#> 
#> $fileexists
#> [1] FALSE
#> 
```
