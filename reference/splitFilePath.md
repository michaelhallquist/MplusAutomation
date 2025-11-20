# Split File and Path into Separate Parts

This is a helper function to split path into path and filename. Code
adapted from R.utils filePath command.

## Usage

``` r
splitFilePath(filepath, normalize = FALSE)
```

## Arguments

- filepath:

  A character string of the file path

## Value

A list with elements for the directory, filename, and absolute path.

## Examples

``` r
  if (FALSE) { # \dontrun{
  splitFilePath("dir1/subdir1/mplus1.inp")
  } # }
```
