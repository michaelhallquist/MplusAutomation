# Get Output File List

This is a helper function used by extractModelSummaries and
extractModelParameters. It determines whether the target is a single
file or a directory. If it is a directory, all .out files are returned
(perhaps recursively) It also permits the files to be filtered using a
certain regular expression.

## Usage

``` r
getOutFileList(target, recursive = FALSE, filefilter, pathfilter)
```

## Arguments

- target:

  The target file or directory

- recursive:

  A logical value whether to search recursively. Defaults to `FALSE`.

- filefilter:

  A PCRE regular expression passed to `grep` used to filter the output
  files based on their file names

- pathfilter:

  A PCRE regular expression passed to `grep` used to filter the output
  files based on their absolute paths

## Value

A character vector of the output files
