# helper function to crawl over the target location, determine if it is a file or folder, then locate all .inp files, and convert them to absolute paths

helper function to crawl over the target location, determine if it is a
file or folder, then locate all .inp files, and convert them to absolute
paths

## Usage

``` r
convert_to_filelist(target, filefilter = NULL, recursive = FALSE)
```

## Arguments

- target:

  a character vector where each element is a directory containing Mplus
  input files (.inp) to run OR a single .inp file to be run. Elements
  may be a full path, relative path, or a filename within the working
  directory.

- filefilter:

  An optional PCRE expression for filtering input files of interest

- recursive:

  if TRUE, search for .inp files in subfolders of all elements of
  `target`

## Value

A vector of .inp file locaitons
