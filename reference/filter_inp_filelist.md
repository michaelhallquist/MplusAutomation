# helper function to filter a set of Mplus input files based on whether the corresponding .out file already exists

helper function to filter a set of Mplus input files based on whether
the corresponding .out file already exists

## Usage

``` r
filter_inp_filelist(inp_files, replaceOutfile = "always", quiet = TRUE)
```

## Arguments

- inp_files:

  a vector of input file locations to check for corresponding .out files

- replaceOutfile:

  optional. Currently supports three settings: “always”, which runs all
  models, regardless of whether an output file for the model exists;
  “never”, which does not run any model that has an existing output
  file; and “modifiedDate”, which only runs a model if the modified date
  for the input file is more recent than the output file modified date
  (implying there have been updates to the model).

- quiet:

  whether to print out text indicating what files were skipped
