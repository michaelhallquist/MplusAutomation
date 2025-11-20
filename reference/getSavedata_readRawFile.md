# Internal function to load the draws from the Bayesian model posterior distribution

This function is used by other extractor functions that read particular
types of saved data such as factor scores or iterations from an MCMC
chain.

## Usage

``` r
getSavedata_readRawFile(
  outfile,
  outfiletext,
  format = "fixed",
  fileName,
  varNames,
  varWidths,
  input
)
```

## Arguments

- outfile:

  The Mplus output file to be used.

- outfiletext:

  The contents of the Mplus output file

- format:

  A character string indicating the format. Defaults to “fixed”.

- fileName:

  The name of the file

- varNames:

  The names of the variables to extract, comes from the fileInfo

- varWidths:

  The widths of the variables to extract, comes from the fileInfo

- input:

  list of parsed Mplus input section extracted upstream in readModels

## Value

A data frame of the extracted data.
