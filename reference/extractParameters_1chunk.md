# Extract Parameters for One Chunk

Helper function for extractModelParameters. Used to parse each
subsection of output within a given file and given results section
(e.g., stdyx section) There will be many chunks if latent classes,
multiple groups, multilevel features are used.

## Usage

``` r
extractParameters_1chunk(filename, thisChunk, columnNames, sectionName)
```

## Arguments

- filename:

  name of Mplus output file being processed

- thisChunk:

  character vector of output chunk from which to extract parameter
  values

- columnNames:

  character vector of expected column names for output chunk

## Value

A data frame (or matrix?)
