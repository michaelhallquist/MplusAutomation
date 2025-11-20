# Reconstruct matrix from multi-line text output

main worker function for extracting Mplus matrix output where matrices
are spread across blocks to keep within width constraints example: tech1
matrix output.

## Usage

``` r
matrixExtract(
  outfiletext,
  headerLine,
  filename,
  ignore.case = FALSE,
  expect_sig = FALSE
)
```

## Arguments

- outfiletext:

  The text of the output file

- headerLine:

  The header line

- filename:

  The name of the output file

- ignore.case:

  Whether to ignore case of header line

- expect_sig:

  Whether to track value significance TRUE/FALSE (\* in Mplus) as an
  attribute

## Value

a matrix

## Examples

``` r
# make me!!!
```
