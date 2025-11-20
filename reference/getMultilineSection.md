# Extract a multiline section from Mplus output

New approach to multiline section: retain spaces and look for next line
that has identical indentation.

## Usage

``` r
getMultilineSection(
  header,
  outfiletext,
  filename,
  allowMultiple = FALSE,
  allowSpace = TRUE,
  ignore.case = FALSE
)
```

## Arguments

- header:

  Header section

- outfiletext:

  Output file text

- filename:

  The name of the file

- allowMultiple:

  Logical indicating whether to allow multiple sections. Defaults to
  `FALSE`.

- allowSpace:

  Logical indicating whether to allow spaces. Defaults to `TRUE`.

- ignore.case:

  Logical whether or not to ignore the case. Defaults to `FALSE`.

## Value

A list of sections

## Examples

``` r
# make me!!!
```
