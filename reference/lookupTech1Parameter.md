# Lookup the matrix element for a give parameter number

The `lookupTech1Parameter` function identifies the position in the Mplus
model matrices corresponding to a given parameter defined in the
TECHNICAL 1 PARAMETER SPECIFICATION OUTPUT. The goal of this function is
to aid in identifying problematic parameters often printed in the
warnings and errors section of Mplus output.

## Usage

``` r
lookupTech1Parameter(tech1Output, paramNumber)
```

## Arguments

- tech1Output:

  The object corresponding to the TECH1 parameter specification from
  readModels.

- paramNumber:

  The parameter number to lookup

## Value

A `data.frame` containing the row(s) and column(s) of TECH1 parameter
specification matching the requested `paramNumber`.

## See also

[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
  models <- readModels("test1.out")
  param <- lookupTech1Parameter(models$tech1, 16)
} # }
```
