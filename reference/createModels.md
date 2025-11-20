# Create Mplus Input Files from Template

The `createModels` function processes a single Mplus template file and
creates a group of related model input files. Definitions and examples
for the template language are provided in the MplusAutomation vignette
and are not duplicated here at the moment. See this documentation:
`vignette("Vignette", package="MplusAutomation")`

## Usage

``` r
createModels(templatefile)
```

## Arguments

- templatefile:

  The filename (absolute or relative path) of an Mplus template file to
  be processed. Example “C:/MplusTemplate.txt”

## Value

No value is returned by this function. It is solely used to process an
Mplus template file.

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
  createModels("L2 Multimodel Template No iter.txt")
} # }
```
