# Extract values from Mplus output An internal function used by extractSummaries_1file to extract parameters from the output file using regular expressions.

Extract values from Mplus output An internal function used by
extractSummaries_1file to extract parameters from the output file using
regular expressions.

## Usage

``` r
extractValue(pattern, textToScan, filename, type = "int")
```

## Arguments

- pattern:

  the exact text to be matched in the outfile that identifies the
  parameter of interest

- textToScan:

  the chunk of Mplus output to be parsed, passed as a vector of
  character strings (from the scan command).

- filename:

  the name of the file containing textToScan. Used to make more
  intelligible warning messages.

- type:

  the data type of the parameter, which determines the regexp used.
  Currently can be “int”, “dec”, “str”, or “calc”. Defaults to “int”.

## Value

A string or numeric vector

## Examples

``` r
#make me!!!
```
