# Extract SVALUES syntax and parsed parameter table

Internal parser for the "MODEL COMMAND WITH FINAL ESTIMATES USED AS
STARTING VALUES" section. This section is syntax-oriented rather than
table-oriented, so it is parsed directly from the raw text instead of
relying on MODEL RESULTS.

## Usage

``` r
extractSvalues(outfiletext, filename, input = NULL)
```

## Arguments

- outfiletext:

  Parsed output text

- filename:

  Name of the output file

- input:

  Parsed Mplus input, if available

## Value

A list with `text` and `parameters` elements
