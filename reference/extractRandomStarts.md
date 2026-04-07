# Extract random starts details from Mplus output

Internal helper used by
[`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)
to parse the optional random starts specification block and the ranked
final-stage loglikelihood table.

## Usage

``` r
extractRandomStarts(outfiletext, filename)
```

## Arguments

- outfiletext:

  Parsed Mplus output text.

- filename:

  Name of the output file, used in warnings/errors.

## Value

A list containing random starts specifications and the final-stage
results table, or `NULL` if neither section is present.
