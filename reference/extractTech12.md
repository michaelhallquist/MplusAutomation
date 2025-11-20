# Extract Technical 12 from Mplus

The TECH12 option is used in conjunction with TYPE=MIXTURE to request
residuals for observed versus model estimated means, variances,
covariances, univariate skewness, and univariate kurtosis. The observed
values come from the total sample. The estimated values are computed as
a mixture across the latent classes.

## Usage

``` r
extractTech12(outfiletext, filename)
```

## Arguments

- outfiletext:

  the text of the output file

- filename:

  The name of the file

## Value

A list of class “mplus.tech12”

## See also

[`matrixExtract`](https://michaelhallquist.github.io/MplusAutomation/reference/matrixExtract.md)

## Examples

``` r
# make me!!!
```
