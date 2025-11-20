# (DEPRECATED) Extract model modification indices.

Extracts the model modification indices from the MODEL MODIFICATION
INDICES section of one or more Mplus output files. If the `target` is a
directory, all .out files therein will be parsed and a single list will
be returned, where the list elements are named by the output file name.
Returned parameters typically include the pairwise relationships between
variables to be freed, the change in model chi-square (M.I.), and the
expected parameter change (E.P.C.).

## Usage

``` r
extractModIndices(target = getwd(), recursive = FALSE, filefilter)
```

## Arguments

- target:

  the directory containing Mplus output files (.out) to parse OR the
  single output file to be parsed. May be a full path, relative path, or
  a filename within the working directory. Defaults to the current
  working directory. Example: “C:/Users/Michael/Mplus Runs”

- recursive:

  optional. If `TRUE`, parse all models nested in subdirectories within
  `target`. Defaults to `FALSE`.

- filefilter:

  a Perl regular expression (PCRE-compatible) specifying particular
  output files to be parsed within `directory`. See `regex` or
  <https://www.pcre.org/pcre.txt> for details about regular expression
  syntax.

## Value

If `target` is a single file, a data.frame containing modification
results for the target output file will be returned. If `target` is a
directory, a list will be returned, where each element contains a
data.frame of the modification indices for a single file, and the
top-level elements are named after the corresponding output file name.
The basic `data.frame` containing the MODEL MODIFICATION INDICES section
of `outfile`. Variables include

- `modV1`: The first variable in the pair to be freed according to the
  M.I.

- `operator`: The suggested relationship between `modV1` and `modV2`
  (e.g., WITH for freeing the covariance between `modV1` and `modV2`)

- `modV2`: The first variable in the pair to be freed according to the
  M.I.

- `MI`: The decrease in model chi-square if the specified relationship
  is freely estimated

- `EPC`: The expected parameter estimate between `modV1` and `modV2` if
  freed.

- `Std_EPC`: The EPC value standardized using the variances of the
  continuous latent variables.

- `StdYX_EPC`: The EPC value standardized using the variances of the
  continuous latent variables as well as the variances of the background
  and/or outcome variables.

## See also

[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md),
[`extractModelSummaries`](https://michaelhallquist.github.io/MplusAutomation/reference/extractModelSummaries.md),
[`extractModelParameters`](https://michaelhallquist.github.io/MplusAutomation/reference/extractModelParameters.md)

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
ex3.14 <- extractModIndices(
  "C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/ex3.14.out")
} # }
```
