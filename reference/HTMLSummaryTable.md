# Create an HTML file containing a summary table of Mplus model statistics

Creates an HTML file containing a summary table of model fit statistics
extracted using the `extractModelSummaries` function. By default, the
following summary statistics are included:
`Title, LL, Parameters, AIC, AICC, BIC, RMSEA_Estimate`, but these are
customizable using the `keepCols` and `dropCols` parameters.

## Usage

``` r
HTMLSummaryTable(
  modelList,
  filename = file.path(getwd(), "Model Comparison.html"),
  keepCols,
  dropCols,
  sortBy = NULL,
  display = FALSE
)
```

## Arguments

- modelList:

  A list of models (as a `data.frame`) returned from the
  `extractModelSummaries` function.

- filename:

  The name of the HTML file to be created. Can be an absolute or
  relative path. If `filename` is a relative path or just the filename,
  then it is assumed that the file resides in the working directory
  [`getwd()`](https://rdrr.io/r/base/getwd.html). Example:
  `"Mplus Summary.html"`

- keepCols:

  A vector of character strings indicating which columns/variables to
  display in the summary. Only columns included in this list will be
  displayed (all others excluded). By default, `keepCols` is:
  `c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")`.
  Example: `c("Title", "LL", "AIC", "CFI")`

- dropCols:

  A vector of character strings indicating which columns/variables to
  omit from the summary. Any column not included in this list will be
  displayed. By default, `dropCols` is `NULL`. Example:
  `c("InputInstructions", "TLI")`

- sortBy:

  optional. Field name (as character string) by which to sort the table.
  Typically an information criterion (e.g., "AIC" or "BIC") is used to
  sort the table. Defaults to `NULL`, which does not sort the table.

- display:

  optional. This parameter specifies whether to display the table in a
  web browser upon creation (`TRUE` or `FALSE`).

## Value

No value is returned by this function. It is solely used to create an
HTML file containing summary statistics.

## Note

You must choose between `keepCols` and `dropCols` because it is not
sensible to use these together to include and exclude columns. The
function will error if you include both parameters.

## See also

[`extractModelSummaries`](https://michaelhallquist.github.io/MplusAutomation/reference/extractModelSummaries.md),
[`showSummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/showSummaryTable.md),
[`LatexSummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/LatexSummaryTable.md)

## Author

Michael Hallquist

## Examples

``` r
# make me!!!
```
