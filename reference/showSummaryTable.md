# Display summary table of Mplus model statistics in separate window

Displays a summary table of model fit statistics extracted using the
`extractModelSummaries` function. This function relies on the `showData`
function from the relimp package, which displays data in a Tk-based
window. By default, the following summary statistics are included:
`Title, LL, Parameters, AIC, AICC, BIC, RMSEA_Estimate`, but these are
customizable using the `keepCols` and `dropCols` parameters.

## Usage

``` r
showSummaryTable(
  modelList,
  keepCols,
  dropCols,
  sortBy = NULL,
  font = "Courier 9"
)
```

## Arguments

- modelList:

  A list of models (as a `data.frame`) returned from the
  `extractModelSummaries` function.

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

  Optional. Field name (as character string) by which to sort the table.
  Typically an information criterion (e.g., “AIC” or “BIC”) is used to
  sort the table. Defaults to `NULL`, which does not sort the table.

- font:

  Optional. The font to be used to display the summary table. Defaults
  to Courier 9.

## Value

No value is returned by this function. It is solely used to display the
summary table in a separate window.

## Note

You must choose between `keepCols` and `dropCols` because it is not
sensible to use these together to include and exclude columns. The
function will error if you include both parameters.

## See also

[`extractModelSummaries`](https://michaelhallquist.github.io/MplusAutomation/reference/extractModelSummaries.md)
[`HTMLSummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/HTMLSummaryTable.md)
[`LatexSummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/LatexSummaryTable.md)

## Author

Michael Hallquist

## Examples

``` r
# make me!!!
```
