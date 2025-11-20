# Display summary table of Mplus model statistics in separate window

Creates a LaTex-formatted summary table of model fit statistics
extracted using the `extractModelSummaries` function. The table syntax
is returned by the function, which is useful for embedding LaTex tables
using Sweave. By default, the following summary statistics are included:
`Title, LL, Parameters, AIC, AICC, BIC, RMSEA_Estimate`, but these are
customizable using the `keepCols` and `dropCols` parameters.

## Usage

``` r
LatexSummaryTable(
  modelList,
  keepCols,
  dropCols,
  sortBy = NULL,
  label = NULL,
  caption = NULL
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

  optional. Field name (as character string) by which to sort the table.
  Typically an information criterion (e.g., "AIC" or "BIC") is used to
  sort the table. Defaults to `NULL`, which does not sort the table.

- label:

  optional. A character string specifying the label for the LaTex table,
  which can be used for referencing the table.

- caption:

  optional. A character string specifying the caption for the LaTex
  table.

## Value

A LaTex-formatted table summarizing the `modelList` is returned (created
by `xtable`).

## Note

You must choose between `keepCols` and `dropCols` because it is not
sensible to use these together to include and exclude columns. The
function will error if you include both parameters.

## See also

[`extractModelSummaries`](https://michaelhallquist.github.io/MplusAutomation/reference/extractModelSummaries.md),
[`HTMLSummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/HTMLSummaryTable.md),
[`showSummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/showSummaryTable.md),
[`Sweave`](https://rdrr.io/r/utils/Sweave.html)

## Author

Michael Hallquist

## Examples

``` r
# make me!!!
```
