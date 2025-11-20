# Create a summary table of Mplus mixture models

Creates a summary table of model fit statistics and relevant diagnostic
information for a list of mixture models. Default statistics reported
are in line with published guidelines (see Jung & Wickrama, 2008; Nylund
et al., 2007):
`c("Title", "Classes", "Warnings", "AIC", "BIC", "aBIC", "Entropy", "T11_VLMR_PValue", "T11_LMR_PValue", "BLRT_PValue", "min_N", "max_N", "min_prob", "max_prob")`.
The table is customizable using the `keepCols` parameter, which is
passed through to
[SummaryTable](https://michaelhallquist.github.io/MplusAutomation/reference/SummaryTable.md).

## Usage

``` r
mixtureSummaryTable(
  modelList,
  keepCols = c("Title", "Classes", "Warnings", "AIC", "BIC", "aBIC", "Entropy",
    "T11_VLMR_PValue", "T11_LMR_PValue", "BLRT_PValue", "min_N", "max_N", "min_prob",
    "max_prob"),
  sortBy = NULL,
  ...
)
```

## Arguments

- modelList:

  A list of models returned from the `extractModelSummaries` function.

- keepCols:

  A vector of character strings indicating which columns/variables to
  display in the summary. Only columns included in this list will be
  displayed (all others excluded). By default, `keepCols` is:
  `c("Title", "Classes", "Warnings", "AIC", "BIC", "aBIC","Entropy", "T11_VLMR_PValue", "T11_LMR_PValue", "BLRT_PValue", "min_N", "max_N", "min_prob", "max_prob")`.

- sortBy:

  Field name (as character string) by which to sort the table. Typically
  an information criterion (e.g., "AIC" or "BIC") is used to sort the
  table. Defaults to "AICC". Set to NULL by default, so the table is
  ordered by increasing number of classes.

- ...:

  Arguments passed to
  [`SummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/SummaryTable.md).

## Value

An object of class data.frame.

## Note

This function is partially a wrapper around SummaryTable, with
enhancements for summarizing mixture models.

## See also

[`SummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/SummaryTable.md)

## Author

Caspar J. van Lissa

## Examples

``` r
if (FALSE) { # \dontrun{
res <- createMixtures(classes = 1:2, filename_stem = "iris", rdata = iris,
               OUTPUT = "tech11 tech14;",
               run = 1L)
mixtureSummaryTable(res)
} # }
```
