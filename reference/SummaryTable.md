# Create a summary table of Mplus model statistics

Creates output (optionally sent to a file) containing a summary table of
model fit statistics extracted using the `extractModelSummaries`
function. By default, the following summary statistics are included:
`Title, LL, Parameters, AIC, AICC, BIC, RMSEA_Estimate`, but these are
customizable using the `keepCols` and `dropCols` parameters.

## Usage

``` r
SummaryTable(
  modelList,
  type = c("none", "screen", "popup", "html", "latex", "markdown"),
  filename = "",
  keepCols,
  dropCols,
  sortBy = NULL,
  caption = "",
  display = FALSE,
  ...,
  include.rownames = FALSE
)
```

## Arguments

- modelList:

  A list of models returned from the `extractModelSummaries` function.

- type:

  A character vector indicating the type of output format to be
  generated. One of: “none”, “screen”, “popup”, “html”, “latex”, or
  “markdown”. Screen results in a simple summary table being sent to the
  R console.

- filename:

  The name of the file to be created. Can be an absolute or relative
  path. If `filename` is a relative path or just the filename, then it
  is assumed that the file resides in the working directory
  [`getwd()`](https://rdrr.io/r/base/getwd.html). Example:
  `"Mplus Summary.html"`. By default, no filename is given, which
  results in the output being sent to the console. Note that currently,
  filename only has an effect for “html” and “latex”.

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

- caption:

  A character string, the caption to be given to the table. Currently
  only applies to types “html”, “latex”, and “markdown”.

- display:

  optional logical (defaults to `FALSE`). This parameter specifies
  whether to display the table upon creation (`TRUE` or `FALSE`).

- include.rownames:

  optional logical whether to include rownames or not.

- ...:

  additional arguments passed on to specific formatting types.

## Value

Invisibly returns the summary table, which can be used if the printing
options avaiable are not sufficient.

## Note

You must choose between `keepCols` and `dropCols` because it is not
sensible to use these together to include and exclude columns. The
function will error if you include both parameters.

## See also

[`extractModelSummaries`](https://michaelhallquist.github.io/MplusAutomation/reference/extractModelSummaries.md)

## Author

Joshua F. Wiley based on code by Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
 m1 <- mplusObject(TITLE = "Reduced",
  MODEL = "mpg ON wt;", rdata = mtcars)
 m1.fit <- mplusModeler(m1, "mtcars.dat", run = 1L)
 m2 <- mplusObject(TITLE = "Full",
  MODEL = "mpg ON wt hp qsec;", rdata = mtcars)
 m2.fit <- mplusModeler(m2, "mtcars.dat", run = 1L)

 SummaryTable(list(m1.fit, m2.fit))
 SummaryTable(list(m1.fit, m2.fit), type = "popup")
 SummaryTable(list(m1.fit, m2.fit), type = "markdown",
   keepCols = c("Title", "Parameters", "LL", "AIC", "CFI", "SRMR"),
   caption = "Table of Model Fit Statistics",
   split.tables = 200)

 # remove files
 unlink("mtcars.dat")
 unlink("mtcars.inp")
 unlink("mtcars.out")
 unlink("Mplus Run Models.log")
 closeAllConnections()
} # }
```
