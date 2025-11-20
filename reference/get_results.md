# Extract Mplus results

This function allows users to extract elements of Mplus output by name
from different types of objects returned by `MplusAutomation`.

## Usage

``` r
get_results(x, element, simplify = FALSE, ...)

get_input(x, simplify = FALSE, ...)

get_warn_err(x, simplify = FALSE, ...)

get_data_summary(x, simplify = FALSE, ...)

get_sampstat(x, simplify = FALSE, ...)

get_covariance_coverage(x, simplify = FALSE, ...)

get_summaries(x, simplify = FALSE, ...)

get_invariance_testing(x, simplify = FALSE, ...)

get_parameters(x, simplify = FALSE, ...)

get_class_counts(x, simplify = FALSE, ...)

get_indirect(x, simplify = FALSE, ...)

get_mod_indices(x, simplify = FALSE, ...)

get_residuals(x, simplify = FALSE, ...)

get_savedata(x, simplify = FALSE, ...)

get_bparameters(x, simplify = FALSE, ...)

get_tech1(x, simplify = FALSE, ...)

get_tech3(x, simplify = FALSE, ...)

get_tech4(x, simplify = FALSE, ...)

get_tech7(x, simplify = FALSE, ...)

get_tech8(x, simplify = FALSE, ...)

get_tech9(x, simplify = FALSE, ...)

get_tech10(x, simplify = FALSE, ...)

get_tech12(x, simplify = FALSE, ...)

get_tech15(x, simplify = FALSE, ...)

get_fac_score_stats(x, simplify = FALSE, ...)

get_lcCondMeans(x, simplify = FALSE, ...)

get_gh5(x, simplify = FALSE, ...)
```

## Arguments

- x:

  Object from which to extract results.

- element:

  Which element of the results to extract.

- simplify:

  Logical; should the result be simplified to a vector, matrix or higher
  dimensional array if possible? See
  [`sapply`](https://rdrr.io/r/base/lapply.html). Defaults to `FALSE`.

- ...:

  Additional arguments passed to and from functions.

## Value

An atomic vector or matrix or list of the same length as X (of length n
for replicate). If simplification occurs, the output type is determined
from the highest type of the return values in the hierarchy NULL \< raw
\< logical \< integer \< double \< complex \< character \< list \<
expression, after coercion of pairlists to lists.

## Examples

``` r
if (FALSE) { # \dontrun{
 test <- mplusObject(MODEL = "mpg ON wt hp;
 wt WITH hp;", rdata = mtcars)
 res <- mplusModeler(test, modelout = "model1.inp", run = 1L)
 get_results(res, "summaries")
 unlink(res$results$input$data$file)
 unlink("model1.inp")
 unlink("model1.out")
} # }
out <- get_input(res)
out <- get_warn_err(res)
out <- get_data_summary(res)
out <- get_sampstat(res)
out <- get_covariance_coverage(res)
out <- get_summaries(res)
out <- get_invariance_testing(res)
out <- get_parameters(res)
out <- get_class_counts(res)
out <- get_indirect(res)
out <- get_mod_indices(res)
out <- get_residuals(res)
out <- get_savedata(res)
out <- get_bparameters(res)
out <- get_tech1(res)
out <- get_tech3(res)
out <- get_tech4(res)
out <- get_tech7(res)
out <- get_tech8(res)
out <- get_tech9(res)
out <- get_tech10(res)
out <- get_tech12(res)
out <- get_tech15(res)
out <- get_fac_score_stats(res)
out <- get_lcCondMeans(res)
out <- get_gh5(res)
```
