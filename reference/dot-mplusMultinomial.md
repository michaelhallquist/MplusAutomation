# Internal Function for Multinomial Regression in Mplus

Internal Function for Multinomial Regression in Mplus

## Usage

``` r
.mplusMultinomial(
  dv,
  iv,
  data,
  idvar = "",
  integration = 1000,
  processors = 2,
  OR = TRUE,
  pairwise = TRUE,
  ...
)
```

## Arguments

- dv:

  A character string with the variable name for the dependent (outcome)
  variable.

- iv:

  A character vector with the variable name(s) for the independent
  (predictor/explanatory) variable(s).

- data:

  A dataset.

- idvar:

  Optional. A character string indicating the name of the ID variable.
  Not currently used but may be used in future.

- integration:

  An integer indicating the number of Monte Carlo integration points to
  use. Defaults to 1000.

- processors:

  An integer indicating the number of processors to use. Passed to
  Mplus. Defaults to 2.

- OR:

  A logical value whether odds ratios should be returned. Defaults to
  `TRUE`.

- pairwise:

  A logical value indicating whether all pairwise tests should be
  computed. Defaults to `TRUE`.

- ...:

  Additional arguments passed to
  [`mplusModeler()`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.md).

## Value

A list of results and Mplus model object.

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{

set.seed(1234)
tmpd <- data.frame(
  x1 = rnorm(200),
  x2 = rnorm(200),
  x3 = cut(rnorm(200),
           breaks = c(-Inf, -.7, .7, Inf),
           labels = c("a", "b", "c")))
tmpd$y <- cut(rnorm(200, sd = 2) + tmpd$x1 + tmpd$x2 + I(tmpd$x3 == "b"),
              breaks = c(-Inf, -.5, 1, Inf),
              labels = c("L", "M", "H"))

tmpres <- MplusAutomation:::.mplusMultinomial(
  dv = "y",
  iv = c("x1", "x2"),
  data = tmpd,
  pairwise = TRUE)
tmpres2 <- MplusAutomation:::.mplusMultinomial(
  dv = "y",
  iv = c("x1", "x2"),
  data = tmpd,
  pairwise = FALSE)
tmpres3 <- MplusAutomation:::.mplusMultinomial(
  dv = "y",
  iv = c("x1@0", "x2@0"),
  data = tmpd,
  pairwise = FALSE)

} # }
```
