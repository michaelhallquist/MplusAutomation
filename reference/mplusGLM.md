# Function to fit GLMs in Mplus

The purpose of this function is to make it (relatively) easy to fit
(most) generalized linear models in Mplus. Fitting GLMs in Mplus offers
advantages such as using full information maximum likelihood for missing
data, robust estimators (default used is MLR), and standard errors
adjusted for clustering (planned; not currently available via
`mplusGLM()`. The overarching aim of this function is to make most GLMs
as easy to fit in Mplus as they are in R.

## Usage

``` r
mplusGLM(formula, data, idvar = "", ...)
```

## Arguments

- formula:

  An R formula class object as used in
  [`glm()`](https://rdrr.io/r/stats/glm.html). Note that currently, only
  basic formula are accepted. On the fly recoding, arthimetic, and on
  the fly interactions do not currently work.

- data:

  A dataset.

- idvar:

  Optional. A character string indicating the name of the ID variable.
  Not currently used but may be used in future.

- ...:

  Additional arguments passed to helper functions. For example
  [`.mplusMultinomial()`](https://michaelhallquist.github.io/MplusAutomation/reference/dot-mplusMultinomial.md).

## Value

A list of results and Mplus model object.

## Details

Note that although there are benefits to fitting GLMs in Mplus. Caution
also is warranted. Using full information maximum likelihood for missing
data requires a number of assumptions. These may be (badly) violated.
`mplusGLM()` requires the analyst to check these as appropriate.

Currently, `mplusGLM()` only supports multinomial outcomes. More
outcomes are planned in the future including binary, continuous/normal,
and count outcomes.

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
test <- mplusGLM(y ~ x1 + x2 + x3, data = tmpd)
} # }
```
