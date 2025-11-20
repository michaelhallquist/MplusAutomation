# (DEPRECATED) Extract model parameters from MODEL RESULTS section.

Extracts the model parameters from the MODEL RESULTS section of one or
more Mplus output files. If a particular output file has more than one
results section (unstandardized, stdyx, stdy, and/or std), a list will
be returned. If the `target` is a directory, all .out files therein will
be parsed and a single list will be returned, where the list elements
are named by the output file name. Returned parameters often include the
parameter estimate, std. err, param/s.e., and two-tailed p-value.

## Usage

``` r
extractModelParameters(
  target = getwd(),
  recursive = FALSE,
  filefilter,
  dropDimensions = FALSE,
  resultType
)
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

- dropDimensions:

  Relevant only for multi-file parsing. If `TRUE`, then if only one
  output section (usually unstandardized) is present for all files in
  the parsed list, then eliminate the second-level list (which contains
  elements for each output section). The result is that the elements of
  the returned list are `data.frame` objects with the relevant
  parameters.

- resultType:

  N.B.: this parameter is deprecated and will be removed in a future
  version. The new default is to extract all results that are present
  and return a list (see below for details). `resultType` specified the
  results section to extract. If `raw`, the unstandardized estimates
  will be returned. “stdyx”, “stdy”, and “std” are the other options,
  which extract different standardized solutions. See the Mplus User's
  Guide for additional details about the differences in these
  standardizations.

## Value

If `target` is a single file, a list containing unstandardized and
standardized results will be returned. If all standardized solutions are
available, the list element will be named: `unstandardized`,
`stdyx.standardized`, `stdy.standardized`, and `std.standardized`. If
confidence intervals are output using OUTPUT:CINTERVAL, then a list
element named `ci.unstandardized` will be included. Each of these list
elements is a `data.frame` containing relevant model parameters.

If `target` is a directory, a list will be returned, where each element
contains the results for a single file, and the top-level elements are
named after the corresponding output file name. Each element within this
list is itself a list, with elements as in the single file case above.

The core `data.frame` for each MODEL RESULTS section typically has the
following structure:

- `paramHeader`: The header that begins a given parameter set. Example:
  "FACTOR1 BY"

- `param`: The particular parameter being measured (within
  `paramHeader`). Example: "ITEM1"

- `est`: Parameter estimate value.

- `se`: Standard error of the estimate

- `est_se`: Quotient of `est/se`, representing z-test/t-test in large
  samples

- `pval`: Two-tailed p-value for the `est_se` quotient.

In the case of output from Bayesian estimation (ESTIMATOR=BAYES), the
`data.frame` will contain a different set of variables, including some
of the above, as well as

- `posterior_sd`: Posterior standard deviation of the estimate.

- `lower_2.5ci`: Lower 2.5 percentile of the estimate.

- `upper_2.5ci`: Upper 2.5 percentile (aka 97.5 percentile) of the
  estimate.

Also note that the `pval` column for Bayesian output represents a
one-tailed estimate.

In the case of output from a Monte Carlo study (MONTECARLO: and MODEL
POPULATION:), the `data.frame` will contain a different set of
variables, including some of the above, as well as

- `population`: Population parameter value.

- `average`: Average parameter estimate across replications.

- `population_sd`: Standard deviation of parameter value in population
  across replications.

- `average_se`: Average standard error of estimated parameter value
  across replications.

- `mse`: Mean squared error.

- `cover_95`: Proportion of replications whose 95\\

- `pct_sig_coef`: Proportion of replications for which the two-tailed
  significance test of the parameter is significant (p \< .05).

In the case of confidence interval output (OUTPUT:CINTERVAL), the list
element `ci.unstandardized` will contain a different set of variables,
including some of the above, as well as

- `low.5`: Lower 0.5\\

- `low2.5`: Lower 2.5\\

- `low5`: Lower 5\\

- `est`: Parameter estimate value.

- `up5`: Upper 5\\

- `up2.5`: Upper 2.5\\

- `up.5`: Upper 0.5\\

If the model contains multiple latent classes, an additional variable,
`LatentClass`, will be included, specifying the latent class number.
Also, the Categorical Latent Variables section will be included as
`LatentClass` "Categorical.Latent.Variables."

If the model contains multiple groups, `Group` will be included.

If the model contains two-level output (between/within), `BetweenWithin`
will be included.

## See also

[`extractModelSummaries`](https://michaelhallquist.github.io/MplusAutomation/reference/extractModelSummaries.md)

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
ex3.14 <- extractModelParameters(
  "C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/ex3.14.out")
} # }
```
