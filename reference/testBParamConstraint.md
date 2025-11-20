# Test inequality-constrained hypothesis for two parameters based on iterations of MCMC chains

Tests a simple inequality-constrained hypothesis (van de Schoot,
Hoijtink, Hallquist, & Boelen, in press) based on draws from the
posterior distribution of the model parameters, which provides
information about the proportion of the distribution that is in
agreement with a given hypothesis. This function is used for simple
hypothesis for two parameters, whereas testBParamCompoundConstraint
gives full access to multiple parameters and R's logic syntax. This
function accepts a bparameters object containing iterations of the MCMC
chains (rows) for each model parameter (columns) and prints out the
number and proportion of draws that are consistent with the requested
hypothesis test. The `coef1`, `operator`, and `coef2` arguments are
appended in sequence, so that the hypothesis test is constructed from
left-to-right. e.g.,
`testBParamConstraint(bparamsDF, "MGM.TRT1", ">", "MGM.EX2")`.

## Usage

``` r
testBParamConstraint(bparams, coef1, operator, coef2)
```

## Arguments

- bparams:

  An object containing draws from the posterior distribution (class
  `mplus.model` or `mplus.bparameters`). Obtained by
  SAVEDATA:BPARAMETERS in Mplus and
  [`getSavedata_Bparams`](https://michaelhallquist.github.io/MplusAutomation/reference/getSavedata_Bparams.md)
  or
  [`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)
  in `MplusAutomation`.

- coef1:

  The name of the first parameter to be compared. Example: `"MGM.TRT1"`

- operator:

  A logical operator to compare the two parameters. Should be one of
  `>=, >, <, or <=`. Example: `">="`

- coef2:

  The name of the first parameter to be compared. Example: `"MGM.EX2"`

## Value

No value is returned by this function. Instead, two summary tables are
printed to the screen containing the number and proportion of draws
consistent with the hypothesis.

## See also

[testBParamCompoundConstraint](https://michaelhallquist.github.io/MplusAutomation/reference/testBParamCompoundConstraint.md)

## Author

Michael Hallquist

## Examples
