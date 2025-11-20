# Test inequality-constrained hypothesis for two or more parameters based on iterations of MCMC chains

Tests an inequality-constrained hypothesis (van de Schoot, Hoijtink,
Hallquist, & Boelen, in press) based on draws from the posterior
distribution of the model parameters, which provides information about
the proportion of the distribution that is in agreement with a given
hypothesis. This function is used for more complex hypotheses about
three or more parameters, whereas testBParamConstraint tests a simple
two-parameter hypothesis.

## Usage

``` r
testBParamCompoundConstraint(bparams, test)
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

- test:

  The `R` code defining the parameter test of three or more parameters.
  Example:
  `"(STAITOT.ON.CG > STAITOT.ON.UCG) & (BDIM.ON.CG > BDIM.ON.UCG)"`.

## Value

No value is returned by this function. Instead, two summary tables are
printed to the screen containing the number and proportion of draws
consistent with the hypothesis.

## Details

This function accepts a bparameters object containing iterations of the
MCMC chains (rows) for each model parameter (columns) and prints out the
number and proportion of draws that are consistent with the requested
hypothesis test.

The `test` argument is evaluated directly as `R` code, with the
`bparams` object attached so that variable names are available directly
in the environment. Because the goal is to evaluate the test for each
draw from the posterior distribution, remember to use vector-based logic
operators, not boolean operators. That is, stick to `&` or `|` for
joining tests of parameters, rather than `&&` or `||` since the latter
will return a single TRUE/FALSE, which is irrelevant.

An example test in R logic would be
`"(STAITOT.ON.CG > STAITOT.ON.UCG) & (BDIM.ON.CG > BDIM.ON.UCG)"`.

## See also

[`testBParamConstraint`](https://michaelhallquist.github.io/MplusAutomation/reference/testBParamConstraint.md)

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
  #using bparameters directly
  btest <- getSavedata_Bparams("model vb1_simpel_b.out")
  testBParametersCompoundConstraint(btest,
  "(STDYX_STAITOT.ON.CG > STDYX_STAITOT.ON.UCG) & (STDYX_BDIM.ON.CG > STDYX_BDIM.ON.UCG)")

  #or using readModels
  btest <- readModels("model vb1_simpel_b.out")
  testBParametersCompoundConstraint(btest,
  "(STDYX_STAITOT.ON.CG > STDYX_STAITOT.ON.UCG) & (STDYX_BDIM.ON.CG > STDYX_BDIM.ON.UCG)")
} # }
```
