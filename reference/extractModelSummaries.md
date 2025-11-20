# (DEPRECATED) Extract summary statistics from a single output file or from a group of Mplus models within a directory

Parses a group of Mplus model output files (.out extension) for model
fit statistics. At this time, the details extracted are fixed and
include:
`Filename, InputInstructions, Title, Estimator, LL, BIC, aBIC, AIC, AICC, Parameters, Observations, CFI, TLI, RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB, RMSEA_pLT05, ChiSqM_Value, ChiSqM_DF, ChiSq_PValue, BLRT_KM1LL, BLRT_PValue, BLRT_Numdraws)`.
The infrastructure is in place to allow for user-specified selection of
summary statistics in future versions.

## Usage

``` r
extractModelSummaries(target = getwd(), recursive = FALSE, filefilter)
```

## Arguments

- target:

  the directory containing Mplus output files (.out) to parse OR the
  single output file to be parsed. Defaults to the current working
  directory. Example: "C:/Users/Michael/Mplus Runs"

- recursive:

  optional. If `TRUE`, parse all models nested in subdirectories within
  `directory`. Defaults to `FALSE`.

- filefilter:

  a Perl regular expression (PCRE-compatible) specifying particular
  output files to be parsed within `directory`. See `regex` or
  <https://www.pcre.org/pcre.txt> for details about regular expression
  syntax.

## Value

Returns a `data.frame` containing model fit statistics for all output
files within `directory`. The `data.frame` contains some of the
following variables (depends on model type):

- `Title`: Title for the model, specified by the TITLE: command

- `Filename`: Filename of the output file

- `Estimator`: Estimator used for the model (e.g., ML, MLR, WLSMV, etc.)

- `LL`: Log-likelihood of the model

- `BIC`: Bayesian Information Criterion

- `aBIC`: Sample-Size-Adjusted BIC (Sclove, 1987)

- `AIC`: Akaike's Information Criterion

- `AICC`: Corrected AIC, based on Sugiura (1978) and recommended by
  Burnham & Anderson (2002)

- `DIC`: Deviance Information Criterion. Available in ESTIMATOR=BAYES
  output.

- `Parameters`: Number of parameters estimated by the model

- `pD`: Estimated number of parameters in Bayesian output

- `Observations`: The number of observations for the model (does not
  suppport multiple-groups analysis at this time)

- `CFI`: Confirmatory Fit Index

- `TLI`: Tucker-Lewis Index

- `RMSEA_Estimate`: Point estimate of root mean squared error of
  approximation

- `RMSEA_90CI_LB`: Lower bound of the 90\\

- `RMSEA_90CI_UB`: Upper bound of the 90\\

- `RMSEA_pLT05`: Probability that the RMSEA estimate falls below .05,
  indicating good fit.

- `ChiSqM_Value`: Model chi-squared value

- `ChiSqM_DF`: Model chi-squared degrees of freedom

- `ChiSqM_PValue`: Model chi-squared p value

- `ChiSqM_ScalingCorrection`: H0 Scaling Correction Factor

- `ObsRepChiSqDiff_95CI_LB`: Lower bound of 95\\

- `ObsRepChiSqDiff_95CI_UB`: Upper bound of 95\\

- `PostPred_PValue`: Posterior predictive p-value

- `PriorPostPred_PValue`: Prior Posterior Predictive P-Value

- `BLRT_RequestedDraws`: Number of requested bootstrap draws for TECH14.

- `BLRT_KM1LL`: Log-likelihood of the K-1 model (one less class) for the
  Bootstrapped Likelihood Ratio Test (TECH14).

- `BLRT_2xLLDiff`: Two times the log-likelihood difference of the models
  with K and K-1 classes (TECH14).

- `BLRT_ParamDiff`: Difference in the number of parameters for models
  with K and K-1 classes (TECH14).

- `BLRT_PValue`: P-value of the Bootstrapped Likelihood Ratio Test
  (TECH14) testing whether the K class model is significantly better
  than K-1

- `BLRT_SuccessfulDraws`: The number of successful bootstrapped samples
  used in the Bootstrapped Likelihood Ratio Test

- `SRMR`: Standardized root mean square residual

- `SRMR.Between`: For TYPE=TWOLEVEL output, standardized root mean
  square residual for between level

- `SRMR.Within`: For TYPE=TWOLEVEL output, standardized root mean square
  residual for within level

- `WRMR`: Weighted root mean square residual

- `ChiSqBaseline_Value`: Baseline (unstructured) chi-squared value

- `ChiSqBaseline_DF`: Baseline (unstructured) chi-squared degrees of
  freedom

- `ChiSqBaseline_PValue`: Baseline (unstructured) chi-squared p value

- `NumFactors`: For TYPE=EFA output, the number of factors

- `T11_KM1Starts`: TECH11: Number of initial stage random starts for k-1
  model

- `T11_KM1Final`: TECH11: Number of final stage optimizations for k-1
  model

- `T11_KM1LL`: TECH11: Log-likelihood of the K-1 model used for the
  Vuong-Lo-Mendell-Rubin LRT

- `T11_VLMR_2xLLDiff`: TECH11: 2 \* Log-likelihood Difference of K-class
  vs. K-1-class model for the Vuong-Lo-Mendell-Rubin LRT

- `T11_VLMR_ParamDiff`: TECH11: Difference in number of parameters
  between K-class and K-1-class model for the Vuong-Lo-Mendell-Rubin LRT

- `T11_VLMR_Mean`: TECH11: Vuong-Lo-Mendell-Rubin LRT mean

- `T11_VLMR_SD`: TECH11: Vuong-Lo-Mendell-Rubin LRT standard deviation

- `T11_VLMR_PValue`: TECH11: Vuong-Lo-Mendell-Rubin LRT p-value

- `T11_LMR_Value`: TECH11: Lo-Mendell-Rubin Adjusted LRT value

- `T11_LMR_PValue`: TECH11: Lo-Mendell-Rubin Adjusted LRT p-value

## See also

[`regex`](https://rdrr.io/r/base/regex.html),
[`runModels`](https://michaelhallquist.github.io/MplusAutomation/reference/runModels.md),
[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
  allExamples <- extractModelSummaries(
    "C:/Program Files/Mplus/Mplus Examples/User's Guide Examples")
} # }
```
