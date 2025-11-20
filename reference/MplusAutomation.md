# Automating Mplus Model Estimation and Interpretation

The MplusAutomation package leverages the flexibility of the R language
to automate latent variable model estimation and interpretation using
'Mplus', a powerful latent variable modeling program developed by Muthen
and Muthen (<http://www.statmodel.com>). Specifically, MplusAutomation
provides routines for creating related groups of models, running batches
of models, and extracting and tabulating model parameters and fit
statistics.

## Details

The MplusAutomation package has four primary purposes:

- To automatically run groups/batches of models.

- To provide routines to extract model fit statistics, parameter
  estimates, and raw data from 'Mplus' output files.

- To facilitate comparisons among models

- To provide a template language that allows for the creation of related
  input files. The core routine for running batches of models is
  [`runModels`](https://michaelhallquist.github.io/MplusAutomation/reference/runModels.md),
  with an easy-to-use GUI wrapper,
  [`runModels_Interactive`](https://michaelhallquist.github.io/MplusAutomation/reference/runModels_Interactive.md).

The core routine for extracting information from 'Mplus' outputs is
[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md),
which returns a list containing all output sections that the package can
extract.

To extract summaries, parameters, modification indices, SAVEDATA output,
and all other sections that the package can understand, use the
[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)
function. This is the recommended way to extract 'Mplus' output with
this package. If the `target` argument to
[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)
is a single .out file, an `mplus.model` (that is also a `list`) will be
returned containing all output sections that the package can extract. If
`target` is a directory, a list of `mplus.model` objects will be
returned, named according to the output filenames.

Note:
[`extractModelSummaries`](https://michaelhallquist.github.io/MplusAutomation/reference/extractModelSummaries.md)
is deprecated and
[`readModels`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)
should be preferred. To extract model summary statistics from one or
more output files, see
[`extractModelSummaries`](https://michaelhallquist.github.io/MplusAutomation/reference/extractModelSummaries.md),
which returns a `data.frame` of fit statistics for models located within
a directory. Model fit results can be summarized in tabular form (for
comparing among models) using
[`showSummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/showSummaryTable.md)
(displays table in separate window),
[`HTMLSummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/HTMLSummaryTable.md)
(creates HTML file containing summary table), or
[`LatexSummaryTable`](https://michaelhallquist.github.io/MplusAutomation/reference/LatexSummaryTable.md)
(returns a LaTex-formatted table of summary statistics).

Detailed model fit and parameter comparisons between two models can be
obtained using
[`compareModels`](https://michaelhallquist.github.io/MplusAutomation/reference/compareModels.md).

To create a group of related models from a single template, see
[`createModels`](https://michaelhallquist.github.io/MplusAutomation/reference/createModels.md).
Please read the MplusAutomation vignette provided along with the package
(and on the CRAN website) in order to understand the template language:
vignette("Vignette", package="MplusAutomation").

In addition to the major functions above, a function for converting an R
data.frame for use with 'Mplus' is provided:
[`prepareMplusData`](https://michaelhallquist.github.io/MplusAutomation/reference/prepareMplusData.md).
This converts the data.frame to a tab-delimited file and provides an
'Mplus' syntax stub for variable names.

|           |                 |
|-----------|-----------------|
| Package:  | MplusAutomation |
| Type:     | Package         |
| Version:  | 1.2             |
| Date:     | 2025-08-25      |
| License:  | LGPL-3          |
| LazyLoad: | yes             |

## References

Mplus software. Muthen and Muthen. <http://www.statmodel.com>

## See also

See
[`runModels`](https://michaelhallquist.github.io/MplusAutomation/reference/runModels.md)
for an example running a model.

## Author

Michael Hallquist <michael.hallquist@gmail.com>, Joshua F. Wiley
<jwiley.psych@gmail.com>

Maintainer: Michael Hallquist <michael.hallquist@gmail.com>
