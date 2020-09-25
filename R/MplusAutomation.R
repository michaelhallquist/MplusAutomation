#' @title Automating Mplus Model Estimation and Interpretation
#' @docType package
#' @name MplusAutomation
#'
#' @description The MplusAutomation package leverages the flexibility of the
#'   R language to automate latent variable model estimation and interpretation
#'   using 'Mplus', a powerful latent variable modeling program developed by Muthen
#'   and Muthen (\url{http://www.statmodel.com}). Specifically, MplusAutomation provides routines
#'   for creating related groups of models, running batches of models, and extracting
#'   and tabulating model parameters and fit statistics.
#'
#' @details The MplusAutomation package has four primary purposes:
#' \enumerate{
#'   \item To automatically run groups/batches of models.
#'   \item To provide routines to extract model fit statistics, parameter estimates, and raw data from 'Mplus' output files.
#'   \item To facilitate comparisons among models
#'   \item To provide a template language that allows for the creation of related input files.
#' }
#' The core routine for running batches of models is \code{\link{runModels}}, with
#'   an easy-to-use GUI wrapper, \code{\link{runModels_Interactive}}.
#'
#' The core routine for extracting information from 'Mplus' outputs is \code{\link{readModels}}, which
#'   returns a list containing all output sections that the package can extract.
#'
#' To extract summaries, parameters, modification indices, SAVEDATA output, and all other sections that the package
#'   can understand, use the \code{\link{readModels}} function. This is the recommended way to extract 'Mplus'
#'   output with this package. If the \code{target} argument to \code{\link{readModels}} is a single .out file,
#'   an \code{mplus.model} (that is also a \code{list}) will be returned containing all output sections that
#'   the package can extract. If \code{target} is a directory, a list of \code{mplus.model} objects will be returned,
#'   named according to the output filenames.
#'
#' Note: \code{\link{extractModelSummaries}} is deprecated and \code{\link{readModels}} should be preferred.
#'   To extract model summary statistics from one or more output files, see
#'   \code{\link{extractModelSummaries}}, which returns a \code{data.frame} of
#'   fit statistics for models located within a directory. Model fit results can
#'   be summarized in tabular form (for comparing among models) using
#'   \code{\link{showSummaryTable}} (displays table in separate window),
#'   \code{\link{HTMLSummaryTable}} (creates HTML file containing summary table),
#'   or \code{\link{LatexSummaryTable}} (returns a LaTex-formatted table of
#'   summary statistics).
#'
#' Deprecated: To extract raw data created by the SAVEDATA command (e.g., class membership probabilities
#'   or factor scores), see \code{\link{getSavedata_Data}}.
#'
#' Deprecated: To extract unstandardized or standardized parameter estimates from a single output file,
#'   see \code{\link{extractModelParameters}}.
#'
#' Detailed model fit and parameter comparisons between two models can be obtained using
#'   \code{\link{compareModels}}.
#'
#' To create a group of related models from a single template, see \code{\link{createModels}}.
#'   Please read the MplusAutomation vignette provided along with the package (and on the CRAN website)
#'   in order to understand the template language: vignette("Vignette", package="MplusAutomation").
#'
#' In addition to the major functions above, a function for converting an R data.frame
#'   for use with 'Mplus' is provided: \code{\link{prepareMplusData}}. This converts the
#'   data.frame to a tab-delimited file and provides an 'Mplus' syntax stub for variable names.
#'
#' \tabular{ll}{
#' Package: \tab MplusAutomation\cr
#' Type: \tab Package\cr
#' Version: \tab 0.7-3\cr
#' Date: \tab 2018-11-19\cr
#' License: \tab LGPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @author
#' Michael Hallquist \email{michael.hallquist@@gmail.com},
#' Joshua F. Wiley \email{jwiley.psych@@gmail.com}
#'
#' Maintainer: Michael Hallquist \email{michael.hallquist@@gmail.com}
#' @keywords package
#' @import plyr
#' @importFrom utils packageDescription
#' @seealso See \code{\link{runModels}} for an example running a model.
#' @references Mplus software. Muthen and Muthen. \url{http://www.statmodel.com}
NULL


# display version number and citation when the package is loaded
.onAttach <- function(libname, pkgname) {

  desc  <- packageDescription(pkgname, libname)

  packageStartupMessage(

      'Version:  ', desc$Version, '\n',

    'We work hard to write this free software. Please help us get credit by citing: \n\n',

    paste("Hallquist, M. N. & Wiley, J. F. (2018).",
      "MplusAutomation: An R Package for Facilitating Large-Scale Latent Variable Analyses in Mplus.",
      "Structural Equation Modeling, 25, 621-638.",
      "doi: 10.1080/10705511.2017.1402334.\n\n"),

    '-- see citation("MplusAutomation").'

  )
}
