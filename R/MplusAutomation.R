#' @title Automating Mplus Model Estimation and Interpretation
#' @docType package
#' @name MplusAutomation
#'
#' @description The MplusAutomation package leverages the flexibility of the
#'   R language to automate latent variable model estimation and interpretation
#'   using Mplus, a powerful latent variable modeling program developed by Muthen
#'   and Muthen (www.statmodel.com). Specifically, MplusAutomation provides routines
#'   for creating related groups of models, running batches of models, and extracting
#'   and tabulating model parameters and fit statistics.
#'
#' @details The MplusAutomation package has four primary purposes:
#' \enumerate{
#'   \item To automatically run groups/batches of models.
#'   \item To provide routines to extract model fit statistics, parameter estimates, and raw data from Mplus output files.
#'   \item To facilitate comparisons among models
#'   \item To provide a template language that allows for the creation of related input files.
#' }
#' The core routine for running batches of models is \code{\link{runModels}}, with
#'   an easy-to-use GUI wrapper, \code{\link{runModels_Interactive}}.
#'
#' To extract model summary statistics from one or more output files, see
#'   \code{\link{extractModelSummaries}}, which returns a \code{data.frame} of
#'   fit statistics for models located within a directory. Model fit results can
#'   be summarized in tabular form (for comparing among models) using
#'   \code{\link{showSummaryTable}} (displays table in separate window),
#'   \code{\link{HTMLSummaryTable}} (creates HTML file containing summary table),
#'   or \code{\link{LatexSummaryTable}} (returns a LaTex-formatted table of
#'   summary statistics).
#'
#' To extract raw data created by the SAVEDATA command (e.g., class membership probabilities
#'   or factor scores), see \code{\link{getSavedata_Data}}.
#'
#' To extract unstandardized or standardized parameter estimates from a single output file,
#'   see \code{\link{extractModelParameters}}.
#'
#' Summaries, parameters, modification indices, and SAVEDATA output can be extracted
#'   simultaneously using the \code{\link{readModels}} function, and this is the
#'   recommended way to extract output with this package.
#'
#' Model fit and parameter comparisons between models can be obtained using
#'   \code{\link{compareModels}}.
#'
#' To create a group of related models from a single template, see \code{\link{createModels}}.
#'   Please read the MplusAutomation vignette provided along with the package (and on the CRAN website)
#'   in order to understand the template language.
#'
#' In addition to the major functions above, a function for converting an R data.frame
#'   for use with Mplus is provided: \code{\link{prepareMplusData}}. This converts the
#'   data.frame to a tab-delimited file and provides an Mplus syntax stub for variable names.
#'
#' \tabular{ll}{
#' Package: \tab MplusAutomation\cr
#' Type: \tab Package\cr
#' Version: \tab 0.6-2\cr
#' Date: \tab 2013-10-24\cr
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
#' @import tcltk
#' @import plyr
#' @seealso See \code{\link{runModels}} for an example running a model.
#' @references Mplus software. Muthen and Muthen. \url{http://www.statmodel.com}
NULL
