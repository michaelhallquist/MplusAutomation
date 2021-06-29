#' @param dataout the name of the file to output the data to for Mplus.
#'   If missing, defaults to \code{modelout} changing .inp to .dat.
#' @param modelout the name of the output file for the model.
#'   This is the file all the syntax is written to, which becomes the
#'   Mplus input file. It should end in .inp.  If missing, defaults to
#'   \code{dataout} changing the extension to .inp.
#' @param run an integer indicating how many models should be run. Defaults to zero.
#'   If zero, the data and model input files are all created, but the model is not run.
#'   This can be useful for seeing how the function works and what setup is done. If one, a basic
#'   model is run. If greater than one, the model is bootstrapped with \code{run} replications as
#'   well as the basic model.
#' @param check logical whether the body of the Mplus syntax should be checked for missing
#'   semicolons using the \code{\link{parseMplus}} function. Defaults to \code{FALSE}.
#' @param varwarnings A logical whether warnings about variable length should be left, the
#'   default, or removed from the output file.
#' @param Mplus_command optional. N.B.: No need to pass this parameter for most users (has intelligent
#'   defaults). Allows the user to specify the name/path of the Mplus executable to be used for
#'   running models. This covers situations where Mplus is not in the system's path,
#'   or where one wants to test different versions of the Mplus program.
#' @param writeData A character vector, one of \sQuote{ifmissing},
#'   \sQuote{always}, \sQuote{never} indicating whether the data files
#'   (*.dat) should be written to disk.  This is passed on to \code{prepareMplusData}.
#'   Note that previously, \code{mplusModeler} always (re)wrote the data to disk.
#'   However, now the default is to write the data to disk only if it is missing
#'   (i.e., \sQuote{ifmissing}).  See details for further information.
#' @param hashfilename A logical whether or not to add a hash of the raw data to the
#'   data file name.  Defaults to \code{TRUE} in \code{mplusModeler}.  Note that this
#'   behavior is a change from previous versions and differs from \code{prepareMplusData}
#'   which maintains the old behavior by default of \code{FALSE}.
#' @param killOnFail A logical whether or not to kill any mplus processes on failure.
#'   Passed on to control behavior of \code{\link{runModels}}. Defaults to \code{TRUE}.
#' @param \ldots additional arguments passed to the
#'   \code{\link[MplusAutomation]{prepareMplusData}} function.
#' @param quiet optional. If \code{TRUE}, show status messages in the console.
#' @return An Mplus model object, with results.
#'   If \code{run = 1}, returns an invisible list of results from the run of
#'   the Mplus model (see \code{\link[MplusAutomation]{readModels}} from the
#'   MplusAutomation package). If \code{run = 0}, the function returns a list
#'   with two elements, \sQuote{model} and \sQuote{boot} that are both \code{NULL}.
#'   if \code{run >= 1},returns a list with two elements, \sQuote{model} and \sQuote{boot}
#'   containing the regular Mplus model output and the boot object, respectively.
#'   In all cases, the Mplus data file and input files are created.
