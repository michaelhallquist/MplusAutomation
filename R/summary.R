
#' Subset a list of Mplus model results
#'
#' a helper function to be used by wrappers that generate HTML, LaTex, and on-screen displays of summary statistics
#' @param modelList A list object of Mplus models
#' @param keepCols Columns to keep
#' @param dropCols Columns to drop (use only one of keep/dropCols)
#' @param sortBy How to sort
#' @return Extracted and sorted data
#' @keywords internal
#' @examples
#' # make me!!!
subsetModelList <- function(modelList, keepCols = c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate"), dropCols, sortBy) {

  # only allow keep OR drop.
  if(!missing(keepCols) && !missing(dropCols)) stop("keepCols and dropCols passed to subsetModelList. You must choose one or the other, but not both.")

  # if passed an mplus.model.list from readModels, then just extract summaries for disply
  if (inherits(modelList, "mplus.model.list")) {
    modelList <- do.call("rbind.fill", sapply(modelList, "[", "summaries"))
  } else if (inherits(modelList, "mplus.model")) { #single model (e.g., EFA output with many factor solutions)
    modelList <- modelList$summaries
  }

  # keep only columns specified by keepCols
  if (!missing(keepCols) && length(keepCols) > 0) {
    #check to make sure each column exists if keepCols used
    summaryNames <- names(modelList)
    keepCols <- keepCols[keepCols %in% summaryNames]

    if (length(keepCols) == 0) stop("All fields passed as keepCols are missing from data.frame\n  Fields in data.frame are:\n  ",
      paste(strwrap(paste(summaryNames, collapse=" "), width=80, exdent=4), collapse="\n"))
    MplusData <- modelList[, keepCols, drop = FALSE]
  }

  #drop columns specified by dropCols
  if (!missing(dropCols) && length(dropCols) > 0) {
    MplusData <- modelList
    #Process vector of columns to drop
    for (column in dropCols) {
      MplusData[[column]] <- NULL
    }

  }

  #make a list of non-missing columns
  notMissing <- unlist(lapply(names(MplusData), function(column) {
            if(!all(is.na(MplusData[[column]]))) return(column)
          }))

  #handle cases where sortBy is missing
  if (missing(sortBy)) {
    if ("AICC" %in% notMissing) sortBy <- "AICC"
    else if ("AIC" %in% notMissing) sortBy <- "AIC"
    else if ("BIC" %in% notMissing) sortBy <- "BIC"
    else if ("Title" %in% notMissing) sortBy <- "Title"
    else sortBy <- NA_character_
  }

  if (!sortBy %in% notMissing) stop("sortBy field: ", sortBy,
    " is not present in the summary data.frame.\n  Check your keepCols and dropCols arguments and the summary data.frame")

  #sort data set correctly and drop columns where all models are missing
  #need drop=FALSE to retain as data.frame in case only one column returned
  MplusData <- MplusData[order(MplusData[[sortBy]]), notMissing, drop=FALSE]

  return(MplusData)
}


#' Display summary table of Mplus model statistics in separate window
#'
#' Displays a summary table of model fit statistics extracted using the \code{extractModelSummaries} function.
#' This function relies on the \code{showData} function from the relimp package, which displays data in a Tk-based window.
#' By default, the following summary statistics are included: \code{Title, LL, Parameters, AIC, AICC, BIC, RMSEA_Estimate},
#' but these are customizable using the \code{keepCols} and \code{dropCols} parameters.
#'
#' @note You must choose between \code{keepCols} and \code{dropCols} because
#'   it is not sensible to use these together to include and exclude columns.
#'   The function will error if you include both parameters.
#'
#' @param modelList A list of models (as a \code{data.frame}) returned from the \code{extractModelSummaries} function.
#' @param keepCols A vector of character strings indicating which columns/variables to display in the summary. Only
#'   columns included in this list will be displayed (all others excluded). By default, \code{keepCols} is:
#'   \code{c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")}.
#'   Example: \code{c("Title", "LL", "AIC", "CFI")}
#' @param dropCols A vector of character strings indicating which columns/variables to omit from the summary.
#'   Any column not included in this list will be displayed. By default, \code{dropCols} is \code{NULL}.
#'   Example: \code{c("InputInstructions", "TLI")}
#' @param sortBy Optional. Field name (as character string) by which to sort the table.
#'   Typically an information criterion (e.g., \dQuote{AIC} or \dQuote{BIC}) is used to sort the table. Defaults to \dQuote{AICC}.
#' @param font Optional. The font to be used to display the summary table. Defaults to Courier 9.
#'
#' @return No value is returned by this function. It is solely used to display the summary table in a separate window.
#' @author Michael Hallquist
#' @seealso \code{\link{extractModelSummaries}} \code{\link{HTMLSummaryTable}} \code{\link{LatexSummaryTable}}
#' @export
#' @keywords interface
#' @examples
#' # make me!!!
showSummaryTable <- function(modelList, keepCols, dropCols, sortBy, font="Courier 9") {
  if (!suppressWarnings(require(relimp))) {
    stop("The relimp package is absent. Interactive folder selection cannot function.")
  }

  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)
  showData(MplusData, font=font, placement="+30+30", maxwidth=150, maxheight=50, rownumbers=FALSE, title="Mplus Summary Table")
}

#' Create an HTML file containing a summary table of Mplus model statistics
#'
#' Creates an HTML file containing a summary table of model fit statistics extracted using the \code{extractModelSummaries} function.
#' By default, the following summary statistics are included: \code{Title, LL, Parameters, AIC, AICC, BIC, RMSEA_Estimate},
#' but these are customizable using the \code{keepCols} and \code{dropCols} parameters.
#'
#' @param modelList A list of models (as a \code{data.frame}) returned from the \code{extractModelSummaries} function.
#' @param filename The name of the HTML file to be created. Can be an absolute or relative path. If \code{filename}
#'   is a relative path or just the filename, then it is assumed that the file resides in the working
#'   directory \code{getwd()}. Example: \code{"Mplus Summary.html"}
#' @param keepCols A vector of character strings indicating which columns/variables to display in the summary.
#'   Only columns included in this list will be displayed (all others excluded). By default, \code{keepCols}
#'   is: \code{c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")}. Example: \code{c("Title", "LL", "AIC", "CFI")}
#' @param dropCols A vector of character strings indicating which columns/variables to omit from the summary.
#'   Any column not included in this list will be displayed. By default, \code{dropCols} is \code{NULL}.
#'   Example: \code{c("InputInstructions", "TLI")}
#' @param sortBy optional. Field name (as character string) by which to sort the table. Typically an information criterion
#'   (e.g., "AIC" or "BIC") is used to sort the table. Defaults to "AICC".
#' @param display optional. This parameter specifies whether to display the table in a web
#'   browser upon creation (\code{TRUE} or \code{FALSE}).
#' @return No value is returned by this function. It is solely used to create an HTML file containing summary statistics.
#' @author Michael Hallquist
#' @note You must choose between \code{keepCols} and \code{dropCols} because it is not sensible to use these
#'   together to include and exclude columns. The function will error if you include both parameters.
#' @seealso \code{\link{extractModelSummaries}}, \code{\link{showSummaryTable}}, \code{\link{LatexSummaryTable}}
#' @export
#' @importFrom xtable xtable
#' @keywords interface
#' @examples
#' # make me!!!
HTMLSummaryTable <- function(modelList, filename=file.path(getwd(), "Model Comparison.html"), keepCols, dropCols, sortBy, display=FALSE) {
  #create HTML table and write to file.

  #ensure that the filename has a .html or .htm at the end
  if (!length(grep(".*\\.htm[l]*", filename)) > 0) {
    filename <- paste0(filename, ".html")
  }

  if (length(grep("[\\/]", filename)) == 0) {
    #Filename does not contain a path. Therefore, add the working directory
    filename <- file.path(getwd(), filename)
  }

  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)

  print(x=xtable(MplusData),
      type="html",
      file=filename,
      include.rownames = FALSE,
      NA.string = "."
  )

  if (display) {
    #load table in browser
    shell.exec(paste0("file:///", filename))
  }

}

#' Display summary table of Mplus model statistics in separate window
#'
#' Creates a LaTex-formatted summary table of model fit statistics extracted using the \code{extractModelSummaries} function.
#' The table syntax is returned by the function, which is useful for embedding LaTex tables using Sweave.
#' By default, the following summary statistics are included: \code{Title, LL, Parameters, AIC, AICC, BIC, RMSEA_Estimate},
#' but these are customizable using the \code{keepCols} and \code{dropCols} parameters.
#'
#' @param modelList A list of models (as a \code{data.frame}) returned from the \code{extractModelSummaries} function.
#' @param keepCols A vector of character strings indicating which columns/variables to display in the summary. Only columns
#'   included in this list will be displayed (all others excluded). By default, \code{keepCols}
#'   is: \code{c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")}.
#'   Example: \code{c("Title", "LL", "AIC", "CFI")}
#' @param dropCols A vector of character strings indicating which columns/variables to omit from the summary.
#'   Any column not included in this list will be displayed. By default, \code{dropCols} is \code{NULL}.
#'   Example: \code{c("InputInstructions", "TLI")}
#' @param sortBy optional. Field name (as character string) by which to sort the table.
#'   Typically an information criterion (e.g., "AIC" or "BIC") is used to sort the table. Defaults to "AICC"
#' @param label optional. A character string specifying the label for the LaTex table, which can be
#'   used for referencing the table.
#' @param caption optional. A character string specifying the caption for the LaTex table.
#' @return A LaTex-formatted table summarizing the \code{modelList} is returned (created by \code{xtable}).
#' @author Michael Hallquist
#' @note You must choose between \code{keepCols} and \code{dropCols} because it is not sensible to use these together
#'   to include and exclude columns. The function will error if you include both parameters.
#' @seealso \code{\link{extractModelSummaries}}, \code{\link{HTMLSummaryTable}}, \code{\link{showSummaryTable}}, \code{\link{Sweave}}
#' @export
#' @importFrom xtable xtable
#' @keywords interface
#' @examples
#' # make me!!!
LatexSummaryTable <- function(modelList, keepCols, dropCols, sortBy, label=NULL, caption=NULL) {
  #return latex table to caller

  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)

  return(xtable(MplusData, label=label, caption=caption))
}

