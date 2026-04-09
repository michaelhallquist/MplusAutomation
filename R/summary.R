#' Subset a list of Mplus model results
#'
#' a helper function to be used by wrappers that generate HTML, LaTex, and on-screen displays of summary statistics
#' @param modelList A list object of Mplus models
#' @param keepCols Columns to keep
#' @param dropCols Columns to drop (use only one of keep/dropCols)
#' @param sortBy How to sort. Defaults to \code{NULL}, which does not sort the list.
#' @return Extracted and sorted data
#' @keywords internal
#' @examples
#' # make me!!!
subsetModelList <- function(modelList, keepCols, dropCols, sortBy = NULL) {
  # only allow keep OR drop.
  if(!missing(keepCols) && !missing(dropCols)) stop("keepCols and dropCols passed to subsetModelList. You must choose one or the other, but not both.")
  
  if(missing(keepCols) && missing(dropCols)) {
    keepCols <- c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")
  }
  
  # if passed an mplus.model.list from readModels, then just extract summaries for display
  MplusData <- mplus_as_list(modelList)
  MplusData <- do.call("rbind.fill", sapply(MplusData, "[", "summaries"))
  
  #make a list of non-missing columns
  notMissing <- unlist(lapply(names(MplusData), function(column) {
    if(!all(is.na(MplusData[[column]]))) return(column)
  }))
  
  if(!is.null(sortBy)){
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
    MplusData <- MplusData[order(MplusData[[sortBy]]), notMissing, drop = FALSE]
  }
  # keep only columns specified by keepCols
  if (!missing(keepCols) && length(keepCols) > 0) {
    #check to make sure each column exists if keepCols used
    summaryNames <- names(MplusData)
    keepCols <- keepCols[keepCols %in% summaryNames]
    
    if (length(keepCols) == 0) stop("All fields passed as keepCols are missing from data.frame\n  Fields in data.frame are:\n  ",
                                    paste(strwrap(paste(summaryNames, collapse=" "), width=80, exdent=4), collapse="\n"))
    MplusData <- MplusData[, keepCols, drop = FALSE]
  }
  
  #drop columns specified by dropCols
  if (!missing(dropCols) && length(dropCols) > 0) {
    #Process vector of columns to drop
    for (column in dropCols) {
      MplusData[[column]] <- NULL
    }
    
  }
  return(MplusData)
}

summary_field_decimals <- function() {
  decimals <- c(
    Title = FALSE,
    Mplus.version = FALSE,
    AnalysisType = FALSE,
    DataType = FALSE,
    Filename = FALSE,
    Estimator = FALSE,
    Warnings = FALSE,
    Observations = FALSE,
    Parameters = FALSE,
    ParametersWithAux = FALSE,
    NGroups = FALSE,
    NDependentVars = FALSE,
    NIndependentVars = FALSE,
    NContinuousLatentVars = FALSE,
    NCategoricalLatentVars = FALSE,
    NumFactors = FALSE,
    ChiSqM_DF = FALSE,
    ChiSqBaseline_DF = FALSE,
    ChiSqCategoricalPearson_DF = FALSE,
    ChiSqCategoricalLRT_DF = FALSE,
    ChiSqMCARUnrestrictedPearson_DF = FALSE,
    ChiSqMCARUnrestrictedLRT_DF = FALSE,
    ChiSqDiffTest_DF = FALSE,
    ChiSqM_NumComputations = FALSE,
    LL_NumComputations = FALSE,
    UnrestrictedLL_NumComputations = FALSE,
    CFI_NumComputations = FALSE,
    TLI_NumComputations = FALSE,
    PostPred_PValue_NumComputations = FALSE,
    PriorPostPred_PValue_NumComputations = FALSE,
    AIC_NumComputations = FALSE,
    BIC_NumComputations = FALSE,
    aBIC_NumComputations = FALSE,
    RMSEA_NumComputations = FALSE,
    WRMR_NumComputations = FALSE,
    DIC_NumComputations = FALSE,
    pD_NumComputations = FALSE,
    SRMR_NumComputations = FALSE,
    SRMR.Within_NumComputations = FALSE,
    SRMR.Between_NumComputations = FALSE,
    T11_VLMR_ParamDiff = FALSE,
    BLRT_ParamDiff = FALSE,
    BLRT_SuccessfulDraws = FALSE,
    Classes = FALSE,
    min_N = FALSE,
    max_N = FALSE,
    min_prob = TRUE,
    max_prob = TRUE
  )

  return(decimals)
}

is_decimal_summary_field <- function(name, values = NULL) {
  decimals <- summary_field_decimals()

  if (name %in% names(decimals)) {
    return(unname(decimals[[name]]))
  }

  if (grepl("(_DF|_NumComputations|_SuccessfulDraws|_ParamDiff|_Starts|_Final)$", name)) {
    return(FALSE)
  }

  if (grepl("^N[A-Z]", name)) {
    return(FALSE)
  }

  if (!is.null(values) && is.integer(values)) {
    return(FALSE)
  }

  if (!is.null(values) && is.numeric(values)) {
    return(TRUE)
  }

  return(FALSE)
}

format_summary_display <- function(data, digits) {
  if (is.null(digits)) {
    return(data)
  }

  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0) {
    stop("digits must be a single non-negative number.")
  }

  digits <- as.integer(digits)
  formatted <- data

  for (nm in names(formatted)) {
    values <- formatted[[nm]]

    if (!is.numeric(values) || !is_decimal_summary_field(nm, values)) {
      next
    }

    formatted[[nm]] <- ifelse(
      is.na(values),
      NA_character_,
      formatC(values, digits = digits, format = "f")
    )
  }

  formatted
}

xtable_digits <- function(data, digits) {
  if (is.null(digits)) {
    return(NULL)
  }

  xtable_digits <- rep(0L, ncol(data) + 1L)
  digits <- as.integer(digits)

  for (i in seq_along(data)) {
    if (is.numeric(data[[i]]) && is_decimal_summary_field(names(data)[i], data[[i]])) {
      xtable_digits[i + 1L] <- digits
    }
  }

  xtable_digits
}

summary_xtable <- function(data, caption = NULL, label = NULL, digits = NULL) {
  if (is.null(digits)) {
    return(xtable(data, caption = caption, label = label))
  }

  xtable(data,
         caption = caption,
         label = label,
         digits = xtable_digits(data, digits))
}


#' Create a summary table of Mplus model statistics
#'
#' Creates output (optionally sent to a file) containing a summary table of model fit statistics
#' extracted using the \code{extractModelSummaries} function.
#' By default, the following summary statistics are included:
#' \code{Title, LL, Parameters, AIC, AICC, BIC, RMSEA_Estimate},
#' but these are customizable using the \code{keepCols} and \code{dropCols} parameters.
#'
#' @param modelList A list of models returned from the \code{extractModelSummaries} function.
#' @param type A character vector indicating the type of output format to be generated.  One of:
#'   \dQuote{none}, \dQuote{screen}, \dQuote{popup}, \dQuote{html}, \dQuote{latex}, or \dQuote{markdown}. 
#'   Screen results in a simple summary table being sent to the R console.
#' @param filename The name of the file to be created. Can be an absolute or relative path. If \code{filename}
#'   is a relative path or just the filename, then it is assumed that the file resides in the working
#'   directory \code{getwd()}. Example: \code{"Mplus Summary.html"}.  By default, no filename is given,
#'   which results in the output being sent to the console.  Note that currently, filename only has an effect for
#'   \dQuote{html} and \dQuote{latex}.
#' @param keepCols A vector of character strings indicating which columns/variables to display in the summary.
#'   Only columns included in this list will be displayed (all others excluded). By default, \code{keepCols}
#'   is: \code{c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")}.
#'   Example: \code{c("Title", "LL", "AIC", "CFI")}
#' @param dropCols A vector of character strings indicating which columns/variables to omit from the summary.
#'   Any column not included in this list will be displayed. By default, \code{dropCols} is \code{NULL}.
#'   Example: \code{c("InputInstructions", "TLI")}
#' @param sortBy optional. Field name (as character string) by which to sort the table. Typically an information criterion
#'   (e.g., "AIC" or "BIC") is used to sort the table.
#'   Defaults to \code{NULL}, which does not sort the table.
#' @param caption A character string, the caption to be given to the table.  Currently only
#'   applies to types \dQuote{html}, \dQuote{latex}, and \dQuote{markdown}.
#' @param display optional logical (defaults to \code{FALSE}). This parameter specifies whether to display the
#'   table upon creation (\code{TRUE} or \code{FALSE}).
#' @param digits optional. Number of digits to display for decimal-valued summary
#'   fields. This affects rendered output for screen, popup, markdown, html,
#'   and latex formats, but \code{type = "none"} always returns the raw numeric
#'   values.
#' @param \ldots additional arguments passed on to specific formatting types.
#' @param include.rownames optional logical whether to include rownames or not.
#' @return Invisibly returns the summary table, which can be used if the printing options avaiable are not sufficient.
#' @author Joshua F. Wiley based on code by Michael Hallquist
#' @note You must choose between \code{keepCols} and \code{dropCols} because it is not sensible to use these
#'   together to include and exclude columns. The function will error if you include both parameters.
#' @seealso \code{\link{extractModelSummaries}}
#' @export
#' @importFrom xtable xtable
#' @importFrom pander pander
#' @keywords interface
#' @examples
#' \dontrun{
#'  m1 <- mplusObject(TITLE = "Reduced",
#'   MODEL = "mpg ON wt;", rdata = mtcars)
#'  m1.fit <- mplusModeler(m1, "mtcars.dat", run = 1L)
#'  m2 <- mplusObject(TITLE = "Full",
#'   MODEL = "mpg ON wt hp qsec;", rdata = mtcars)
#'  m2.fit <- mplusModeler(m2, "mtcars.dat", run = 1L)
#'
#'  SummaryTable(list(m1.fit, m2.fit))
#'  SummaryTable(list(m1.fit, m2.fit), type = "popup")
#'  SummaryTable(list(m1.fit, m2.fit), type = "markdown",
#'    keepCols = c("Title", "Parameters", "LL", "AIC", "CFI", "SRMR"),
#'    caption = "Table of Model Fit Statistics",
#'    split.tables = 200)
#'
#'  # remove files
#'  unlink("mtcars.dat")
#'  unlink("mtcars.inp")
#'  unlink("mtcars.out")
#'  unlink("Mplus Run Models.log")
#'  closeAllConnections()
#' }
SummaryTable <- function(modelList, type = c("none", "screen", "popup", "html", "latex", "markdown"),
                         filename = "", keepCols, dropCols, sortBy = NULL, caption = "",
                         display = FALSE, digits = NULL, ..., include.rownames = FALSE) {
  type <- match.arg(type)
  
  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)
  displayData <- format_summary_display(MplusData, digits)
  
  if (!include.rownames) {
    rownames(MplusData) <- NULL
    rownames(displayData) <- NULL
  }
  
  if (nzchar(filename)) {
    if (length(grep("[\\/]", filename)) == 0) {
      #Filename does not contain a path. Therefore, add the working directory
      filename <- file.path(getwd(), filename)
    }
  }
  
  switch(type,
         none = return(MplusData),
         screen = print(displayData),
         popup = {
           if (!suppressWarnings(requireNamespace("relimp"))) {
             stop("The relimp package is absent. Interactive folder selection cannot function.")
           }
           relimp::showData(displayData,
                            placement = "+30+30",
                            maxwidth = 150,
                            maxheight = 50,
                            rownumbers = include.rownames,
                            title = "Mplus Summary Table", ...)
         }, 
         html = print(summary_xtable(MplusData, caption = caption, digits = digits),
                      type = "html",
                      file = filename,
                      include.rownames = include.rownames,
                      NA.string = ".", ...),
         latex = print(summary_xtable(MplusData, caption = caption, digits = digits),
                       type = "latex",
                       file = filename,
                       include.rownames = include.rownames,
                       NA.string = ".", ...),
         markdown = pander(displayData,
                           caption = caption,
                           ...)
  )
  
  if (display && type %in% c("html", "latex")) {
    shell.exec(paste0("file:///", filename))
  }
  
  return(invisible(MplusData))
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
#'   Typically an information criterion (e.g., \dQuote{AIC} or \dQuote{BIC}) is used to sort the table.
#'   Defaults to \code{NULL}, which does not sort the table.
#' @param font Optional. The font to be used to display the summary table. Defaults to Courier 9.
#'
#' @return No value is returned by this function. It is solely used to display the summary table in a separate window.
#' @author Michael Hallquist
#' @seealso \code{\link{extractModelSummaries}} \code{\link{HTMLSummaryTable}} \code{\link{LatexSummaryTable}}
#' @export
#' @keywords interface
#' @examples
#' # make me!!!
showSummaryTable <- function(modelList, keepCols, dropCols, sortBy = NULL, font="Courier 9") {
  if (!suppressWarnings(requireNamespace("relimp"))) {
    stop("The relimp package is absent. Interactive folder selection cannot function.")
  }
  
  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)
  relimp::showData(MplusData, font=font, placement="+30+30", maxwidth=150, maxheight=50, rownumbers=FALSE, title="Mplus Summary Table")
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
#'   (e.g., "AIC" or "BIC") is used to sort the table. 
#'   Defaults to \code{NULL}, which does not sort the table.
#' @param display optional. This parameter specifies whether to display the table in a web
#'   browser upon creation (\code{TRUE} or \code{FALSE}).
#' @param digits optional. Number of digits to display for decimal-valued summary
#'   fields in the rendered HTML table.
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
HTMLSummaryTable <- function(modelList, filename=file.path(getwd(), "Model Comparison.html"), keepCols, dropCols, sortBy = NULL, display=FALSE, digits = NULL) {
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
  
  print(x=summary_xtable(MplusData, digits = digits),
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
#'   Typically an information criterion (e.g., "AIC" or "BIC") is used to sort the table.
#'   Defaults to \code{NULL}, which does not sort the table.
#' @param label optional. A character string specifying the label for the LaTex table, which can be
#'   used for referencing the table.
#' @param caption optional. A character string specifying the caption for the LaTex table.
#' @param digits optional. Number of digits to display for decimal-valued summary
#'   fields in the rendered LaTeX table.
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
LatexSummaryTable <- function(modelList, keepCols, dropCols, sortBy = NULL, label=NULL, caption=NULL, digits = NULL) {
  #return latex table to caller
  
  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)
  
  return(summary_xtable(MplusData, label=label, caption=caption, digits = digits))
}
