#' Extract Parameters for One Chunk
#'
#' Helper function for extractModelParameters. Used to parse each subsection of
#' output within a given file and given results section (e.g., stdyx section) There
#' will be many chunks if latent classes, multiple groups, multilevel features are used.
#'
#' @param filename
#' @param thisCunk
#' @param columnNames
#' @return A data frame (or matrix?)
#' @keywords internal
extractParameters_1chunk <- function(filename, thisChunk, columnNames) {
  if (missing(thisChunk) || is.na(thisChunk) || is.null(thisChunk)) stop("Missing chunk to parse.\n  ", filename)
  if (missing(columnNames) || is.na(columnNames) || is.null(columnNames)) stop("Missing column names for chunk.\n  ", filename)

  #okay to match beginning and end of line because strip.white used in scan
  matches <- gregexpr("^\\s*((Means|Thresholds|Intercepts|Variances|Item Difficulties|Residual Variances|New/Additional Parameters|Scales|Dispersion)|([\\w_\\d+\\.#]+\\s+(BY|WITH|ON|\\|)))\\s*$", thisChunk, perl=TRUE)

  #more readable (than above) using ldply from plyr
  convertMatches <- ldply(matches, function(row) data.frame(start=row, end=row+attr(row, "match.length")-1))

  #beware faulty logic below... assumes only one match per line (okay here)
  convertMatches$startline <- 1:nrow(convertMatches)

  #only keep lines with a single match
  #this removes rows that are -1 from gregexpr
  convertMatches <- subset(convertMatches, start > 0)

  #sometimes chunks have no parameters because they are empty. e.g., stdyx for ex7.30
  #in this case, return null
  if (nrow(convertMatches)==0) return(NULL)

  #develop a dataframe that divides into keyword matches versus variable matches
  convertMatches <- ddply(convertMatches, "startline", function(row) {
        #pull the matching keyword based on the start/end attributes from gregexpr
        match <- substr(thisChunk[row$startline], row$start, row$end)

        #check for keyword
        if (match %in% c("Means", "Thresholds", "Intercepts", "Variances", "Residual Variances", "New/Additional Parameters", "Scales", "Item Difficulties", "Dispersion")) {
          return(data.frame(startline=row$startline, keyword=make.names(match), varname=NA_character_, operator=NA_character_))
        }
        else if (length(variable <- strapply(match, "^\\s*([\\w_\\d+\\.#]+)\\s+(BY|WITH|ON|\\|)\\s*$", c, perl=TRUE)[[1]]) > 0) {
          return(data.frame(startline=row$startline, keyword=NA_character_, varname=variable[1], operator=variable[2]))
        }
        else stop("failure to match keyword: ", match, "\n  ", filename)
      })

  comboFrame <- c()

  #convertMatches will now contain a data.frame marking the section headers for the chunk
  #example:
  #		startline keyword varname operator endline
  #		        7    <NA>      FW       BY      12
  #		       13    <NA>      FW       ON      16

  for (i in 1:nrow(convertMatches)) {
    #define the end line for this match as the start of next match - 1
    if (i < nrow(convertMatches)) convertMatches[i,"endline"] <- convertMatches[i+1,"startline"]-1
    else convertMatches[i,"endline"] <- length(thisChunk) # or if last chunk in the section, just define as length

    #need +1 to eliminate header row from params
    paramsToParse <- thisChunk[(convertMatches[i, "startline"]+1):convertMatches[i, "endline"]]

    #should result in a short list of params to parse (that belong to a given header i)
    #Example:
    #"U1                 0.557      0.036     15.470      0.000"
    #"U2                 0.638      0.038     16.751      0.000"
    #"U3                 0.660      0.038     17.473      0.000"
    #"U4                 0.656      0.037     17.585      0.000"

    #define the var title outside of the chunk processing because it will apply to all rows
    if (is.na(convertMatches[i,]$keyword)) varTitle <- paste(convertMatches[i,"varname"], ".", convertMatches[i,"operator"], sep="")
    else varTitle <- as.character(convertMatches[i,"keyword"])

    splitParams <- strsplit(paramsToParse, "\\s+", perl=TRUE)

    #for the Significance column in 7-column Mplus output, it may be missing for a chunk (all n.s./not tested), or for a given row.
    #Handle this condition here by adding FALSE for missing 7th column and converting * to TRUE.
    if (length(columnNames) == 7L && columnNames[7L] == "sig") {
      splitParams <- lapply(splitParams, function(col) {
            lcol <- length(col)
            if (lcol == 6L) { col[7L] <- "FALSE"
            } else if (lcol == 7L && col[7L] == "*") { col[7L] <- "TRUE"
            } else if (lcol != 0) { warning("Unknown columns found for 7-column BAYES format") }
            return(col)

          })
    }

    #rbind the split list as a data.frame
    parsedParams <- data.frame(do.call("rbind", splitParams), stringsAsFactors=FALSE)

    #for each column, convert to numeric if it is. Otherwise, return as character
    parsedParams <- data.frame(lapply(parsedParams, function(col) {
              #a bit convoluted, but we want to test for a purely numeric string by using a regexp that only allows numbers, periods, and the minus sign
              #then sum the number of matches > 0 (i.e., where a number was found).
              #if the sum is the same as the length of the column, then all elements are purely numeric.
              if (all(col %in% c("TRUE", "FALSE"))) return(as.logical(col)) #True/False significance check above
              else if (sum(sapply(gregexpr("^[\\d\\.-]+$", col, perl=TRUE), "[", 1) > 0) == length(col)) return(as.numeric(col))
              else return(as.character(col))
            }), stringsAsFactors=FALSE)


    #use the column names detected in extractParameters_1section
    names(parsedParams) <- columnNames

    #add the paramHeader to the data.frame
    parsedParams$paramHeader <- varTitle

    #put the paramHeader at the front of the data.frame columns
    parsedParams <- parsedParams[,c("paramHeader", columnNames)]

    #add the current chunk to the overall data.frame
    comboFrame <- rbind(comboFrame, parsedParams)

  }

  #under the new strsplit strategy, just return the dataframe
  return(comboFrame)

}

#' Extract Parameters for One Section
#'
#' To do: add details
#'
#' @param filename
#' @param modelSection
#' @param sectionName
#' @return A list of parameters
#' @keywords internal
#' @examples
#' \dontrun{
#'   #a few examples of files to parse
#'   #mg + lc. Results in latent class pattern, not really different from 
#'   #         regular latent class matching. See Example 7.21
#'   #mg + twolevel. Group is top, bw/wi is 2nd. See Example 9.11
#'   #lc + twolevel. Bw/wi is top, lc is 2nd. See Example 10.1.
#'   #               But categorical latent variables is even higher
#'   #test cases for more complex output: 7.21, 9.7, 9.11, 10.1
#' }
extractParameters_1section <- function(filename, modelSection, sectionName) {
  #extract model parameters for a given model results section. A section contains complete output for all parameters of a given type
  #(unstandardized, ci, stdyx, stdy, or std) for a single file.
  #section name is used to name the list element of the returned list

  #first trim all leading and trailing spaces (new under strip.white=FALSE)
  modelSection <- gsub("(^\\s+|\\s+$)", "", modelSection, perl=TRUE)

  #detectColumn names sub-divides (perhaps unnecessarily) the matches based on the putative section type of the output
  #current distinctions include modification indices, confidence intervals, and model results.
  if (sectionName %in% c("ci.unstandardized", "ci.stdyx.standardized", "ci.stdy.standardized", "ci.std.standardized")) { sectionType <- "confidence_intervals"
  } else if (sectionName == "irt.parameterization" || sectionName == "probability.scale") {
    #the IRT section follows from the MODEL RESULTS section, and column headers are not reprinted.
    #Same applies for RESULTS IN PROBABILITY SCALE section
    #Thus, for now (first pass), assume a 5-column header -- kludge
    columnNames <- c("param", "est", "se", "est_se", "pval")
  } else { sectionType <- "model_results" }

  if (!exists("columnNames")) { columnNames <- detectColumnNames(filename, modelSection, sectionType) }

  #Detect model section dividers
  #These include: 1) multiple groups: Group XYZ
  #  2) latent classes: Latent Class XYZ
  #  3) two-level structure: Between Level, Within Level
  #  4) categorical latent variables: Categorical Latent Variables
  #  5) class proportions (only known class output?)

  allSectionParameters <- c() #will hold extracted params for all sections

  betweenWithinMatches <- grep("^\\s*(Between|Within) Level\\s*$", modelSection, ignore.case=TRUE, perl=TRUE)
  latentClassMatches <- grep("^\\s*Latent Class (Pattern )*(\\d+\\s*)+(\\(\\s*\\d+\\s*\\))*$", modelSection, ignore.case=TRUE, perl=TRUE)
  multipleGroupMatches <- grep("^\\s*Group \\w+\\s*$", modelSection, ignore.case=TRUE, perl=TRUE)
  catLatentMatches <- grep("^\\s*Categorical Latent Variables\\s*$", modelSection, ignore.case=TRUE)
  classPropMatches <- grep("^\\s*Class Proportions\\s*$", modelSection, ignore.case=TRUE)

  topLevelMatches <- sort(c(betweenWithinMatches, latentClassMatches, multipleGroupMatches, catLatentMatches, classPropMatches))

  if (length(topLevelMatches) > 0) {

    lcNum <- NULL
    bwWi <- NULL
    groupName <- NULL

    matchIndex <- 1
    for (match in topLevelMatches) {

      if (match %in% betweenWithinMatches) bwWi <- sub("^\\s*(Between|Within) Level\\s*$", "\\1", modelSection[match], perl=TRUE)
      else if (match %in% latentClassMatches) {
        if ((pos <- regexpr("Pattern", modelSection[match], ignore.case=TRUE)) > 0) {
          #need to pull out and concatenate all numerical values following pattern
          postPattern <- trimSpace(substr(modelSection[match], pos + attr(pos, "match.length"), nchar(modelSection[match])))
          #replace any spaces with periods to create usable unique lc levels
          lcNum <- gsub("\\s+", "\\.", postPattern, perl=TRUE)
        }
        else lcNum <- sub("^\\s*Latent Class\\s+(\\d+)\\s*(\\(\\s*\\d+\\s*\\))*$", "\\1", modelSection[match], perl=TRUE)
      }
      else if (match %in% multipleGroupMatches) groupName <- sub("^\\s*Group (\\w+)\\s*$", "\\1", modelSection[match], perl=TRUE)
      else if (match %in% catLatentMatches) {
        #the categorical latent variables section is truly "top level"
        #that is, it starts over in terms of bw/wi and latent classes
        #multiple groups with cat latent variables is handled by knownclass and results in a latent class
        #pattern, so don't have to worry about nullifying groupName
        lcNum <- "Categorical.Latent.Variables"
        bwWi <- NULL
      }
      else if (match %in% classPropMatches) {
        #the class proportions section is truly "top level"
        #that is, it starts over in terms of bw/wi and latent classes
        #multiple groups with cat latent variables is handled by knownclass and results in a latent class
        #pattern, so don't have to worry about nullifying groupName
        lcNum <- "Class.Proportions"
        bwWi <- NULL

        #N.B.: 15Mar2012. the parse chunk routine can't handle the class proportions output right now
        #because there is no nesting. Need to come back and fix.
        #for now, this output is just ignored.
      }

      #if the subsequent top level match is more than 2 lines away, assume that there is a
      #chunk to be parsed. If it's <= 2, then assume that these are just blank lines
      chunkToParse <- FALSE
      if (matchIndex < length(topLevelMatches) &&
          (topLevelMatches[matchIndex + 1] - topLevelMatches[matchIndex]) > 2) {

        #extract all text between this match and the next one (add one to omit this header row,
        #subtract one to exclude the subsequent header row)
        thisChunk <- modelSection[(match+1):(topLevelMatches[matchIndex+1]-1)]
        chunkToParse <- TRUE
      }
      else if (matchIndex == length(topLevelMatches) && match+1 <= length(modelSection)) {
        #also assume that the text following the last topLevelMatch is also to be parsed
        #second clause ensures that there is some chunk below the final header.
        #this handles issues where a blank section terminates the results section, such as multilevel w/ no between
        thisChunk <- modelSection[(match+1):length(modelSection)]
        chunkToParse <- TRUE
      }

      if (chunkToParse == TRUE) {
        parsedChunk <- extractParameters_1chunk(filename, thisChunk, columnNames)

        #only append if there are some rows
        if (!is.null(parsedChunk) && nrow(parsedChunk) > 0) {
          parsedChunk$LatentClass <- lcNum
          parsedChunk$BetweenWithin <- bwWi
          parsedChunk$Group <- groupName
          allSectionParameters <- rbind(allSectionParameters, parsedChunk)
        }
      }

      matchIndex <- matchIndex + 1
    }

  }
  else allSectionParameters <- extractParameters_1chunk(filename, modelSection, columnNames) #just one model section

  #if any std variable is one of the returned columns, we are dealing with an old-style combined results section (i.e.,
  #standardized results are not divided into their own sections, as with newer output).
  #newer output would just have the params, est, etc.
  #for consistency with newer output, we need to parse these into individual list elements and remove from unstandardized output.
  #this is a tricky maneuver in some ways because the function may return a data.frame or a list... will have to be handled by the caller
  oldStyleColumns <- c("stdyx", "stdy", "std")
  listParameters <- list()

  if (any(oldStyleColumns %in% names(allSectionParameters))) {

    #for each standardized column present, reprocess into its own df, append to list, and remove from the df
    for (colName in oldStyleColumns[oldStyleColumns %in% names(allSectionParameters)]) {
      listParameters[[paste0(colName, ".standardized")]] <- data.frame(paramHeader=allSectionParameters$paramHeader,
          param=allSectionParameters$param, est=allSectionParameters[,colName], stringsAsFactors=FALSE)

      #also include latent class, multiple groups and bw/wi in the output
      if ("LatentClass" %in% names(allSectionParameters)) listParameters[[paste0(colName, ".standardized")]]$LatentClass <- allSectionParameters$LatentClass
      if ("Group" %in% names(allSectionParameters)) listParameters[[paste0(colName, ".standardized")]]$Group <- allSectionParameters$Group
      if ("BetweenWithin" %in% names(allSectionParameters)) listParameters[[paste0(colName, ".standardized")]]$BetweenWithin <- allSectionParameters$BetweenWithin

      allSectionParameters[[colName]] <- NULL #remove from unstandardized output

    }
    listParameters[[sectionName]] <- allSectionParameters #now that standardized removed, add remainder to the list under appropriate name
  }
  else {
    #if output only contains results of one section type (stdyx, unstandardized, etc.),
    #then return a list with a single element, which will be appended to other elements by the extractParameters_1file function
    #copy data.frame into the appropriate list element to be returned.
    listParameters[[sectionName]] <- allSectionParameters

  }

  #tag as mplusParams class
  listParameters <- lapply(listParameters, function(x) {
        class(x) <- c("data.frame", "mplus.params")
        attr(x, "filename") <- filename
        return(x)
      })
  return(listParameters)
}

#' Extract Parameters for One File
#'
#' To do: add details
#'
#' @param filename
#' @param modelSection
#' @param sectionName
#' @return A list of parameters
#' @keywords internal
extractParameters_1file <- function(outfiletext, filename, resultType) {

  if (length(grep("TYPE\\s+(IS|=|ARE)\\s+((MIXTURE|TWOLEVEL)\\s+)+EFA\\s+\\d+", outfiletext, ignore.case=TRUE, perl=TRUE)) > 0) {
    warning(paste0("EFA, MIXTURE EFA, and TWOLEVEL EFA files are not currently supported by extractModelParameters.\n  Skipping outfile: ", filename))
    return(NULL) #skip file
  }

  # copy elements of append into target. note that data.frames inherit list,
  # so could be wonky if append is a data.frame (shouldn't happen here)
  appendListElements <- function(target, append) {
    if (!is.list(target)) stop("target is not a list.")
    if (!is.list(append)) stop("append is not a list.")

    for (elementName in names(append)) {
      if (!is.null(target[[elementName]])) warning("Element is already present in target list: ", elementName)
      target[[elementName]] <- append[[elementName]]
    }

    return(target)
  }

  allSections <- list() #holds parameters for all identified sections
  unstandardizedSection <- getSection("^MODEL RESULTS$", outfiletext)
  if (!is.null(unstandardizedSection)) {
    allSections <- appendListElements(allSections, extractParameters_1section(filename, unstandardizedSection, "unstandardized"))
  }

  standardizedSection <- getSection("^STANDARDIZED MODEL RESULTS$", outfiletext)

  if (!is.null(standardizedSection)) {
    # check to see if standardized results are divided by standardization type (new format)

    # probably somewhat kludgy to use the blanklines code here, but it gets the job done
    # ultimately probably better to search for the three sections, split them, etc.
    # gregexpr("STD[YX]*Standardization", capsLine, perl=TRUE)

    stdYXSection <- getSection_Blanklines("^STDYX Standardization$", standardizedSection)
    if (!is.null(stdYXSection)) {
      allSections <- appendListElements(allSections, extractParameters_1section(filename, stdYXSection, "stdyx.standardized"))
    }

    stdYSection <- getSection_Blanklines("^STDY Standardization$", standardizedSection)
    if (!is.null(stdYSection)) {
      allSections <- appendListElements(allSections, extractParameters_1section(filename, stdYSection, "stdy.standardized"))
    }

    stdSection <- getSection_Blanklines("^STD Standardization$", standardizedSection)
    if (!is.null(stdSection)) {
      allSections <- appendListElements(allSections, extractParameters_1section(filename, stdSection, "std.standardized"))
    }

    #if all individual standardized sections are absent, but the standardized section is present, must be old-style
    #combined standardized section (affects WLS and MUML, too). Extract and process old section.
    if (all(is.null(stdYXSection), is.null(stdYSection), is.null(stdSection))) {
      # this section name should never survive the call
      allSections <- appendListElements(allSections, extractParameters_1section(filename, standardizedSection, "standardized"))
    }

  }

  #two-parameter IRT output
  irtSection <- getSection("^IRT PARAMETERIZATION IN TWO-PARAMETER (PROBIT|LOGISTIC) METRIC$", outfiletext)
  if (!is.null(irtSection)) {
    #parse what the logit or probit is
    probitLogit <- tolower(sub("^\\s*where the (probit|logit) is.*$", "\\1", irtSection[1L], ignore.case=TRUE, perl=TRUE))
    def <- tolower(sub("^\\s*where the (?:probit|logit) is\\s+(.*)$", "\\1", irtSection[1L], ignore.case=TRUE, perl=TRUE))
    irtSection <- irtSection[2:length(irtSection)] #drop line "WHERE THE LOGIT IS 1.7*DISCRIMINATION*(THETA - DIFFICULTY)"
    irtParsed <- extractParameters_1section(filename, irtSection, "irt.parameterization")
    attr(irtParsed[["irt.parameterization"]], probitLogit) <- def #add probit/logit definition as attribute
    allSections <- appendListElements(allSections, irtParsed)
  }
  
#  probSection <- getSection("^RESULTS IN PROBABILITY SCALE$", outfiletext)
#  if (!is.null(probSection)) {
#    probParsed <- extractParameters_1section(filename, probSection, "probability.scale")
#    allSections <- appendListElements(allSections, probParsed)
#  }
  
  #confidence intervals for usual output, credibility intervals for bayesian output
  ciSection <- getSection("^(CONFIDENCE INTERVALS OF MODEL RESULTS|CREDIBILITY INTERVALS OF MODEL RESULTS)$", outfiletext)
  if (!is.null(ciSection)) {
    allSections <- appendListElements(allSections, extractParameters_1section(filename, ciSection, "ci.unstandardized"))
  }

  ciStdSection <- getSection("^(CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS|CREDIBILITY INTERVALS OF STANDARDIZED MODEL RESULTS)$", outfiletext)
  if (!is.null(ciStdSection)) {
    stdsections <- c("STDYX Standardization", "STDY Standardization", "STD Standardization")
    stdyx.section <- getSection("STDYX Standardization", ciStdSection, headers=stdsections)
    if (!is.null(stdyx.section)) { allSections <- appendListElements(allSections, extractParameters_1section(filename, stdyx.section, "ci.stdyx.standardized")) }

    stdy.section <- getSection("STDY Standardization", ciStdSection, headers=stdsections)
    if (!is.null(stdy.section)) allSections <- appendListElements(allSections, extractParameters_1section(filename, stdy.section, "ci.stdy.standardized"))

    std.section <- getSection("STD Standardization", ciStdSection, headers=stdsections)
    if (!is.null(std.section)) allSections <- appendListElements(allSections, extractParameters_1section(filename, std.section, "ci.std.standardized"))
  }

  # cleaner equivalent of above
  listOrder <- c("unstandardized", "ci.unstandardized",
      "irt.parameterization", "probability.scale",
      "stdyx.standardized", "ci.stdyx.standardized",
      "stdy.standardized", "ci.stdy.standardized",
      "std.standardized", "ci.std.standardized")
  listOrder <- listOrder[listOrder %in% names(allSections)]


  #only re-order if out of order
  if(!identical(names(allSections), listOrder)) allSections <- allSections[listOrder]

  #this needs to be here not to conflict with the drop to 1 element logic above.
  #if resultType passed (deprecated), only return the appropriate element
  #this is inefficient because all sections will be parsed, but it's deprecated, so no worries.
  if (!missing(resultType)) {
    warning(paste("resultType is deprecated and will be removed in a future version.\n  ",
            "extractModelParameters now returns a list containing unstandardized and standardized parameters, where available.\n  ",
            "For now, resultType is respected, so a data.frame will be returned."))

    oldNewTranslation <- switch(EXPR = resultType,
        raw   = "unstandardized",
        stdyx = "stdyx.standardized",
        stdy  = "stdy.standardized",
        std   = "std.standardized")

    allSections <- allSections[[oldNewTranslation]]
  }

  return(allSections)
}

#' Extract model parameters from MODEL RESULTS section.
#'
#' Extracts the model parameters from the MODEL RESULTS section of one or more Mplus output files.
#' If a particular output file has more than one results section (unstandardized, stdyx, stdy, and/or std),
#' a list will be returned. If the \code{target} is a directory, all .out files therein will be parsed
#' and a single list will be returned, where the list elements are named by the output file name.
#' Returned parameters often include the parameter estimate, std. err, param/s.e., and two-tailed p-value.
#'
#' @param target the directory containing Mplus output files (.out) to parse OR the single output file to
#'   be parsed. May be a full path, relative path, or a filename within the working directory.
#'   Defaults to the current working directory. Example: \dQuote{C:/Users/Michael/Mplus Runs}
#' @param recursive optional. If \code{TRUE}, parse all models nested in subdirectories
#'   within \code{target}. Defaults to \code{FALSE}.
#' @param filefilter a Perl regular expression (PCRE-compatible) specifying particular output
#'   files to be parsed within \code{directory}. See \code{regex} or \url{http://www.pcre.org/pcre.txt}
#'   for details about regular expression syntax.
#' @param dropDimensions Relevant only for multi-file parsing. If \code{TRUE}, then if only one output
#'   section (usually unstandardized) is present for all files in the parsed list, then eliminate
#'   the second-level list (which contains elements for each output section). The result is
#'   that the elements of the returned list are \code{data.frame} objects with the relevant parameters.
#' @param resultType N.B.: this parameter is deprecated and will be removed in a future version. The
#'   new default is to extract all results that are present and return a list (see below for details).
#'   \code{resultType} specified the results section to extract. If \code{raw}, the unstandardized
#'   estimates will be returned. \dQuote{stdyx}, \dQuote{stdy}, and \dQuote{std}
#'   are the other options, which extract different standardized solutions.
#'   See the Mplus User's Guide for additional details about the differences in these standardizations.
#'
#' @return If \code{target} is a single file, a list containing unstandardized and standardized results will be
#' returned. If all standardized solutions are available, the list element will be named: \code{unstandardized},
#' \code{stdyx.standardized}, \code{stdy.standardized}, and \code{std.standardized}. If confidence intervals
#' are output using OUTPUT:CINTERVAL, then a list element named \code{ci.unstandardized} will be included.
#' Each of these list elements is a \code{data.frame} containing relevant model parameters.
#'
#' If \code{target} is a directory, a list will be returned, where each element contains the results for
#' a single file, and the top-level elements are named after the corresponding output file name. Each
#' element within this list is itself a list, with elements as in the single file case above.
#'
#' The core \code{data.frame} for each MODEL RESULTS section typically has the following structure:
#'
#' \item{paramHeader}{The header that begins a given parameter set. Example: "FACTOR1 BY"}
#' \item{param}{The particular parameter being measured (within \code{paramHeader}). Example: "ITEM1"}
#' \item{est}{Parameter estimate value.}
#' \item{se}{Standard error of the estimate}
#' \item{est_se}{Quotient of \code{est/se}, representing z-test/t-test in large samples}
#' \item{pval}{Two-tailed p-value for the \code{est_se} quotient.}
#'
#' In the case of output from Bayesian estimation (ESTIMATOR=BAYES), the \code{data.frame} will contain
#' a different set of variables, including some of the above, as well as
#' \item{posterior_sd}{Posterior standard deviation of the estimate.}
#' \item{lower_2.5ci}{Lower 2.5 percentile of the estimate.}
#' \item{upper_2.5ci}{Upper 2.5 percentile (aka 97.5 percentile) of the estimate.}
#'
#' Also note that the \code{pval} column for Bayesian output represents a one-tailed estimate.
#'
#' In the case of output from a Monte Carlo study (MONTECARLO: and MODEL POPULATION:), the \code{data.frame} will contain
#' a different set of variables, including some of the above, as well as
#' \item{population}{Population parameter value.}
#' \item{average}{Average parameter estimate across replications.}
#' \item{population_sd}{Standard deviation of parameter value in population across replications.}
#' \item{average_se}{Average standard error of estimated parameter value across replications.}
#' \item{mse}{Mean squared error.}
#' \item{cover_95}{Proportion of replications whose 95\% confidence interval for the parameter includes the population value.}
#' \item{pct_sig_coef}{Proportion of replications for which the two-tailed significance test of the parameter is significant (p < .05).}
#'
#' In the case of confidence interval output (OUTPUT:CINTERVAL), the list element \code{ci.unstandardized} will contain
#' a different set of variables, including some of the above, as well as
#' \item{low.5}{Lower 0.5\% CI estimate.}
#' \item{low2.5}{Lower 2.5\% CI estimate.}
#' \item{low5}{Lower 5\% CI estimate.}
#' \item{est}{Parameter estimate value.}
#' \item{up5}{Upper 5\% (i.e., 95\%) CI estimate.}
#' \item{up2.5}{Upper 2.5\% (i.e., 97.5\%) CI estimate.}
#' \item{up.5}{Upper 0.5\% (i.e., 99.5\%) CI estimate.}
#'
#' If the model contains multiple latent classes, an additional variable, \code{LatentClass},
#' will be included, specifying the latent class number. Also, the Categorical Latent Variables section
#' will be included as \code{LatentClass} "Categorical.Latent.Variables."
#'
#' If the model contains multiple groups, \code{Group} will be included.
#'
#' If the model contains two-level output (between/within), \code{BetweenWithin} will be included.
#'
#'
#' @author Michael Hallquist
#' @seealso \code{\link{extractModelSummaries}}
#' @export
#' @keywords interface
#' @examples
#' \dontrun{
#' ex3.14 <- extractModelParameters(
#' 	"C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/ex3.14.out")
#' }
extractModelParameters <- function(target=getwd(), recursive=FALSE, filefilter, dropDimensions=FALSE, resultType) {

  #function tree (top to bottom):
  #extractModelParameters: loop over one or more output files
  #extractParameters_1file: extract model parameters for all sections (unstandardized, stdyx, stdy, std in a single file
  #extractParameters_1section: extract model parameters for a given section.
  #extractParameters_1chunk: extract model parameters for a given chunk (e.g., Latent class 2, Between Level) within a given section.

  outfiles <- getOutFileList(target, recursive, filefilter)

  allFiles <- list()
  for (curfile in outfiles) {
    #if not recursive, then each element is uniquely identified (we hope!) by filename alone
    if (recursive==FALSE)	listID <- make.names(splitFilePath(curfile)$filename) #each list element is named by the respective file
    else listID <- make.names(curfile) #each list element is named by the respective file

    outfiletext <- scan(curfile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE, quiet=TRUE)

    allFiles[[listID]] <- extractParameters_1file(outfiletext, curfile, resultType)
  }


  #dropDimensions <- TRUE
  if (length(allFiles) == 1) allFiles <- allFiles[[1]] # when only extracting a single file, return just the parameters list for the single model
  else if (dropDimensions == TRUE) {
    #in the case of multi-file output, we want to ensure that the interior lists (which contain model sections like stdyx.standardized)
    #all have a similar structure. But if all of them have only one element
    allNames <- sapply(allFiles, names)
    allLengths <- sapply(allNames, length)

    #if there is only one unique name in the bunch and all sub-list lengths are 1, then collapse
    #could probably just check for one unique name.
    if (length(unique(unlist(allLengths))) == 1 && length(unique(unlist(allNames))) == 1) {
      allFiles <- sapply(allFiles, "[", 1)
    }
#		nameLengths <- sapply(allNames, length)
#		names(nameLengths) <- NULL
#		numUniqueLengths <- length(unique(nameLengths))
#		if (numUniqueLengths == 1) {
#			#all files in the model results list have the same number of elements
#			#need to check for identical names
#
#		}
  }

  return(allFiles)
}
