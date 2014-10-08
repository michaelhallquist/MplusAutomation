#' Read Parameters, Summary Statistics, and Savedata from Mplus Output
#'
#' Extracts the model parameters, summary statistics, and savedata from the one or more Mplus output files.
#' This function is essentially a wrapper around extractModelParameters, extractModelSummaries, and getSavedata_Data,
#' respectively. The goal is to have a single function that parses all (supported) aspects of Mplus output and to combine
#' these into a list object, with one element per output file identified.
#'
#' @param target the directory containing Mplus output files (.out) to
#'   	parse OR the single output file to be parsed. May be a full
#'   	path, relative path, or a filename within the working
#'   	directory. Defaults to the current working directory. Example:
#'   	"C:/Users/Michael/Mplus Runs"
#' @param recursive optional. If \code{TRUE}, parse all models nested in
#'   	subdirectories within \code{target}. Defaults to \code{FALSE}.
#' @param filefilter a Perl regular expression (PCRE-compatible)
#'   	specifying particular output files to be parsed within
#'   	\code{directory}. See \code{regex} or
#'   	\url{http://www.pcre.org/pcre.txt} for details about regular
#'   	expression syntax.
#'
#' @return A list with one element per file. Each element is composed of
#' up to three subelements: summaries, parameters, and savedata. If
#' \code{target} is a single file, then the top-level elements will be
#' the summaries, parameters, and savedata, not a list of files.
#'   \item{summaries}{Summary statistics from \code{extractModelSummaries}, having structure as specified by that function}
#'   \item{parameters}{Model parameters from \code{extractModelParameters}, having structure as specified by that function}
#'   \item{class_counts}{Latent class counts and proportions for models that include a categorical latent variable}
#'   \item{mod_indices}{Model modification indices from \code{extractModIndices}, having structure as specified by that function}
#'   \item{savedata_info}{File information about SAVEDATA files related to this output}
#'   \item{savedata}{SAVEDATA file as an R \code{data.frame}, as described in \code{getSavedata_Data}}
#'   \item{bparameters}{an \code{mcmc.list} object containing the draws from the MCMC chains for a Bayesian model that uses the
#'   	SAVEDATA: BPARAMETERS command}
#'   \item{residuals}{a list containing relevant information from OUTPUT: RESIDUALS}
#'   \item{tech1}{a list containing parameter specification and starting values from OUTPUT: TECH1}
#'   \item{tech3}{a list containing parameter covariance and correlation matrices from OUTPUT: TECH3}
#'   \item{tech4}{a list containing means, covariances, and correlations for latent variables from OUTPUT: TECH4}
#'   \item{tech7}{a list containing sample statistics for each latent class from OUTPUT: TECH7}
#'   \item{tech9}{a list containing warnings/errors from replication runs for MONTECARLO analyses from OUTPUT: TECH9}
#'   \item{tech12}{a list containing observed versus estimated sample statistics for TYPE=MIXTURE analyses from OUTPUT: TECH12}
#'   \item{lcCondMeans}{conditional latent class means and pairwise comparisons, obtained using auxiliary(e) syntax in latent class models}
#'   \item{gh5}{a list containing data from the gh5 (graphics) file corresponding to this output. (Requires rhdf5 package)}
#' @author Michael Hallquist
#' @seealso \code{\link{extractModelSummaries}},
#' \code{\link{extractModelParameters}}, \code{\link{extractModIndices}},
#' \code{\link{getSavedata_Fileinfo}}, \code{\link{getSavedata_Data}},
#' \code{\link{getSavedata_Bparams}}
#' @keywords interface
#' @export
#' @examples
#' \dontrun{
#'   allOutput <- readModels(
#'     "C:/Program Files/Mplus/Mplus Examples/User's Guide Examples", recursive=TRUE)
#' }
readModels <- function(target=getwd(), recursive=FALSE, filefilter) {
  #large wrapper function to read summaries, parameters, and savedata from one or more output files.

  outfiles <- getOutFileList(target, recursive, filefilter)

  allFiles <- list()
  for (curfile in outfiles) {
    cat("Reading model: ", curfile, "\n")
    #if not recursive, then each element is uniquely identified (we hope!) by filename alone
    if (recursive==FALSE)	listID <- make.names(splitFilePath(curfile)$filename) #each list element is named by the respective file
    else listID <- make.names(curfile) #each list element is named by the respective file

    outfiletext <- scan(curfile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE, quiet=TRUE)

    allFiles[[listID]]$input <- inp <- extractInput_1file(outfiletext, curfile)
    warn_err <- extractWarningsErrors_1file(outfiletext, curfile, input=inp)
    allFiles[[listID]]$warnings <- warn_err$warnings
    allFiles[[listID]]$errors <- warn_err$errors
    allFiles[[listID]]$summaries <- extractSummaries_1file(outfiletext, curfile, input=inp)
    allFiles[[listID]]$parameters <- extractParameters_1file(outfiletext, curfile)
    allFiles[[listID]]$class_counts <- extractClassCounts(outfiletext, curfile) #latent class counts
    allFiles[[listID]]$mod_indices <- extractModIndices_1file(outfiletext, curfile)
    allFiles[[listID]]$savedata_info <- fileInfo <- l_getSavedata_Fileinfo(curfile, outfiletext)

    #missing widths indicative of MI/MC run
    if (!is.null(fileInfo) && is.na(fileInfo[["fileVarWidths"]])) {
      allFiles[[listID]]$savedata <- l_getSavedata_readRawFile(curfile, outfiletext, format="free", fileName=fileInfo[["fileName"]], varNames=fileInfo[["fileVarNames"]], input=inp)
    } else {
      allFiles[[listID]]$savedata <- l_getSavedata_readRawFile(curfile, outfiletext, format="fixed", fileName=fileInfo[["fileName"]], varNames=fileInfo[["fileVarNames"]], varWidths=fileInfo[["fileVarWidths"]], input=inp)
    }

    allFiles[[listID]]$bparameters <- l_getSavedata_Bparams(curfile, outfiletext, fileInfo, discardBurnin=FALSE)
    allFiles[[listID]]$residuals <- extractResiduals(outfiletext, curfile)
    allFiles[[listID]]$tech1 <- extractTech1(outfiletext, curfile) #parameter specification
    allFiles[[listID]]$tech3 <- extractTech3(outfiletext, fileInfo, curfile) #covariance/correlation matrix of parameter estimates
    allFiles[[listID]]$tech4 <- extractTech4(outfiletext, curfile) #latent means
    allFiles[[listID]]$tech7 <- extractTech7(outfiletext, curfile) #sample stats for each class
    allFiles[[listID]]$tech9 <- extractTech9(outfiletext, curfile) #tech 9 output (errors and warnings for Monte Carlo output)
    allFiles[[listID]]$tech12 <- extractTech12(outfiletext, curfile) #observed versus estimated sample stats for TYPE=MIXTURE
    allFiles[[listID]]$fac_score_stats <- extractFacScoreStats(outfiletext, curfile) #factor scores mean, cov, corr assoc with PLOT3

    #aux(e) means and pairwise comparisons
    allFiles[[listID]]$lcCondMeans <- extractAux(outfiletext, curfile)

    #add class tag for use with compareModels
    class(allFiles[[listID]]) <- c("mplus.model", "list")
    attr(allFiles[[listID]], "filename") <- curfile

    #cleanup summary columns containing only NAs
    for (col in names(allFiles[[listID]]$summaries)) {
      if (all(is.na(allFiles[[listID]]$summaries[[col]]))) allFiles[[listID]]$summaries[[col]] <- NULL
    }

    #check for gh5 file, and load if possible
    gh5 <- list()
    gh5fname <- sub("^(.*)\\.out$", "\\1.gh5", curfile, ignore.case=TRUE, perl=TRUE)
    if (file.exists(gh5fname)) {
      if (suppressWarnings(require(rhdf5))) {
        gh5 <- h5dump(file=gh5fname, recursive=TRUE, load=TRUE)
      } else { warning(paste(c("Unable to read gh5 file because rhdf5 package not installed.\n",
                    "To install, in an R session, type:\n",
                    "  source(\"http://bioconductor.org/biocLite.R\")\n",
                    "  biocLite(\"rhdf5\")\n")))
      }
    }
    allFiles[[listID]]$gh5 <- gh5
  }

  if (length(outfiles)==1) {
    allFiles <- allFiles[[1]] #no need for single top-level element when there is only one file
  } else {
    class(allFiles) <- c("mplus.model.list", "list")
  }

  return(allFiles)
}

#' Extract values from Mplus output
#' An internal function used by extractSummaries_1file to extract
#' parameters from the output file using regular expressions.
#'
#' @param pattern the exact text to be matched in the outfile that identifies the parameter of interest
#' @param textToScan the chunk of Mplus output to be parsed, passed as a vector of character strings (from the scan command).
#' @param filename the name of the file containing textToScan. Used to make more intelligible warning messages.
#' @param type the data type of the parameter, which determines the regexp used. Currently can be \dQuote{int}, \dQuote{dec}, \dQuote{str}, or \dQuote{calc}. Defaults to \dQuote{int}.
#' @return A string or numeric vector
#' @keywords internal
#' @examples
#' #make me!!!
extractValue <- function(pattern, textToScan, filename, type="int") {
  #regex pattern now allows for specification to search for value on some line before or after match
  #example: +2:the Observed and the Replicated Chi-Square Values

  offset <- 0
  if (grepl("^[+-]+\\d+:.*$", pattern, perl=TRUE)) {
    offset <- as.numeric(sub("^([+-]+\\d+):.*$", "\\1", pattern, perl=TRUE))
    pattern <- sub("^[+-]+\\d+:(.*)$", "\\1", pattern, perl=TRUE) #chop offset
  }

  #locate the matching line in the output file
  matchpos <- grep(pattern, textToScan, ignore.case=TRUE)
  matchlines <- textToScan[(matchpos+offset)]

  if (length(matchlines) > 1) {
    stop("More than one match found for parameter: ", pattern, "\n  ", filename)
    #return(matchlines) #not sure what I was thinking here... seems better to stop than warn and return lines
  }
  else if (length(matchlines) == 0) {
    #if the parameter of interest not found in this file, then return NA
    #warning(paste("Parameter not found: ", pattern, "\n  ", filename, sep=""))
    if (type == "int") return(NA_integer_)
    else if (type == "dec") return(NA_real_)
    else if (type == "str") return(NA_character_)
  }

  #different idea: concatenate pattern with var type and match on that
  #then sub just the pattern part from the larger line

  typePrefix <- substr(type, 1, 3)

  if (typePrefix == "int") {
    regexp <- "-*\\d+" #optional negative sign in front
  }
  else if (typePrefix == "dec") {
    #regexpr: -*\\d+\\.\\d+ : -* optional negative sign, \\d+ match at least one digit \\. match decimal sign \\d+ match decimal digits
    regexp <- "-*\\d+\\.\\d+"
  }
  else if (typePrefix == "str") {
    regexp <- paste(pattern, ".*", sep="")
  }

  #locate the match
  valueMatches <- gregexpr(regexp, matchlines[1], perl=TRUE)[[1]]

  if (type == "str") {
    #remove the tag portion of the string (e.g., "title:"), retaining rest of line
    returnVal <- as.character(sub(pattern, "", matchlines[1], ignore.case=TRUE))
  }
  else {
    #excessively tight syntax: replace dec[15] with 15, if number at end of type. Otherwise return just "dec".
    #then grep result for only numeric characters (\\d+). If grep is false (i.e., no numerals in substitution,
    #then no index was specified in type, so type must be simply "dec", "int", or "str" (as opposed to "int[15]"), so set as 1
    if (!grepl("^\\d+$", whichMatch <- sub("^.*\\[(\\d+)\\]$", "\\1", type, perl=TRUE), perl=TRUE)) whichMatch <- 1
    else whichMatch <- as.numeric(whichMatch)

    #pull from the start of the match through match.length, which is the length of characters that matched
    #need to subtract one from the start + length offset to grab the correct number of characters
    #(e.g., if the match runs from 40-44, the start will be 40, with length 5, but 40 + 5 would be 6 characters, hence -1
    returnVal <- as.numeric(substr(matchlines[1], valueMatches[whichMatch], valueMatches[whichMatch] + attr(valueMatches, "match.length")[whichMatch] - 1))

  }

  return(returnVal)
}

#' Extract a multiline section from Mplus output
#'
#' New approach to multiline section: retain spaces and look for next line that has identical indentation.
#'
#' @param header Header section
#' @param outfiletext Output file text
#' @param filename The name of the file
#' @param Logical indicating whether to allow multiple sections. Defaults to \code{FALSE}
#' @param allowSpace Logical indicating whether to allow spaces. Defaults to \code{TRUE}.
#' @return A list of sections
#' @keywords internal
#' @examples
#' # make me!!!
getMultilineSection <- function(header, outfiletext, filename, allowMultiple=FALSE, allowSpace=TRUE) {

  #allow for multiple depths (subsections) separated by ::
  #will just extract from deepest depth
  header <- strsplit(header, "::", fixed=TRUE)[[1]]

  sectionList <- list()
  targetText <- outfiletext
  for (level in 1:length(header)) {
    if (allowSpace==TRUE) headerRow <- grep(paste("^\\s*", header[level], "\\s*$", sep=""), targetText, perl=TRUE)
    else headerRow <- grep(paste("^", header[level], "$", sep=""), targetText, perl=TRUE) #useful for equality of means where we just want anything with 0 spaces

    if (length(headerRow) == 1L || (length(headerRow) > 0L && allowMultiple==TRUE)) {
      for (r in 1:length(headerRow)) {
        #locate the position of the first non-space character
        numSpacesHeader <- regexpr("\\S+.*$", targetText[headerRow[r]], perl=TRUE) - 1

        sectionStart <- headerRow[r] + 1 #skip header row itself
        #if (outfiletext[sectionStart] == "") sectionStart <- sectionStart + 1 #As far as I know, there is always a blank line after the header, so skip past it

        sameLevelMatch <- FALSE
        readStart <- sectionStart #counter variable to chunk through output
        while(sameLevelMatch == FALSE) {
          #read 20-line chunks of text to find next line with identical identation
          #more efficient than running gregexpr on whole output
          #match position of first non-space character, subtract 1 to get num spaces.
          #blank lines will generate a value of -2, so shouldn't throw off top-level match
          firstNonspaceCharacter <- lapply(gregexpr("\\S+.*$", targetText[readStart:(readStart+19)], perl=TRUE), FUN=function(x) x - 1)
          samelevelMatches <- which(firstNonspaceCharacter == numSpacesHeader)
          if (length(samelevelMatches) > 0) {
            sameLevelMatch <- TRUE
            sectionEnd <- readStart+samelevelMatches[1] - 2 #-1 for going to line before next header, another -1 for readStart
          }
          else if (readStart+19 >= length(targetText)) {
            sameLevelMatch <- TRUE
            sectionEnd <- length(targetText)
          }
          else readStart <- readStart + 20 #process next batch

          #if (readStart > 100000) browser()#stop ("readStart exceeded 100000. Must be formatting problem.")
        }

        #there will probably be collisions between use of nested headers :: and use of allowMultiple
        #I haven't attempted to get both to work together as they're currently used for different purposes
        if (isTRUE(allowMultiple))
          sectionList[[r]] <- targetText[sectionStart:sectionEnd]
        else
          #set targetText as chunk from start to end. If there are multiple subsections, then the
          #next iteration of the for loop will process within the subsetted targetText.
          targetText <- targetText[sectionStart:sectionEnd]

      }

    }
    else {
      targetText <- NA_character_
      if (length(headerRow) > 1L) warning(paste("Multiple matches for header: ", header, "\n  ", filename, sep=""))
      break
      #else if (length(headerRow) < 1) warning(paste("Could not locate section based on header: ", header, "\n  ", filename, sep=""))
    }

  }

  if (length(sectionList) > 0L && allowMultiple) {
    attr(sectionList, "matchlines") <- headerRow
    return(sectionList)
  }
  else return(targetText)
}

#' Worker function used in extractSummaries_1section
#'
#' @param arglist The argument list
#' @param sectionHeaders A character vector with headers for each section of interest
#' @param sectionFields is a list of data.frames where each data.frame specifies the fields to be extracted for that section
#' @param textToParse The text to parse
#' @param filename The filename
#' @return A list
#' @keywords internal
#' @examples
#' # make me!!!
extractSummaries_1plan <- function(arglist, sectionHeaders, sectionFields, textToParse, filename) {
  #make this a more generic function that accepts headers and fields in case it is useful outside the MODEL FIT section
  if (length(sectionHeaders) < 1) stop("No section headers provided.")
  if (length(sectionHeaders) != length(sectionFields)) stop("Section headers and section fields have different lengths.")

  #multiple sections
  for (header in 1:length(sectionHeaders)) {
    #a blank section header indicates to match anywhere in the textToParse
    if (sectionHeaders[header] == "") {
      sectionText <- textToParse
    } else {
      #could be pretty inefficient if the same section header is repeated several times.
      #could build a list with divided output and check whether a section is present in the list before extracting
      sectionText <- getMultilineSection(sectionHeaders[header], textToParse, filename)
    }

    #process all fields for this section
    sectionFieldDF <- sectionFields[[header]]

    for (i in 1:nrow(sectionFieldDF)) {
      thisField <- sectionFieldDF[i,]

      #Check whether this field already exists and is not missing. If so, skip the extraction.
      #This was initially setup because of Tech 14 section changes where the number of final stage optimizations is different from v6 to v7.
      if (!thisField$varName %in% names(arglist) || is.na(arglist[[ thisField$varName ]])) {
        arglist[[ thisField$varName ]] <- extractValue(pattern=thisField$regexPattern, sectionText, filename, type=thisField$varType)
      }
    }
  }

  return(arglist)

}

#' Extract summary information for one section from Mplus output
#'
#' Function to extract model fit statistics from a section, wrapped to allow for multiple fit sections, as in EFA files.
#' Calls \code{extractSummaries_1plan}
#'
#' @param modelFitSection The fit information section
#' @param arglist The argument list
#' @param filename The file name
#' @return The argument list
#' @keywords internal
#' @examples
#' # make me!!!
extractSummaries_1section <- function(modelFitSection, arglist, filename) {

  #MI and Montecarlo data types have fundamentally different output (means and sds per fit stat)
  if (grepl("imputation", arglist$DataType, ignore.case=TRUE) || grepl("montecarlo", arglist$DataType, ignore.case=TRUE)) {
    modelFitSectionHeaders <- c(
        "", #section-inspecific parameters
        "Chi-Square Test of Model Fit",
#        "Chi-Square Test of Model Fit for the Baseline Model",
        "Loglikelihood::H0 Value",
        "Loglikelihood::H1 Value",
        "CFI/TLI::CFI",
        "CFI/TLI::TLI",
        "Information Criteria::Akaike \\(AIC\\)",
        "Information Criteria::Bayesian \\(BIC\\)",
        "Information Criteria::Sample-Size Adjusted BIC \\(n\\* = \\(n \\+ 2\\) / 24\\)",
        "RMSEA \\(Root Mean Square Error Of Approximation\\)",
        "WRMR \\(Weighted Root Mean Square Residual\\)",
        "Information Criterion::Deviance \\(DIC\\)",
        "Information Criterion::Estimated Number of Parameters \\(pD\\)",
        "Information Criterion::Bayesian \\(BIC\\)"
    )
    modelFitSectionFields <- list(
        data.frame(
            varName=c("Parameters"), #defined outside of information criteria section for non-ML estimators
            regexPattern=c("Number of Free Parameters"),
            varType=c("int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("ChiSqM_DF", "ChiSqM_Mean", "ChiSqM_SD", "ChiSqM_NumComputations"),
            regexPattern=c("Degrees of Freedom", "Mean", "Std Dev", "Number of successful computations"),
            varType=c("int", "dec", "dec", "int"), stringsAsFactors=FALSE
        ),
#        data.frame(
#            varName=c("ChiSqBaseline_Value", "ChiSqBaseline_DF", "ChiSqBaseline_PValue"),
#            regexPattern=c("Value", "Degrees of Freedom", "^P-Value"),
#            varType=c("dec", "int", "dec"), stringsAsFactors=FALSE
#        ),
        data.frame(
            varName=c("LL_Mean", "LL_SD", "LL_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("UnrestrictedLL_Mean", "UnrestrictedLL_SD", "UnrestrictedLL_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("CFI_Mean", "CFI_SD", "CFI_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("TLI_Mean", "TLI_SD", "TLI_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("AIC_Mean", "AIC_SD", "AIC_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("BIC_Mean", "BIC_SD", "BIC_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("aBIC_Mean", "aBIC_SD", "aBIC_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("RMSEA_Mean", "RMSEA_SD", "RMSEA_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("WRMR_Mean", "WRMR_SD", "WRMR_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame( #Information Criterion:: DIC
            varName=c("DIC_Mean", "DIC_SD", "DIC_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame( #Information Criterion:: Estimated number of parameters (pD)
            varName=c("pD_Mean", "pD_SD", "pD_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        ),
        data.frame( #Information Criterion:: Bayesian (BIC) -- sometimes within Information Criterion, sometimes Information Criteria (above)...
            varName=c("BIC_Mean", "BIC_SD", "BIC_NumComputations"),
            regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
            varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
        )
    )

    #handle two-level models, which return separate srmr for between vs. within
    if (grepl("twolevel", arglist$AnalysisType, ignore.case=TRUE)) {
      modelFitSectionHeaders <- append(modelFitSectionHeaders, c(
              "SRMR \\(Standardized Root Mean Square Residual\\) for the WITHIN level",
              "SRMR \\(Standardized Root Mean Square Residual\\) for the BETWEEN level"))

      modelFitSectionFields <- c(modelFitSectionFields,
          list(data.frame(
                  varName=c("SRMR.Within_Mean", "SRMR.Within_SD", "SRMR.Within_NumComputations"),
                  regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
                  varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
              ),
              data.frame(
                  varName=c("SRMR.Between_Mean", "SRMR.Between_SD", "SRMR.Between_NumComputations"),
                  regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
                  varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
              ))
      )
    }
    else {
      modelFitSectionHeaders <- append(modelFitSectionHeaders, "SRMR \\(Standardized Root Mean Square Residual\\)")
      modelFitSectionFields <- c(modelFitSectionFields,
          list(data.frame(
                  varName=c("SRMR_Mean", "SRMR_SD", "SRMR_NumComputations"),
                  regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
                  varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
              ))
      )

    }

  }
  else { #not imputation or monte carlo output
    modelFitSectionHeaders <- c(
        "", #section-inspecific parameters
        "Chi-Square Test of Model Fit",
        "Chi-Square Test of Model Fit for the Baseline Model",
        "Chi-Square Test for Difference Testing",
        "Loglikelihood",
        "CFI/TLI",
        "Information Criteria",
        "RMSEA \\(Root Mean Square Error Of Approximation\\)",
        "WRMR \\(Weighted Root Mean Square Residual\\)",
        "Bayesian Posterior Predictive Checking using Chi-Square",
        "Information Criterion", #somehow singular for bayes output?
        "Wald Test of Parameter Constraints"
    )
    modelFitSectionFields <- list(
        data.frame(
            varName=c("Parameters"), #defined outside of information criteria section for non-ML estimators
            regexPattern=c("Number of Free Parameters"),
            varType=c("int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("ChiSqM_Value", "ChiSqM_DF", "ChiSqM_PValue", "ChiSqM_ScalingCorrection"),
            regexPattern=c("^\\s*Value", "Degrees of Freedom", "^\\s*P-Value", "Scaling Correction Factor"),
            varType=c("dec", "int", "dec", "dec"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("ChiSqBaseline_Value", "ChiSqBaseline_DF", "ChiSqBaseline_PValue"),
            regexPattern=c("^\\s*Value", "Degrees of Freedom", "^\\s*P-Value"),
            varType=c("dec", "int", "dec"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("ChiSqDiffTest_Value", "ChiSqDiffTest_DF", "ChiSqDiffTest_PValue"),
            regexPattern=c("^\\s*Value", "Degrees of Freedom", "^\\s*P-Value"),
            varType=c("dec", "int", "dec"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("LL", "UnrestrictedLL", "LLCorrectionFactor", "UnrestrictedLLCorrectionFactor"),
            regexPattern=c("H0 Value", "H1 Value", "H0 Scaling Correction Factor", "H1 Scaling Correction Factor"),
            varType=c("dec", "dec", "dec", "dec"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("CFI", "TLI"),
            regexPattern=c("CFI", "TLI"),
            varType=c("dec", "dec"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("AIC", "BIC", "aBIC"),
            regexPattern=c("Akaike \\(AIC\\)", "Bayesian \\(BIC\\)", "Sample-Size Adjusted BIC"),
            varType=c("dec", "dec", "dec"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("RMSEA_Estimate", "RMSEA_90CI_LB", "RMSEA_90CI_UB", "RMSEA_pLT05"),
            regexPattern=c("Estimate", "90 Percent C.I.", "90 Percent C.I.", "Probability RMSEA <= .05"),
            varType=c("dec", "dec[1]", "dec[2]", "dec"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("WRMR"),
            regexPattern=c("Value"),
            varType=c("dec"), stringsAsFactors=FALSE
        ),
        data.frame( #Bayesian Posterior Predictive Checking using Chi-Square
            varName=c("ObsRepChiSqDiff_95CI_LB", "ObsRepChiSqDiff_95CI_UB", "PostPred_PValue"),
            regexPattern=c("+2:the Observed and the Replicated Chi-Square Values", "+2:the Observed and the Replicated Chi-Square Values", "Posterior Predictive P-Value"),
            varType=c("dec[1]", "dec[2]", "dec"), stringsAsFactors=FALSE
        ),
        data.frame( #Information Criterion
            varName=c("DIC", "pD", "BIC"),
            regexPattern=c("Deviance \\(DIC\\)", "Estimated Number of Parameters \\(pD\\)", "Bayesian \\(BIC\\)"), #sometimes BIC is listed here (e.g., MI Bayes output)
            varType=c("dec", "dec", "dec"), stringsAsFactors=FALSE
        ),
        data.frame( #Wald Test of Parameter Constraints
            varName=c("WaldChiSq_Value", "WaldChiSq_DF", "WaldChiSq_PValue"),
            regexPattern=c("^\\s*Value", "Degrees of Freedom", "^\\s*P-Value"),
            varType=c("dec", "int", "dec"), stringsAsFactors=FALSE
        )
    )

    if (grepl("twolevel", arglist$AnalysisType, ignore.case=TRUE)) {

      modelFitSectionHeaders <- append(modelFitSectionHeaders, "SRMR \\(Standardized Root Mean Square Residual\\)")

      modelFitSectionFields <- c(modelFitSectionFields,
          list(data.frame(
                  varName=c("SRMR.Within", "SRMR.Between"),
                  regexPattern=c("Value for Within", "Value for Between"),
                  varType=c("dec", "dec"), stringsAsFactors=FALSE
              ))
      )

    }
    else {

      modelFitSectionHeaders <- append(modelFitSectionHeaders, "SRMR \\(Standardized Root Mean Square Residual\\)")

      #append two lists together
      modelFitSectionFields <- c(modelFitSectionFields,
          list(data.frame(
                  varName=c("SRMR"),
                  regexPattern=c("Value"),
                  varType=c("dec"), stringsAsFactors=FALSE
              )
          ))
    }
  }

  arglist <- extractSummaries_1plan(arglist, modelFitSectionHeaders, modelFitSectionFields, modelFitSection, filename)
  return(arglist)
}


#' Divide text into fields
#'
#' Helper function to divide an input section into key-value pair list taken from mplus2lavaan
#'
#' @param section.text The section text
#' @param required Required sections
#' @return Divided sections
#' @keywords internal
#' @examples
#' # make me!!!
divideIntoFields <- function(section.text, required) {

  if (is.null(section.text)) { return(NULL) }
  section.split <- strsplit(paste(section.text, collapse=" "), ";", fixed=TRUE)[[1]]

  section.divide <- list()

  for (cmd in section.split) {
    if (grepl("^\\s*!.*", cmd, perl=TRUE)) next #skip comment lines
    if (grepl("^\\s+$", cmd, perl=TRUE)) next #skip blank lines

    #mplus is apparently tolerant of specifications that don't include IS/ARE/=
    #example: usevariables x1-x10;
    #thus, split on spaces and assume that first element is lhs, drop second element if IS/ARE/=, and assume remainder is rhs

    #but if user uses equals sign, then spaces will not always be present (e.g., usevariables=x1-x10)
    if ( (leadingEquals <- regexpr("^\\s*[A-Za-z]+[A-Za-z_-]*\\s*(=)", cmd[1L], perl=TRUE))[1L] > 0) {
      cmdName <- trimSpace(substr(cmd[1L], 1, attr(leadingEquals, "capture.start") - 1))
      cmdArgs <- trimSpace(substr(cmd[1L], attr(leadingEquals, "capture.start") + 1, nchar(cmd[1L])))
    } else {
      cmd.spacesplit <- strsplit(trimSpace(cmd[1L]), "\\s+", perl=TRUE)[[1L]]

      if (length(cmd.spacesplit) < 2L) {
        #for future: make room for this function to prase things like just TECH13 (no rhs)
      } else {
        cmdName <- trimSpace(cmd.spacesplit[1L])
        if (length(cmd.spacesplit) > 2L && tolower(cmd.spacesplit[2L]) %in% c("is", "are")) {
          cmdArgs <- paste(cmd.spacesplit[3L:length(cmd.spacesplit)], collapse=" ") #remainder, removing is/are
        } else {
          cmdArgs <- paste(cmd.spacesplit[2L:length(cmd.spacesplit)], collapse=" ") #is/are not used, so just join rhs
        }
      }

    }

    section.divide[[make.names(tolower(cmdName))]] <- cmdArgs

  }

  if (!missing(required)) { stopifnot(all(required %in% names(section.divide))) }
  return(section.divide)
}

#' Extract warnings and errors from 1 mplus file
#'
#' Helper function
#'
#' @param outfiletext The text of the output file
#' @param filename The filename
#' @param input The input
#' @return A list with two elements
#'   \item{errors}{Mplus Errors}
#'   \item{warnings}{Mplus Warnings}
#' @keywords internal
#' @examples
#' # make me!!!
extractWarningsErrors_1file <- function(outfiletext, filename, input) {

  warnerr <- list(warnings = list(), errors = list())
  class(warnerr$errors) <- c("list", "mplus.errors")
  class(warnerr$warnings) <- c("list", "mplus.warnings")
  if (!inherits(input, "mplus.inp")) {
    warning("Could not identify warnings and errors; input is not of class mplus.inp")
    return(warnerr)
  }
  if (is.null(attr(input, "start.line")) || is.null(attr(input, "end.line")) ||
      attr(input, "start.line") < 0L || attr(input, "end.line") < 0L) {
    warning("Could not identify bounds of input section: ", filename)
    return(warnerr)
  }

  #handle input warnings and errors first
  startInputWarnErr <- attr(input, "end.line") + 1L #first eligible line is after input section
  endInputWarnErr <- grep("^\\s*(INPUT READING TERMINATED NORMALLY|\\*\\*\\* WARNING.*|\\d+ (?:ERROR|WARNING)\\(S\\) FOUND IN THE INPUT INSTRUCTIONS|\\*\\*\\* ERROR.*)\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)

  w <- 1 #counters for warnings and errors lists
  e <- 1

  #only process section if end was identified properly
  if (length(endInputWarnErr) > 0L) {
    #The above will match all of the possible relevant lines.
    #To identify input warnings/errors section, need to go to first blank line after the final warning or error. (look in next 100 lines)
    lastWarn <- endInputWarnErr[length(endInputWarnErr)]
    blank <- which(outfiletext[lastWarn:(lastWarn + 100 )] == "")[1L] + lastWarn - 1

    warnerrtext <- outfiletext[startInputWarnErr[1L]:(blank-1)]

    lines <- friendlyGregexpr("^\\s*(\\*\\*\\* WARNING|\\*\\*\\* ERROR).*\\s*$", warnerrtext, perl=TRUE)

    if (!is.null(lines)) {
      for (l in 1:nrow(lines)) {
        if (l < nrow(lines)) {
          warn.err.body <- trimSpace(warnerrtext[(lines[l,"element"] + 1):(lines[l+1,"element"] - 1)])
        } else {
          warn.err.body <- trimSpace(warnerrtext[(lines[l,"element"] + 1):length(warnerrtext)])
        }

        if (substr(lines[l,"tag"], 1, 11) == "*** WARNING") {
          warnerr$warnings[[w]] <- warn.err.body
          w <- w + 1
        } else if (substr(lines[l,"tag"], 1, 9) == "*** ERROR") {
          warnerr$errors[[e]] <- warn.err.body
          splittag <- strsplit(lines[l,"tag"], "\\s+", perl=TRUE)[[1L]]
          if (length(splittag) > 3L && splittag[3L] == "in") {
            attr(warnerr$errors[[e]], "section") <- tolower(paste(splittag[4L:(which(splittag == "command") - 1L)], collapse="."))
          }
          e <- e + 1
        } else { stop ("Cannot discern warning/error type: ", lines[l, "tag"]) }
      }
    }
  }

  #now handle estimation errors and warnings
  #these fall above either
  #  1) MODEL FIT INFORMATION: model converged with warnings
  #  2) MODEL RESULTS: model did not converge, so no fit statistics produced
  #  3) FINAL CLASS COUNTS (occurs for some mixture models, which report class counts before model results)
  #  4) TESTS OF MODEL FIT (older versions of Mplus)
  #
  # It's harder to determine where the section begins, however, because there is no clear boundary
  # with the preceding section, which is heterogeneous (e.g., sample stats).
  #
  # In the case of warnings only, the estimation warnings section is demarcated by
  # THE MODEL ESTIMATION TERMINATED NORMALLY above and MODEL FIT INFORMATION below.
  #
  # In other cases (maybe dependent on Mplus version), warnings are printed above THE MODEL ESTIMATION TERMINATED NORMALLY.
  # Allow for the possibility that the estimation warnings/errors section begins with WARNING:
  #
  # For failed models, the section likely begins with one of three possibilities:
  #  1) THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY
  #  2) THE LOGLIKELIHOOD DECREASED
  #  3) NO CONVERGENCE
  #
  # Warnings that can potentially be ignored are prefixed by "WARNING: "
  # whereas more serious estimation problems (errors) typically have no prefix.
  #
  # Blank lines indicate a boundary in each message.

  #the end sections are more well behaved (esp. if there is Tech 9 output). Identify end first, then constrain start to precede end
  endEstWarnErr <- grep("^\\s*(MODEL FIT INFORMATION|FINAL CLASS COUNTS|MODEL RESULTS|TESTS OF MODEL FIT)\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)

  if (length(endEstWarnErr) == 0L) { return(warnerr) } #unable to find section properly
  startEstWarnErr <- grep("^\\s*(WARNING:.*|THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY.*|THE LOGLIKELIHOOD DECREASED.*|THE MODEL ESTIMATION TERMINATED NORMALLY|NO CONVERGENCE\\.\\s+NUMBER OF ITERATIONS EXCEEDED\\..*)\\s*$",
      outfiletext[1:endEstWarnErr[1L]], ignore.case=TRUE, perl=TRUE)

  if (length(startEstWarnErr) > 0L && length(endEstWarnErr) > 0L) {
    warnerrtext <- outfiletext[startEstWarnErr[1L]:(endEstWarnErr[1L] - 1)]

    #if the model estimation terminated normally, delete this line from the text to parse (whereas the other start flags indicate a meaningful message)
    if (length(normexit <- grep("^\\s*THE MODEL ESTIMATION TERMINATED NORMALLY\\s*$", warnerrtext, perl=TRUE, ignore.case=TRUE)) > 0L) {
      warnerrtext <- warnerrtext[-normexit]
    }

    if (!any(warnerrtext != "")) {
      return(warnerr) #no non-blank lines -- just exit function as is
    }

    #trim blank lines from beginning and end of section
    warnerrtext <- warnerrtext[min(which(warnerrtext != "")):max(which(warnerrtext != ""))]

    #estimation warnings and errors are separated by blank lines.
    blanks <- which(warnerrtext == "")

    #trim consecutive blank lines (throw off blanks-based parsing below)
    consec <- which(diff(blanks) == 1)
    if (length(consec) > 0L) {
      warnerrtext <- warnerrtext[-1*blanks[consec]]
      blanks <- which(warnerrtext == "") #clunky
    }

    #for loop is probably clunky here, but works for now
    startMsg <- 1 #first line of a message
    for (line in 1:length(warnerrtext)) {
      if ((line %in% blanks && ! (line-1) %in% blanks) || line == length(warnerrtext)) {
        msg <- trimSpace(warnerrtext[startMsg:ifelse(line %in% blanks, line - 1, line)])

        if (grepl("^\\s*WARNING:", msg[1L], ignore.case=TRUE, perl=TRUE)) {
          warnerr$warnings[[w]] <- msg
          w <- w+1
        } else {
          warnerr$errors[[e]] <- msg #if not prefixed by WARNING:, treat as error
          e <- e + 1
        }

        startMsg <- line + 1
      }
    }

  } else { } #warning("Unable to identify estimation warnings and errors section.") }

  return(warnerr)
}



#' Extract and parse Mplus input file
#'
#' Function to extract and parse mplus input syntax from the output file
#'
#' @param outfiletext The text of the output file
#' @param filename The filename
#' @return The parsed input file
#' @keywords internal
#' @examples
#' # make me!!!
extractInput_1file <- function(outfiletext, filename) {
  input <- list()
  class(input) <- c("list", "mplus.inp")

  startInput <- grep("^\\s*INPUT INSTRUCTIONS\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)
  if (length(startInput) == 0L) {
    warning("Could not find beginning of input for: ", filename)
    attr(input, "start.line") <- attr(input, "end.line") <- -1L
    return(input)
  } else { startInput <- startInput[1L] + 1L } #skip input instructions line itself

  endInput <- grep("^\\s*(INPUT READING TERMINATED NORMALLY|\\*\\*\\* WARNING.*|\\d+ (?:ERROR|WARNING)\\(S\\) FOUND IN THE INPUT INSTRUCTIONS|\\*\\*\\* ERROR.*)\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)
  if (length(endInput) == 0L) {
    warning("Could not find end of input for: ", filename)
    attr(input, "start.line") <- attr(input, "end.line") <- -1
    return(input)
  } else { endInput <- endInput[1L] - 1L } #one line before first warning or end of instructions

  input.text <- outfiletext[startInput[1L]:endInput[1L]] #explicit first element because there could be both warnings and errors.

  #some code adapted from mplus2lavaan prototype
  inputHeaders <- grep("^\\s*(title:|data.*:|variable:|define:|analysis:|model.*:|output:|savedata:|plot:|montecarlo:)", input.text, ignore.case=TRUE, perl=TRUE)

  stopifnot(length(inputHeaders) > 0L)

  for (h in 1:length(inputHeaders)) {
    sectionEnd <- ifelse(h < length(inputHeaders), inputHeaders[h+1] - 1, length(input.text))
    section <- input.text[inputHeaders[h]:sectionEnd]
    sectionName <- trimSpace(sub("^([^:]+):.*$", "\\1", section[1L], perl=TRUE)) #obtain text before the colon

    #dump section name from input syntax
    section[1L] <- sub("^[^:]+:(.*)$", "\\1", section[1L], perl=TRUE)

    input[[make.names(tolower(sectionName))]] <- section
  }

  #divide some input sections into fields
  #need to do a better job here of handling blank lines and such
  input$title <- paste(trimSpace(input$title), collapse=" ")
  input$data <- divideIntoFields(input$data)
  input$data.imputation <- divideIntoFields(input$data.imputation)
  input$variable <- divideIntoFields(input$variable)
  input$analysis <- divideIntoFields(input$analysis)
  input$montecarlo <- divideIntoFields(input$montecarlo)


  attr(input, "start.line") <- startInput
  attr(input, "end.line") <- endInput

  return(input)
}

#' Extract the summaries from one file
#'
#' Description: This function parses an output file for specific model details. It returns a list of model details for a single output file.
#'
#' @param outfiletext This is the output file in string form to be parsed. Passed in from extractModelSummaries.
#' @param filename Name of the file being parsed. Used in case of bad model, prints a warning.
#' @return A list of the summaries
#' @keywords internal
#' @examples
#' # make me!!!
extractSummaries_1file <- function(outfiletext, filename, input)
{
  #preallocates list
  arglist <- list()

  #obtain mplus software version
  if ((mplus.version <- regexpr("\\s*Mplus VERSION ([\\d\\.]+)\\s*", outfiletext[1L], perl=TRUE)) > 0L) {
    arglist$Mplus.version <- substr(outfiletext[1L], attr(mplus.version, "capture.start")[1L], attr(mplus.version, "capture.start")[1L] + attr(mplus.version, "capture.length")[1L] - 1)
  }

  ###Copy some elements of the input instructions into the summaries

  #copy title into arglist
  if (!is.null(input$title)) {
    arglist$Title <- input$title
  } else {
    #warning("Unable to locate title field. Returning missing") #Warning doesn't seem very useful
    arglist$Title <- NA_character_
  }

  #extract the analysis type, which is important for setting other parameters.
  if (!is.null(input$analysis$type)) {
    arglist$AnalysisType <- input$analysis$type
  } else {
    arglist$AnalysisType <- "GENERAL" #Analysis type not specified, default to general
  }

  #extract the data type (important for detecting imputation datasets)
  if (!is.null(input$data$type)) {
    arglist$DataType <- input$data$type
  }  else if (any(c("montecarlo", "model.population") %in% names(input))) {
    arglist$DataType <- "MONTECARLO"
  } else {
    arglist$DataType <- "INDIVIDUAL" #Data type not specified, default to individual
  }

  #End input instructions processing


  #BEGIN ANALYSIS SUMMARY PROCESSING
  analysisSummarySection <- getSection("^\\s*SUMMARY OF ANALYSIS\\s*$", outfiletext)

  arglist$Estimator <- extractValue(pattern="^\\s*Estimator\\s*", analysisSummarySection, filename, type="str")
  arglist$Observations <- extractValue(pattern="^\\s*Number of observations\\s*", analysisSummarySection, filename, type="int")

  #END ANALYSIS SUMMARY PROCESSING

  #BEGIN MODEL FIT STATISTICS PROCESSING
  #handle EFA output, which has separate model fit sections within each file
  #do this by extracting model fit sections for each and using an rbind call

  if (grepl("(?!MIXTURE|TWOLEVEL)\\s*EFA\\s+", arglist$AnalysisType, ignore.case=TRUE, perl=TRUE)) {

    factorLB <- as.numeric(sub(".*EFA\\s+(\\d+).*", "\\1", arglist$AnalysisType, perl=TRUE))
    factorUB <- as.numeric(sub(".*EFA\\s+\\d+\\s+(\\d+).*", "\\1", arglist$AnalysisType, perl=TRUE))
    factorSeq <- seq(factorLB, factorUB)
    EFASections <- grep(paste("^\\s*EXPLORATORY FACTOR ANALYSIS WITH (",
            paste(factorSeq, collapse="|"), ") FACTOR\\(S\\):\\s*$", sep=""), outfiletext, perl=TRUE)

    if (!length(EFASections) > 0) stop("Unable to locate section headers for EFA model fit statistics")

    #need to convert from list to data.frame format to allow for proper handling of rbind below
    arglistBase <- as.data.frame(arglist, stringsAsFactors=FALSE)

    efaList <- list()
    for (thisFactor in 1:length(factorSeq)) {

      #subset output by starting text to be searched at the point where factor output begins
      modelFitSection <- getSection_Blanklines("^(TESTS OF MODEL FIT|MODEL FIT INFORMATION)$", outfiletext[EFASections[thisFactor]:length(outfiletext)])

      efaList[[thisFactor]] <- extractSummaries_1section(modelFitSection, arglistBase, filename)
      efaList[[thisFactor]]$NumFactors <- factorSeq[thisFactor]
    }

    arglist <- do.call(rbind, efaList)
  }
  else {

    modelFitSection <- getSection("^(TESTS OF MODEL FIT|MODEL FIT INFORMATION)$", outfiletext)
    arglist <- extractSummaries_1section(modelFitSection, arglist, filename)
  }

  #CLASSIFICATION QUALITY
  classificationQuality <- getSection("^CLASSIFICATION QUALITY$", outfiletext)

  if (!is.null(classificationQuality))
    arglist$Entropy <- extractValue(pattern="^\\s*Entropy\\s*", classificationQuality, filename, type="dec")
  #overkill
  #arglist <- extractSummaries_1plan(arglist, "", list(data.frame(varName="Entropy", regexPattern="Entropy", varType=c("dec"), stringsAsFactors=FALSE)), classificationQuality, filename)
  else
    arglist$Entropy <- NA_real_ #maybe try to avoid the is null logic and just have extractModelSummary correctly handle null sections

  #TECH11 OUTPUT: LMR LRT
  tech11Output <- getSection("^\\s*TECHNICAL 11 OUTPUT\\s*$", outfiletext)

  if (!is.null(tech11Output)) {
    tech11headers <- c(
        "Random Starts Specifications for the k-1 Class Analysis Model",
        "VUONG-LO-MENDELL-RUBIN LIKELIHOOD RATIO TEST FOR \\d+ \\(H0\\) VERSUS \\d+ CLASSES",
        "LO-MENDELL-RUBIN ADJUSTED LRT TEST"
    )
    tech11fields <- list(
        data.frame(
            varName=c("T11_KM1Starts", "T11_KM1Final"),
            regexPattern=c("Number of initial stage random starts", "Number of final stage optimizations"),
            varType=c("int", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("T11_KM1LL", "T11_VLMR_2xLLDiff", "T11_VLMR_ParamDiff", "T11_VLMR_Mean", "T11_VLMR_SD", "T11_VLMR_PValue"),
            regexPattern=c("H0 Loglikelihood Value", "2 Times the Loglikelihood Difference", "Difference in the Number of Parameters", "Mean", "Standard Deviation", "P-Value"),
            varType=c("dec", "dec", "int", "dec", "dec", "dec"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("T11_LMR_Value", "T11_LMR_PValue"),
            regexPattern=c("^\\s*Value", "^\\s*P-Value"),
            varType=c("dec", "dec"), stringsAsFactors=FALSE
        )
    )

    arglist <- extractSummaries_1plan(arglist, tech11headers, tech11fields, tech11Output, filename)
  }


  tech14Output <- getSection("^\\s*TECHNICAL 14 OUTPUT\\s*$", outfiletext)

  if (!is.null(tech14Output)) {
    tech14headers <- c(
        "", #section-inspecific parameters
        "Random Starts Specifications for the k-1 Class Analysis Model",
        "Random Starts Specification for the k-1 Class Model for Generated Data",
        "Random Starts Specification for the k Class Model for Generated Data",
        "PARAMETRIC BOOTSTRAPPED LIKELIHOOD RATIO TEST FOR \\d+ \\(H0\\) VERSUS \\d+ CLASSES"
    )
    tech14fields <- list(
        #top-level (no section)
        data.frame(
            varName=c("BLRT_RequestedDraws"),
            regexPattern=c("Number of bootstrap draws requested"),
            varType=c("str"), stringsAsFactors=FALSE
        ),
        #Random Starts Specifications for the k-1 Class Analysis Model
        data.frame(
            varName=c("BLRT_KM1AnalysisStarts", "BLRT_KM1AnalysisFinal"),
            regexPattern=c("Number of initial stage random starts", "Number of final stage optimizations"),
            varType=c("int", "int"), stringsAsFactors=FALSE
        ),
        #Random Starts Specification for the k-1 Class Model for Generated Data
        #v7 format: Number of final stage optimizations for the\n initial stage random starts  <N>
        #v6 format: Number of final stage optimizations <N>
        #Thus, include the genfinal twice here to catch both circumstances
        data.frame(
            varName=c("BLRT_KM1GenStarts", "BLRT_KM1GenFinal", "BLRT_KM1GenFinal"),
            regexPattern=c("Number of initial stage random starts", "+1:Number of final stage optimizations for the", "Number of final stage optimizations"),
            varType=c("int", "int", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("BLRT_KGenStarts", "BLRT_KGenFinal"),
            regexPattern=c("Number of initial stage random starts", "Number of final stage optimizations"),
            varType=c("int", "int"), stringsAsFactors=FALSE
        ),
        data.frame(
            varName=c("BLRT_KM1LL", "BLRT_2xLLDiff", "BLRT_ParamDiff", "BLRT_PValue", "BLRT_SuccessfulDraws"),
            regexPattern=c("H0 Loglikelihood Value", "2 Times the Loglikelihood Difference", "Difference in the Number of Parameters", "Approximate P-Value", "Successful Bootstrap Draws"),
            varType=c("dec", "dec", "int", "dec", "int"), stringsAsFactors=FALSE
        )
    )

    arglist <- extractSummaries_1plan(arglist, tech14headers, tech14fields, tech14Output, filename)

  }

  #calculate adjusted AIC per Burnham & Anderson(2004), which is better than AIC for non-nested model selection
  #handle AICC calculation, requires AIC, Parameters, and observations
  if (!is.null(arglist$Parameters) && !is.na(arglist$Parameters) &&
      !is.null(arglist$AIC) && !is.na(arglist$AIC) &&
      !is.null(arglist$Observations) && !is.na(arglist$Observations)) {
    arglist$AICC <- arglist$AIC + (2*arglist$Parameters*(arglist$Parameters+1))/(arglist$Observations-arglist$Parameters-1)
  } else {
    arglist$AICC <- NA_real_
  }

  #Only warn about missing LL for ML-based estimators
#too convoluted to maintain (and not so useful), generating errors I don't want to debug
#  if ("Estimator" %in% extract && "LL" %in% extract
#			&& !is.na(arglist$Estimator) && arglist$Estimator %in% c("ML", "MLR", "MLM", "MLMV", "MLF")
#			&& ((grepl("imputation", arglist$DataType, ignore.case=TRUE) && is.na(arglist$LL_Mean))
#			|| (!grepl("imputation", arglist$DataType, ignore.case=TRUE) && is.na(arglist$LL))))
#    warning("Model missing LL value, despite use of ML-based estimator. Likely a failed run.\n  ", filename)
#

  #for now, skip including input instructions in the returned data.frame. Makes the output too cluttered.
  #arglist$InputInstructions <- paste((outfiletext[(startInput+1):(endInput-1)]), collapse="\n")
  arglist$Filename <- splitFilePath(filename)$filename #only retain filename, not path

  arglist <- as.data.frame(arglist, stringsAsFactors=FALSE)
  class(arglist) <- c("data.frame", "mplus.summaries")
  attr(arglist, "filename") <- arglist$Filename
  return(arglist)
}

#' Extract summary statistics from a single output file or from a group of Mplus models within a directory
#'
#' Parses a group of Mplus model output files (.out extension) for model fit statistics.
#' At this time, the details extracted are fixed and include: \code{Filename, InputInstructions, Title, Estimator,
#' LL, BIC, aBIC, AIC, AICC, Parameters, Observations, CFI, TLI, RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB,
#' RMSEA_pLT05, ChiSqM_Value, ChiSqM_DF, ChiSq_PValue, BLRT_KM1LL, BLRT_PValue, BLRT_Numdraws)}. The
#' infrastructure is in place to allow for user-specified selection of summary statistics in future versions.
#'
#' @param target the directory containing Mplus output files (.out) to parse OR the
#'   single output file to be parsed. Defaults to the current working directory.
#'   Example: "C:/Users/Michael/Mplus Runs"
#' @param recursive optional. If \code{TRUE}, parse all models nested in
#'   subdirectories within \code{directory}. Defaults to \code{FALSE}.
#' @param filefilter a Perl regular expression (PCRE-compatible) specifying particular
#'   output files to be parsed within \code{directory}. See \code{regex} or
#'   \url{http://www.pcre.org/pcre.txt} for details about regular expression syntax.
#'
#' @return Returns a \code{data.frame} containing model fit statistics for all output files within \code{directory}.
#' The \code{data.frame} contains some of the following variables (depends on model type):
#' \item{Title}{Title for the model, specified by the TITLE: command}
#' \item{Filename}{Filename of the output file}
#' \item{Estimator}{Estimator used for the model (e.g., ML, MLR, WLSMV, etc.)}
#' \item{LL}{Log-likelihood of the model}
#' \item{BIC}{Bayesian Information Criterion}
#' \item{aBIC}{Sample-Size-Adjusted BIC (Sclove, 1987)}
#' \item{AIC}{Akaike's Information Criterion}
#' \item{AICC}{Corrected AIC, based on Sugiura (1978) and recommended by Burnham & Anderson (2002)}
#' \item{DIC}{Deviance Information Criterion. Available in ESTIMATOR=BAYES output.}
#' \item{Parameters}{Number of parameters estimated by the model}
#' \item{pD}{Estimated number of parameters in Bayesian output}
#' \item{Observations}{The number of observations for the model (does not suppport multiple-groups analysis at this time)}
#' \item{CFI}{Confirmatory Fit Index}
#' \item{TLI}{Tucker-Lewis Index}
#' \item{RMSEA_Estimate}{Point estimate of root mean squared error of approximation}
#' \item{RMSEA_90CI_LB}{Lower bound of the 90\% Confidence Interval around the RMSEA estimate.}
#' \item{RMSEA_90CI_UB}{Upper bound of the 90\% Confidence Interval around the RMSEA estimate.}
#' \item{RMSEA_pLT05}{Probability that the RMSEA estimate falls below .05, indicating good fit.}
#' \item{ChiSqM_Value}{Model chi-squared value}
#' \item{ChiSqM_DF}{Model chi-squared degrees of freedom}
#' \item{ChiSqM_PValue}{Model chi-squared p value}
#' \item{ObsRepChiSqDiff_95CI_LB}{Lower bound of 95\% confidence interval for the difference between observed and replicated chi-square values}
#' \item{ObsRepChiSqDiff_95CI_UB}{Upper bound of 95\% confidence interval for the difference between observed and replicated chi-square values}
#' \item{PostPred_PValue}{Posterior predictive p-value}
#' \item{BLRT_RequestedDraws}{Number of requested bootstrap draws for TECH14.}
#' \item{BLRT_KM1LL}{Log-likelihood of the K-1 model (one less class) for the Bootstrapped Likelihood Ratio Test (TECH14).}
#' \item{BLRT_2xLLDiff}{Two times the log-likelihood difference of the models with K and K-1 classes (TECH14).}
#' \item{BLRT_ParamDiff}{Difference in the number of parameters for models with K and K-1 classes (TECH14).}
#' \item{BLRT_PValue}{P-value of the Bootstrapped Likelihood Ratio Test (TECH14) testing whether the K class model is significantly better than K-1}
#' \item{BLRT_SuccessfulDraws}{The number of successful bootstrapped samples used in the Bootstrapped Likelihood Ratio Test}
#' \item{SRMR}{Standardized root mean square residual}
#' \item{SRMR.Between}{For TYPE=TWOLEVEL output, standardized root mean square residual for between level}
#' \item{SRMR.Within}{For TYPE=TWOLEVEL output, standardized root mean square residual for within level}
#' \item{WRMR}{Weighted root mean square residual}
#' \item{ChiSqBaseline_Value}{Baseline (unstructured) chi-squared value}
#' \item{ChiSqBaseline_DF}{Baseline (unstructured) chi-squared degrees of freedom}
#' \item{ChiSqBaseline_PValue}{Baseline (unstructured) chi-squared p value}
#' \item{NumFactors}{For TYPE=EFA output, the number of factors}
#' \item{T11_KM1Starts}{TECH11: Number of initial stage random starts for k-1 model}
#' \item{T11_KM1Final}{TECH11: Number of final stage optimizations for k-1 model}
#' \item{T11_KM1LL}{TECH11: Log-likelihood of the K-1 model used for the Vuong-Lo-Mendell-Rubin LRT}
#' \item{T11_VLMR_2xLLDiff}{TECH11: 2 * Log-likelihood Difference of K-class vs. K-1-class model for the Vuong-Lo-Mendell-Rubin LRT}
#' \item{T11_VLMR_ParamDiff}{TECH11: Difference in number of parameters between K-class and K-1-class model for the Vuong-Lo-Mendell-Rubin LRT}
#' \item{T11_VLMR_Mean}{TECH11: Vuong-Lo-Mendell-Rubin LRT mean}
#' \item{T11_VLMR_SD}{TECH11: Vuong-Lo-Mendell-Rubin LRT standard deviation}
#' \item{T11_VLMR_PValue}{TECH11: Vuong-Lo-Mendell-Rubin LRT p-value}
#' \item{T11_LMR_Value}{TECH11: Lo-Mendell-Rubin Adjusted LRT value}
#' \item{T11_LMR_PValue}{TECH11: Lo-Mendell-Rubin Adjusted LRT p-value}
#'
#' @author Michael Hallquist
#' @seealso \code{\link{regex}}, \code{\link{runModels}}, \code{\link{readModels}}
#' @keywords interface
#' @export
#' @examples
#' \dontrun{
#'   allExamples <- extractModelSummaries(
#'     "C:/Program Files/Mplus/Mplus Examples/User's Guide Examples")
#' }
extractModelSummaries <- function(target=getwd(), recursive=FALSE, filefilter) {
  #retain working directory and reset at end of run
  curdir <- getwd()

  outfiles <- getOutFileList(target, recursive, filefilter)

  details <- list()

  #for each output file, use the extractSummaries_1file function to extract relevant data
  #note that extractSummaries_1file returns data as a list
  #rbind creates an array of lists by appending each extractSummaries_1file return value
  for (i in 1:length(outfiles)) {
    #read the file
    readfile <- scan(outfiles[i], what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE, quiet=TRUE)

    #bomb out for EFA files
    if (length(grep("TYPE\\s+(IS|=|ARE)\\s+((MIXTURE|TWOLEVEL)\\s+)+EFA\\s+\\d+", readfile, ignore.case=TRUE, perl=TRUE)) > 0) {
      warning(paste0("EFA, MIXTURE EFA, and TWOLEVEL EFA files are not currently supported by extractModelSummaries.\n  Skipping outfile: ", outfiles[i]))
      next #skip file
    }

    #append params for this file to the details array
    #note that this is a memory-inefficient solution because of repeated copying. Better to pre-allocate.

    inp <- extractInput_1file(readfile, outfiles[i])
    details[[i]] <- extractSummaries_1file(readfile, outfiles[i], inp)
  }

  #if there are several output files, then use rbind.fill to align fields
  if (length(details) > 1L) details <- do.call(rbind.fill, details)
  else details <- details[[1L]]

  #reset working directory
  setwd(curdir)

  #cleanup columns containing only NAs
  for (col in names(details)) {
    if (all(is.na(details[[col]]))) details[[col]] <- NULL
  }

  return(details)
}


#' Add header to saved data
#'
#' Description
#'
#' @param outfile The output file
#' @param director The current working directory by default
#' @return NULL
#' @keywords internal
#' @examples
#' # make me!!!
addHeaderToSavedata <- function(outfile, directory=getwd()) {

}


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
subsetModelList <- function(modelList, keepCols, dropCols, sortBy) {

  #if passed an mplus.model.list from readModels, then just extract summaries for disply
  if (inherits(modelList, "mplus.model.list")) {
    modelList <- do.call("rbind.fill", sapply(modelList, "[", "summaries"))
  } else if (inherits(modelList, "mplus.model")) { #single model (e.g., EFA output with many factor solutions)
    modelList <- modelList$summaries
  }

  #only allow keep OR drop.
  if(!missing(keepCols) && !missing(dropCols)) stop("keepCols and dropCols passed to subsetModelList. You must choose one or the other, but not both.")

  #if did not pass either drop or keep, setup useful defaults
  if (missing(keepCols) && missing(dropCols)) keepCols <- c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")

  #keep only columns specified by keepCols
  if (!missing(keepCols) && length(keepCols) > 0) {
    #check to make sure each column exists if keepCols used
    summaryNames <- names(modelList)
    for (colName in keepCols) {
      if (!colName %in% summaryNames) keepCols <- keepCols[-which(keepCols==colName)]
    }

    if (length(keepCols) == 0) stop("All fields passed as keepCols are missing from data.frame\n  Fields in data.frame are:\n  ", paste(strwrap(paste(summaryNames, collapse=" "), width=80, exdent=4), collapse="\n"))
    MplusData <- modelList[, keepCols, drop=FALSE]
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

  if (!sortBy %in% notMissing) stop("sortBy field: ", sortBy, " is not present in the summary data.frame.\n  Check your keepCols and dropCols arguments and the summary data.frame")

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


#' Extract residual matrices
#'
#' Function that extracts the residual matrices including standardized ones
#'
#' @param outfiletext the text of the output file
#' @param filename The name of the file
#' @return A list of the residual matrices
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractResiduals <- function(outfiletext, filename) {
  residSection <- getSection("^RESIDUAL OUTPUT$", outfiletext)
  if (is.null(residSection)) return(list()) #no residuals output

  #allow for multiple groups
  residSubsections <- getMultilineSection("ESTIMATED MODEL AND RESIDUALS \\(OBSERVED - ESTIMATED\\)( FOR [\\w\\d\\s\\.,]+)*",
      residSection, filename, allowMultiple=TRUE)

  matchlines <- attr(residSubsections, "matchlines")

  if (length(residSubsections) == 0) {
    warning("No sections found within residuals output.")
    return(list())
  }
  else if (length(residSubsections) > 1)
    groupNames <- make.names(gsub("^\\s*ESTIMATED MODEL AND RESIDUALS \\(OBSERVED - ESTIMATED\\)( FOR ([\\w\\d\\s\\.,]+))*\\s*$", "\\2", residSection[matchlines], perl=TRUE))

  residList <- list()
  #multiple groups possible
  for (g in 1:length(residSubsections)) {
    targetList <- list()

    targetList[["meanEst"]] <- matrixExtract(residSubsections[[g]], "Model Estimated Means(/Intercepts/Thresholds)*", filename)
    targetList[["meanResid"]] <- matrixExtract(residSubsections[[g]], "Residuals for Means(/Intercepts/Thresholds)*", filename)
    targetList[["meanResid.std"]] <- matrixExtract(residSubsections[[g]], "Standardized Residuals \\(z-scores\\) for Means(/Intercepts/Thresholds)*", filename)
    targetList[["meanResid.norm"]] <- matrixExtract(residSubsections[[g]], "Normalized Residuals for Means(/Intercepts/Thresholds)*", filename)
    targetList[["covarianceEst"]] <- matrixExtract(residSubsections[[g]], "Model Estimated Covariances(/Correlations/Residual Correlations)*", filename)
    targetList[["covarianceResid"]] <- matrixExtract(residSubsections[[g]], "Residuals for Covariances(/Correlations/Residual Correlations)*", filename)
    targetList[["covarianceResid.std"]] <- matrixExtract(residSubsections[[g]], "Standardized Residuals \\(z-scores\\) for Covariances(/Correlations/Residual Corr)*", filename)
    targetList[["covarianceResid.norm"]] <- matrixExtract(residSubsections[[g]], "Normalized Residuals for Covariances(/Correlations/Residual Correlations)*", filename)
    targetList[["slopeEst"]] <- matrixExtract(residSubsections[[g]], "Model Estimated Slopes", filename)
    targetList[["slopeResid"]] <- matrixExtract(residSubsections[[g]], "Residuals for Slopes", filename)

    if (length(residSubsections) > 1) {
      class(targetList) <- c("list", "mplus.residuals")
      residList[[groupNames[g]]] <- targetList
    }
    else
      residList <- targetList
  }

  class(residList) <- c("list", "mplus.residuals")
  if (length(residSubsections) > 1) attr(residList, "group.names") <- groupNames

  return(residList)
}

#' Extract Technical 1 matrix from Mplus
#'
#' Function that extracts the Tech1 matrix
#'
#' @param outfiletext the text of the output file
#' @param filename The name of the file
#' @return A list of class \dQuote{mplus.tech1}
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractTech1 <- function(outfiletext, filename) {
  tech1Section <- getSection("^TECHNICAL 1 OUTPUT$", outfiletext)
  if (is.null(tech1Section)) return(list()) #no tech1 output

  tech1List <- list()

  paramSpecSubsections <- getMultilineSection("PARAMETER SPECIFICATION( FOR [\\w\\d\\s\\.,]+)*",
      tech1Section, filename, allowMultiple=TRUE)

  matchlines <- attr(paramSpecSubsections, "matchlines")

  paramSpecList <- list()
  if (length(paramSpecSubsections) == 0)
    warning ("No parameter specfication sections found within TECH1 output.")
  else if (length(paramSpecSubsections) > 1)
    groupNames <- make.names(gsub("^\\s*PARAMETER SPECIFICATION( FOR ([\\w\\d\\s\\.,]+))*\\s*$", "\\2", tech1Section[matchlines], perl=TRUE))
  else #just one section, no groups
    groupNames <- ""

  for (g in 1:length(paramSpecSubsections)) {
    targetList <- list()

    targetList[["tau"]] <- matrixExtract(paramSpecSubsections[[g]], "TAU", filename)
    targetList[["nu"]] <- matrixExtract(paramSpecSubsections[[g]], "NU", filename)
    targetList[["lambda"]] <- matrixExtract(paramSpecSubsections[[g]], "LAMBDA", filename)
    targetList[["theta"]] <- matrixExtract(paramSpecSubsections[[g]], "THETA", filename)
    targetList[["alpha"]] <- matrixExtract(paramSpecSubsections[[g]], "ALPHA", filename)
    targetList[["beta"]] <- matrixExtract(paramSpecSubsections[[g]], "BETA", filename)
    targetList[["gamma"]] <- matrixExtract(paramSpecSubsections[[g]], "GAMMA", filename)
    targetList[["psi"]] <- matrixExtract(paramSpecSubsections[[g]], "PSI", filename)
    targetList[["gamma.c"]] <- matrixExtract(paramSpecSubsections[[g]], "GAMMA\\(C\\)", filename)
    targetList[["alpha.c"]] <- matrixExtract(paramSpecSubsections[[g]], "ALPHA\\(C\\)", filename)

    #latent class indicator part includes subsections for each latent class, such as class-varying thresholds
    if (groupNames[g] == "LATENT.CLASS.INDICATOR.MODEL.PART") {
      tauLines <- grep("TAU\\(U\\) FOR LATENT CLASS \\d+", paramSpecSubsections[[g]], perl=TRUE, value=TRUE)
      uniqueLC <- unique(gsub("^\\s*TAU\\(U\\) FOR LATENT CLASS (\\d+)\\s*$", "\\1", tauLines, perl=TRUE))
      for (lc in uniqueLC) {
        targetList[[paste0("tau.u.lc", lc)]] <- matrixExtract(paramSpecSubsections[[g]], paste0("TAU\\(U\\) FOR LATENT CLASS ", lc), filename)
      }
    }

    if (length(paramSpecSubsections) > 1) {
      class(targetList) <- c("list", "mplus.parameterSpecification")
      paramSpecList[[groupNames[g]]] <- targetList
    }
    else
      paramSpecList <- targetList
  }

  class(paramSpecList) <- c("list", "mplus.parameterSpecification")
  if (length(paramSpecSubsections) > 1) attr(paramSpecList, "group.names") <- groupNames

  startValSubsections <- getMultilineSection("STARTING VALUES( FOR [\\w\\d\\s\\.,]+)*",
      tech1Section, filename, allowMultiple=TRUE)

  matchlines <- attr(startValSubsections, "matchlines")

  startValList <- list()
  if (length(startValSubsections) == 0)
    warning ("No starting value sections found within TECH1 output.")
  else if (length(startValSubsections) > 1)
    groupNames <- make.names(gsub("^\\s*STARTING VALUES( FOR ([\\w\\d\\s\\.,]+))*\\s*$", "\\2", tech1Section[matchlines], perl=TRUE))
  else
    groupNames <- ""

  for (g in 1:length(startValSubsections)) {
    targetList <- list()

    targetList[["tau"]] <- matrixExtract(startValSubsections[[g]], "TAU", filename)
    targetList[["nu"]] <- matrixExtract(startValSubsections[[g]], "NU", filename)
    targetList[["lambda"]] <- matrixExtract(startValSubsections[[g]], "LAMBDA", filename)
    targetList[["theta"]] <- matrixExtract(startValSubsections[[g]], "THETA", filename)
    targetList[["alpha"]] <- matrixExtract(startValSubsections[[g]], "ALPHA", filename)
    targetList[["beta"]] <- matrixExtract(startValSubsections[[g]], "BETA", filename)
    targetList[["gamma"]] <- matrixExtract(startValSubsections[[g]], "GAMMA", filename)
    targetList[["psi"]] <- matrixExtract(startValSubsections[[g]], "PSI", filename)
    targetList[["gamma.c"]] <- matrixExtract(startValSubsections[[g]], "GAMMA\\(C\\)", filename)
    targetList[["alpha.c"]] <- matrixExtract(startValSubsections[[g]], "ALPHA\\(C\\)", filename)

    #latent class indicator part includes subsections for each latent class, such as class-varying thresholds
    if (groupNames[g] == "LATENT.CLASS.INDICATOR.MODEL.PART") {
      tauLines <- grep("TAU\\(U\\) FOR LATENT CLASS \\d+", startValSubsections[[g]], perl=TRUE, value=TRUE)
      uniqueLC <- unique(gsub("^\\s*TAU\\(U\\) FOR LATENT CLASS (\\d+)\\s*$", "\\1", tauLines, perl=TRUE))
      for (lc in uniqueLC) {
        targetList[[paste0("tau.u.lc", lc)]] <- matrixExtract(startValSubsections[[g]], paste0("TAU\\(U\\) FOR LATENT CLASS ", lc), filename)
      }
    }

    if (length(startValSubsections) > 1) {
      class(targetList) <- c("list", "mplus.startingValues")
      startValList[[groupNames[g]]] <- targetList
    }
    else
      startValList <- targetList
  }

  class(startValList) <- c("list", "mplus.startingValues")
  if (length(startValSubsections) > 1) attr(startValList, "group.names") <- groupNames

  tech1List <- list(parameterSpecification=paramSpecList, startingValues=startValList)
  class(tech1List) <- c("list", "mplus.tech1")

  return(tech1List)

}

#' Extract free file output
#'
#' Function for reading "free" output where a sequence of values populates a matrix
#'
#' @param filename The name of the output file
#' @param outfile The output file
#' @param make_symmetric A logical indicating whether or not to make the matrix symmetric, defaults to \code{TRUE}
#' @return a matrix
#' @keywords internal
#' @examples
#' # make me!!!
extractFreeFile <- function(filename, outfile, make_symmetric=TRUE) {
  #Adapted from code graciously provided by Joe Glass.

  if (is.null(filename) || is.na(filename)) return(NULL)

  #TODO: make this filename building into a function (duped from read raw)
  outfileDirectory <- splitFilePath(outfile)$directory
  savedataSplit <- splitFilePath(filename)

  #if outfile target directory is non-empty, but savedataFile is without directory, then append
  #outfile directory to savedataFile. This ensures that R need not be in the working directory
  #to read the savedataFile. But if savedataFile has an absolute directory, don't append

  #if savedata directory is present and absolute, or if no directory in outfile, just use filename as is
  if (!is.na(savedataSplit$directory) && savedataSplit$absolute)
    savedataFile <- filename #just use savedata filename if has absolute path
  else if (is.na(outfileDirectory))
    savedataFile <- filename #just use savedata filename if outfile is missing path (working dir)
  else
    savedataFile <- file.path(outfileDirectory, filename) #savedata path relative or absent and outfile dir is present

  if (!file.exists(savedataFile)) {
    warning("Cannot read file: ", filename)
    return(NULL)
  }

  values <- scan(savedataFile, what="character", strip.white=FALSE, blank.lines.skip=FALSE, quiet=TRUE)
  matrix.size <- function(x) {
    # per algebra of quadratic equations: p is the # of rows & columns in a symmetric
    # matrix given x unique covariance elements (the lower triangle plus diagonal).
    # This was constructed from the equation x = p(p+1)/2.
    p <- (-1/2) + sqrt(2*x + (1/4))
    # if p is not an integer, having x elements does not result in a symmetric matrix
    p.isinteger <- !length(grep("[^[:digit:]]", as.character(p)))
    if (p.isinteger) {
      return (p)
    } else {
      cat("The length of the supplied vector is not appropriate to generate the matrix. Please check the data file.")
      return(NULL)
    }
  }

  matSize <- matrix.size(length(values))
  mat <- matrix(NA_real_, nrow=matSize, ncol=matSize,
      dimnames=list(1:matSize, 1:matSize)) # create empty symmetric matrix
  mat[upper.tri(mat, diag=TRUE)] <- as.numeric(values) # import savedata information into the upper triangle (plus diagonal) of the matrix

  if (make_symmetric) {
    mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)] #populate lower triangle
  } else {
    mat <- t(mat) # transpose the matrix to create a lower triangular matrix (plus diagonal)
  }

  return(mat)
}

#' Extract Technical 3 matrix from Mplus
#'
#' Function that extracts the Tech3 matrix
#'
#' @param outfiletext the text of the output file
#' @param savedata_info Information on saved data
#' @param filename The name of the file
#' @return A list of class \dQuote{mplus.tech3}
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractTech3 <- function(outfiletext, savedata_info, filename) {
  tech3Section <- getSection("^TECHNICAL 3 OUTPUT$", outfiletext)
  if (is.null(tech3Section)) return(list()) #no tech3 output

  tech3List <- list()
  tech3List[["paramCov"]] <- matrixExtract(tech3Section, "ESTIMATED COVARIANCE MATRIX FOR PARAMETER ESTIMATES", filename)
  tech3List[["paramCor"]] <- matrixExtract(tech3Section, "ESTIMATED CORRELATION MATRIX FOR PARAMETER ESTIMATES", filename)

  if (!is.null(savedata_info) && !is.na(savedata_info$tech3File)) {
    tech3List[["paramCov.savedata"]] <- extractFreeFile(savedata_info$tech3File, filename, make_symmetric=TRUE)
  } else {
    tech3List[["paramCov.savedata"]] <- NULL
  }

  class(tech3List) <- c("list", "mplus.tech3")

  return(tech3List)
}

#' Extract Technical 4 matrix from Mplus
#'
#' Function that extracts the Tech4 matrix
#'
#' @param outfiletext the text of the output file
#' @param filename The name of the file
#' @return A list of class \dQuote{mplus.tech4}
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractTech4 <- function(outfiletext, filename) {
  #TODO: have empty list use mplus.tech4 class
  tech4Section <- getSection("^TECHNICAL 4 OUTPUT$", outfiletext)
  if (is.null(tech4Section)) return(list()) #no tech4 output

  tech4List <- list()

  tech4Subsections <- getMultilineSection("ESTIMATES DERIVED FROM THE MODEL( FOR [\\w\\d\\s\\.,]+)*",
      tech4Section, filename, allowMultiple=TRUE)

  matchlines <- attr(tech4Subsections, "matchlines")

  if (length(tech4Subsections) == 0) {
    warning("No sections found within TECH4 output.")
    return(list())
  }
  else if (length(tech4Subsections) > 1) {
    groupNames <- make.names(gsub("^\\s*ESTIMATES DERIVED FROM THE MODEL( FOR ([\\w\\d\\s\\.,]+))*\\s*$", "\\2", tech4Section[matchlines], perl=TRUE))
  }

  for (g in 1:length(tech4Subsections)) {
    targetList <- list()

    targetList[["latMeansEst"]] <- matrixExtract(tech4Subsections[[g]], "ESTIMATED MEANS FOR THE LATENT VARIABLES", filename)
    targetList[["latCovEst"]] <- matrixExtract(tech4Subsections[[g]], "ESTIMATED COVARIANCE MATRIX FOR THE LATENT VARIABLES", filename)
    targetList[["latCorEst"]] <- matrixExtract(tech4Subsections[[g]], "ESTIMATED CORRELATION MATRIX FOR THE LATENT VARIABLES", filename)

    if (length(tech4Subsections) > 1) {
      class(targetList) <- c("list", "mplus.tech4")
      tech4List[[groupNames[g]]] <- targetList
    }
    else
      tech4List <- targetList

  }

  class(tech4List) <- c("list", "mplus.tech4")

  return(tech4List)
}

#' Extract Technical 7 from Mplus
#'
#' The TECH7 option is used in conjunction with TYPE=MIXTURE to request sample statistics
#' for each class using raw data weighted by the estimated posterior probabilities for each class.
#'
#' @param outfiletext the text of the output file
#' @param filename The name of the file
#' @return A list of class \dQuote{mplus.tech7}
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractTech7 <- function(outfiletext, filename) {
  #TODO: have empty list use mplus.tech7 class
  #not sure whether there are sometimes multiple groups within this section.
  tech7Section <- getSection("^TECHNICAL 7 OUTPUT$", outfiletext)
  if (is.null(tech7Section)) return(list()) #no tech7 output

  tech7List <- list()

  tech7Subsections <- getMultilineSection("SAMPLE STATISTICS WEIGHTED BY ESTIMATED CLASS PROBABILITIES FOR CLASS \\d+",
      tech7Section, filename, allowMultiple=TRUE)

  matchlines <- attr(tech7Subsections, "matchlines")

  if (length(tech7Subsections) == 0) {
    warning("No sections found within tech7 output.")
    return(list())
  }
  else if (length(tech7Subsections) > 1) {
    groupNames <- make.names(gsub("^\\s*SAMPLE STATISTICS WEIGHTED BY ESTIMATED CLASS PROBABILITIES FOR (CLASS \\d+)\\s*$", "\\1", tech7Section[matchlines], perl=TRUE))
  }

  for (g in 1:length(tech7Subsections)) {
    targetList <- list()

    targetList[["classSampMeans"]] <- matrixExtract(tech7Subsections[[g]], "Means", filename)
    targetList[["classSampCovs"]] <- matrixExtract(tech7Subsections[[g]], "Covariances", filename)

    if (length(tech7Subsections) > 1) {
      class(targetList) <- c("list", "mplus.tech7")
      tech7List[[groupNames[g]]] <- targetList
    }
    else
      tech7List <- targetList
  }

  class(tech7List) <- c("list", "mplus.tech7")

  return(tech7List)
}


#' Extract Technical 9 matrix from Mplus
#'
#' Function that extracts the Tech9 matrix
#'
#' @param outfiletext the text of the output file
#' @param filename The name of the file
#' @return A list of class \dQuote{mplus.tech9}
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractTech9 <- function(outfiletext, filename) {
  tech9List <- list()
  class(tech9List) <- c("list", "mplus.tech9")

  tech9Section <- getSection("^TECHNICAL 9 OUTPUT$", outfiletext)
  if (is.null(tech9Section)) return(tech9List) #no tech9 output

  tech9Reps <- grep("^\\s*REPLICATION \\d+:\\s*$", tech9Section, perl=TRUE)

  repNums <- as.numeric(gsub("^\\s*REPLICATION (\\d+):\\s*$", "\\1", tech9Section[tech9Reps], perl=TRUE))

  if (length(tech9Reps) > 0L) {
    for (l in 1:length(tech9Reps)) {
      if (l < length(tech9Reps)) {
        msg <- paste(tech9Section[ (tech9Reps[l]+1):(tech9Reps[l+1]-1) ], collapse=" ")
      } else {
        msg <- paste(tech9Section[ (tech9Reps[l]+1):length(tech9Section) ], collapse=" ")
      }
      msg <- trimSpace(gsub("\\s+", " ", msg, perl=TRUE))
      tech9List[[ paste0("rep", repNums[l]) ]] <- list(rep=repNums[l], error=msg)
    }
  }

  return(tech9List)
}

#' Extract Technical 10 matrix from Mplus
#'
#' Function that extracts the Tech10 matrix
#'
#' @param outfiletext the text of the output file
#' @param filename The name of the file
#' @return An empty list
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractTech10 <- function(outfiletext, filename) {
  tech10Section <- getSection("^TECHNICAL 10 OUTPUT$", outfiletext)
  if (is.null(tech10Section)) return(list()) #no tech10 output

  tech10List <- list()

}

#' Extract Technical 12 from Mplus
#'
#' The TECH12 option is used in conjunction with TYPE=MIXTURE to request residuals for observed
#' versus model estimated means, variances, covariances, univariate skewness, and univariate
#' kurtosis. The observed values come from the total sample. The estimated values are computed as
#' a mixture across the latent classes.
#'
#' @param outfiletext the text of the output file
#' @param filename The name of the file
#' @return A list of class \dQuote{mplus.tech12}
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractTech12 <- function(outfiletext, filename) {
  #TODO: have empty list use mplus.tech12 class
  #not sure whether there are sometimes multiple groups within this section.
  tech12Section <- getSection("^TECHNICAL 12 OUTPUT$", outfiletext)
  if (is.null(tech12Section)) return(list()) #no tech12 output

  tech12List <- list()

  tech12Subsections <- getMultilineSection("ESTIMATED MIXED MODEL AND RESIDUALS \\(OBSERVED - EXPECTED\\)",
      tech12Section, filename, allowMultiple=TRUE)

  matchlines <- attr(tech12Subsections, "matchlines")

  if (length(tech12Subsections) == 0) {
    warning("No sections found within tech12 output.")
    return(list())
  }
  else if (length(tech12Subsections) > 1) {
    warning("extractTech12 does not yet know how to handle multiple sections (if such exist)")
  }

  for (g in 1:length(tech12Subsections)) {
    targetList <- list()

    targetList[["obsMeans"]] <- matrixExtract(tech12Subsections[[g]], "Observed Means", filename)
    targetList[["mixedMeans"]] <- matrixExtract(tech12Subsections[[g]], "Estimated Mixed Means", filename)
    targetList[["mixedMeansResid"]] <- matrixExtract(tech12Subsections[[g]], "Residuals for Mixed Means", filename)
    targetList[["obsCovs"]] <- matrixExtract(tech12Subsections[[g]], "Observed Covariances", filename)
    targetList[["mixedCovs"]] <- matrixExtract(tech12Subsections[[g]], "Estimated Mixed Covariances", filename)
    targetList[["mixedCovsResid"]] <- matrixExtract(tech12Subsections[[g]], "Residuals for Mixed Covariances", filename)
    targetList[["obsSkewness"]] <- matrixExtract(tech12Subsections[[g]], "Observed Skewness", filename)
    targetList[["mixedSkewness"]] <- matrixExtract(tech12Subsections[[g]], "Estimated Mixed Skewness", filename)
    targetList[["mixedSkewnessResid"]] <- matrixExtract(tech12Subsections[[g]], "Residuals for Mixed Skewness", filename)
    targetList[["obsKurtosis"]] <- matrixExtract(tech12Subsections[[g]], "Observed Kurtosis", filename)
    targetList[["mixedKurtosis"]] <- matrixExtract(tech12Subsections[[g]], "Estimated Mixed Kurtosis", filename)
    targetList[["mixedKurtosisResid"]] <- matrixExtract(tech12Subsections[[g]], "Residuals for Mixed Kurtosis", filename)

    if (length(tech12Subsections) > 1) {
      class(targetList) <- c("list", "mplus.tech12")
      tech12List[[g]] <- targetList #no known case where there are many output sections
    }
    else
      tech12List <- targetList
  }

  class(tech12List) <- c("list", "mplus.tech12")

  return(tech12List)
}

#' Extract Factor Score Statistics
#'
#' Function for extracting matrices for factor scores
#'
#' @param outfiletext The text of the output file
#' @param filename The name of the output file
#' @return A list
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractFacScoreStats <- function(outfiletext, filename) {
  #for now, skip getSection call and use nested header to getMultilineSection to avoid issue of SAMPLE STATISTICS appearing both
  #as top-level header and sub-header within factor scores

  fssSection <- getMultilineSection("SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES::SAMPLE STATISTICS",
      outfiletext, filename, allowMultiple=FALSE)
  fssList <- list()
  class(fssList) <- c("list", "mplus.facscorestats")

  if (is.na(fssSection[1L])) return(fssList) #no factor scores output

  fssList[["Means"]] <- matrixExtract(fssSection, "Means", filename)
  fssList[["Covariances"]] <- matrixExtract(fssSection, "Covariances", filename)
  fssList[["Correlations"]] <- matrixExtract(fssSection, "Correlations", filename)

  return(fssList)

}


#' Extract Latent Class Counts
#'
#' Function for extracting counts of latent classes
#'
#' @param outfiletext The text of the output file
#' @param filename The name of the output file
#' @return a list
#' @keywords internal
#' @examples
#' # make me!!!
extractClassCounts <- function(outfiletext, filename) {

  ####
  #TODO: Implement class count extraction for multiple categorical latent variable models.
  #Example: UG7.21
  #Output is quite different because of latent class patterns, transition probabilities, etc.

  #helper function for three-column class output
  getClassCols <- function(sectiontext) {
    #identify lines of the form class number, class count, class proportion: e.g., 1		136.38		.2728
    numberLines <- grep("^\\s*\\d+\\s+[0-9\\.-]+\\s+[0-9\\.-]+\\s*$", sectiontext, perl=TRUE)

    if (length(numberLines) > 0) {
      #row bind each line, convert to numeric, and store as data.frame
      counts <- data.frame(do.call(rbind, lapply(strsplit(trimSpace(sectiontext[numberLines]), "\\s+", perl=TRUE), as.numeric)))
      if (!ncol(counts) == 3) {
        warning("Number of columns for model class counts is not three.")
        return(NULL)
      }

      names(counts) <- c("class", "count", "proportion")

      #store counts as integer
      counts <- transform(counts, class=as.integer(class))
      return(counts)
    } else {
      return(NULL)
    }

  }

  countlist <- list()

  modelCounts <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES$", outfiletext)
  countlist[["modelEstimated"]] <- getClassCols(modelCounts)

  ppCounts <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS$", outfiletext)
  countlist[["posteriorProb"]] <- getClassCols(ppCounts)

  mostLikelyCounts <- getSection("^CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP$", outfiletext)
  countlist[["mostLikely"]] <- getClassCols(mostLikelyCounts)

  #most likely by posterior probability section
  mostLikelyProbs <- getSection("^Average Latent Class Probabilities for Most Likely Latent Class Membership \\(Row\\)$", outfiletext)
  if (length(mostLikelyProbs) > 1L) { mostLikelyProbs <- mostLikelyProbs[-1L] } #remove line 1: "by Latent Class (Column)"

  #Example:
  #Average Latent Class Probabilities for Most Likely Latent Class Membership (Row)
  #by Latent Class (Column)
  #
  #  			1        2
  #
  #  1   0.986    0.014
  #  2   0.030    0.970
  #
  #A bit of a wonky section. Some notes:
  # 1) Rows represent those hard classified into that class.
  # 2) Rows sum to 1.0 and represent the summed average posterior probabilities of all the class assignment possibilities.
  # 3) Columns represent average posterior probabilitity of being in class 1 for those hard classified as 1 or 2.
  # 4) High diagonal indicates that hard classification matches posterior probability patterns.

  countlist[["avgProbs.mostLikely"]] <- unlabeledMatrixExtract(mostLikelyProbs, filename)

  #same, but for classification probabilities
  classificationProbs <- getSection("^Classification Probabilities for the Most Likely Latent Class Membership \\(Row\\)$", outfiletext)
  if (length(classificationProbs) > 1L) { classificationProbs <- classificationProbs[-1L] } #remove line 1: "by Latent Class (Column)"

  countlist[["classificationProbs.mostLikely"]] <- unlabeledMatrixExtract(classificationProbs, filename)

  #same, but for classification probability logits
  classificationLogitProbs <- getSection("^Logits for the Classification Probabilities for the Most Likely Latent Class Membership \\(Row\\)$", outfiletext)
  if (length(classificationLogitProbs) > 1L) { classificationLogitProbs <- classificationLogitProbs[-1L] } #remove line 1: "by Latent Class (Column)"

  countlist[["logitProbs.mostLikely"]] <- unlabeledMatrixExtract(classificationLogitProbs, filename)


  return(countlist)
}

#' Reconstruct matrix from unlabeled multi-line text output
#'
#' worker function for extracting Mplus matrix output from an unlabeled section
#' where matrices are spread across blocks to keep within width constraints
#' example: class counts output from latent class models.
#'
#' @param outfiletext The text of the output file
#' @param filename The name of the output file
#' @return a matrix
#' @keywords internal
#' @examples
#' # make me!!!
unlabeledMatrixExtract <- function(outfiletext, filename) {
  #This function extends the matrixExtract function by allowing for the matrix to be recreated
  #to have no header labels and where section headers have a blank line on either side. Only example is in the class counts section, where when there
  #are many classes, the most likely x posterior probability matrix is too wide and is output like this:

  #       1        2        3        4        5        6        7        8        9
  #
  #1   0.885    0.000    0.000    0.017    0.024    0.000    0.000    0.019    0.055
  #2   0.000    0.775    0.006    0.000    0.000    0.064    0.097    0.013    0.000
  #3   0.000    0.004    0.826    0.035    0.000    0.082    0.000    0.000    0.052
  #4   0.014    0.002    0.070    0.804    0.018    0.035    0.000    0.008    0.046
  #5   0.042    0.000    0.001    0.076    0.842    0.000    0.000    0.001    0.038
  #6   0.000    0.096    0.063    0.014    0.001    0.732    0.021    0.026    0.008
  #7   0.002    0.091    0.010    0.005    0.001    0.034    0.808    0.005    0.005
  #8   0.118    0.014    0.006    0.004    0.000    0.030    0.015    0.514    0.139
  #9   0.030    0.001    0.056    0.059    0.014    0.024    0.000    0.109    0.691
  #10  0.030    0.062    0.007    0.007    0.002    0.052    0.130    0.108    0.063
  #
  #      10
  #
  #1   0.000
  #2   0.046
  #3   0.001
  #4   0.004
  #5   0.000
  #6   0.038
  #7   0.039
  #8   0.159
  #9   0.016
  #10  0.539

  #Only one matrix can be extracted from outfiletext since sections are unlabeled

  if (length(outfiletext) > 0L && length(outfiletext) > 1L) {

    #pattern match: 1) blank line; 2) integers line; 3) blank line
    #find these cases, then add "DUMMY" to each of the header blank lines

    blankLines <- which(outfiletext == "")

    if (length(blankLines) > 0L) {
      headerLines <- c()

      for (b in 1:length(blankLines)) {
        if (b < length(blankLines) && blankLines[b+1] == blankLines[b] + 2) {
          # a blank line followed by a non-blank line followed by a blank line...
          # check that it represents an integer sequence (this may need to be removed in more general cases)
          intLine <- strsplit(trimSpace(outfiletext[blankLines[b]+1]), "\\s+", perl=TRUE)[[1L]]
          firstCol <- as.numeric(intLine[1L]) #number of the class in the first column
          if (all(intLine == firstCol:(firstCol + length(intLine) - 1) )) {
            headerLines <- c(headerLines, blankLines[b])
          }
        }
      }

      #add the header to blank lines preceding class labels row
      outfiletext[headerLines] <- "DUMMY"

      #now use matrix extract to reconstruct matrix
      unlabeledMat <- matrixExtract(outfiletext, "DUMMY", filename)

      return(unlabeledMat)

    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}


#' Reconstruct matrix from multi-line text output
#'
#' main worker function for extracting Mplus matrix output
#' where matrices are spread across blocks to keep within width constraints
#' example: tech1 matrix output.
#'
#' @param outfiletext The text of the output file
#' @param headerLine The header line
#' @param filename The name of the output file
#' @return a matrix
#' @keywords internal
#' @examples
#' # make me!!!
matrixExtract <- function(outfiletext, headerLine, filename) {
  matLines <- getMultilineSection(headerLine, outfiletext, filename, allowMultiple=TRUE)

  if (!is.na(matLines[1])) {
    numBlocks <- length(matLines)
    blockList <- list()
    for (m in 1:numBlocks) {
      colHeaders <- strsplit(trimSpace(matLines[[m]][1]), "\\s+", perl=TRUE)[[1]]

      #m+3 because m+1 is col header, m+2 is line of underscores
      block <- matLines[[m]][c(-1,-2)]
      block <- block[block != ""] #drop blank lines

      #10Jul2012: Occasionally, Mplus includes a blank line block just for fun... like this:
      #Residuals for Covariances/Correlations/Residual Correlations
      #STRES4
      #________
      #in this case, skip the block
      if (length(block) == 0) next

      splitData <- strsplit(trimSpace(block), "\\s+", perl=TRUE)

      #alternative to remove blank lines after strsplit (above easier to read)
      #remove blank lines by comparing against character(0)
      #splitData2 <- splitData[sapply(splitData, function(x) !identical(x, character(0)))]

      rowHeaders <- sapply(splitData, "[", 1)

      mat <- matrix(NA_real_, nrow=length(rowHeaders), ncol=length(colHeaders),
          dimnames=list(rowHeaders, colHeaders))

      for (r in 1:length(splitData)) {
        line <- as.numeric(splitData[[r]][-1])
        if ((lenDiff <- length(colHeaders) - length(line)) > 0)
          line <- c(line, rep(NA, lenDiff))
        mat[r,] <- line
      }

      blockList[[m]] <- mat

    }

    #aggregate sections
    aggMatCols <- do.call("c", lapply(blockList, colnames))
    aggMatRows <- rownames(blockList[[1]])
    aggMat <- matrix(NA, nrow=length(aggMatRows), ncol=length(aggMatCols), dimnames=list(aggMatRows, aggMatCols))

    for (l in blockList) {
      aggMat[rownames(l), colnames(l)] <- l #fill in just the block of the aggregate matrix represented in l
    }
  }
  else {
    #warning("No lines identified for matrix extraction using header: \n  ", headerLine)
    aggMat <- NULL
  }

  return(aggMat)

}
