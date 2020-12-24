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
extractSummaries_1section <- function(modelFitSection, arglist, filename, input=list()) {
  
  #DATA IMPUTATION outputs sometimes use the Mean/SD output (I believe in Mplus v6.12 and perhaps v7)
  #In Mplus v8, Model fit statistics are output as usual (e.g., ex11.6.out).
  #This is confusing, so we should just test for the Mean/SD output here and use the MI-type output if found
  useMIHeadings <- FALSE
  if (!is.null(input$data.imputation)) {
    header <- "Chi-Square Test of Model Fit"
    fields <- list(data.frame(
        varName=c("ChiSqM_DF", "ChiSqM_Mean", "ChiSqM_SD", "ChiSqM_NumComputations"),
        regexPattern=c("Degrees of Freedom", "Mean", "Std Dev", "Number of successful computations"),
        varType=c("int", "dec", "dec", "int"), stringsAsFactors=FALSE))
    
    test <- extractSummaries_1plan(arglist, header, fields, modelFitSection, filename)
    if (!is.na(test$ChiSqM_Mean)) {
      useMIHeadings <- TRUE
    }
  }
  
  #MI and Montecarlo data types have fundamentally different output (means and sds per fit stat)
  if (useMIHeadings || grepl("imputation", arglist$DataType, ignore.case=TRUE) || grepl("montecarlo", arglist$DataType, ignore.case=TRUE)) {
    modelFitSectionHeaders <- c(
      "", #section-nonspecific parameters
      "Chi-Square Test of Model Fit",
#      "Chi-Square Test of Model Fit for the Baseline Model",
      "Loglikelihood::H0 Value",
      "Loglikelihood::H1 Value",
      "CFI/TLI::CFI",
      "CFI/TLI::TLI",
      "Bayesian Posterior Predictive Checking using Chi-Square::Posterior Predictive P-Value",
      "Bayesian Prior Posterior Predictive Checking using Chi-Square::Prior Posterior Predictive P-Value",
      "Information Criteria( Including the Auxiliary Part)*::Akaike \\(AIC\\)",
      "Information Criteria( Including the Auxiliary Part)*::Bayesian \\(BIC\\)",
      "Information Criteria( Including the Auxiliary Part)*::Sample-Size Adjusted BIC \\(n\\* = \\(n \\+ 2\\) / 24\\)",
      "RMSEA \\(Root Mean Square Error Of Approximation\\)",
      "WRMR \\(Weighted Root Mean Square Residual\\)",
      "Information Criteri(a|on)::Deviance \\(DIC\\)", #
      "Information Criteri(a|on)::Estimated Number of Parameters \\(pD\\)",
      "Information Criteri(a|on)::Bayesian \\(BIC\\)"
    )
    modelFitSectionFields <- list(
      data.frame(
        varName=c("Parameters"), #defined outside of information criteria section for non-ML estimators
        regexPattern=c("^Number of Free Parameters"),
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
        varName=c("PostPred_PValue_Mean", "PostPred_PValue_SD", "PostPred_PValue_NumComputations"),
        regexPattern=c("Mean", "Std Dev", "Number of successful computations"),
        varType=c("dec", "dec", "int"), stringsAsFactors=FALSE
      ),
      data.frame(
        varName=c("PriorPostPred_PValue_Mean", "PriorPostPred_PValue_SD", "PriorPostPred_PValue_NumComputations"),
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
    } else {
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
      "{+3i}Chi-Square Test of Model Fit for the Binary and Ordered Categorical::{+2b}Pearson Chi-Square", #chi-square header spans two lines, so +3i
      "{+3i}Chi-Square Test of Model Fit for the Binary and Ordered Categorical::{+2b}Likelihood Ratio Chi-Square",
      "Chi-Square Test for MCAR under the Unrestricted Latent Class Indicator Model::{+2b}Pearson Chi-Square", #use blank line to find pearson within section
      "Chi-Square Test for MCAR under the Unrestricted Latent Class Indicator Model::{+2b}Likelihood Ratio Chi-Square",
      "Chi-Square Test for Difference Testing",
      "Loglikelihood( Including the Auxiliary Part)*",
      "CFI/TLI",
      "Information Criteria( Including the Auxiliary Part)*",
      "Information Criteria Including the Auxiliary Part",
      "RMSEA \\(Root Mean Square Error Of Approximation\\)",
      "WRMR \\(Weighted Root Mean Square Residual\\)",
      "Bayesian Posterior Predictive Checking using Chi-Square",
      "Information Criterion", #somehow singular for bayes output?
      "Wald Test of Parameter Constraints"
    )
    modelFitSectionFields <- list(
      data.frame(
        varName=c("Parameters"), #defined outside of information criteria section for non-ML estimators
        regexPattern=c("^Number of Free Parameters"), #only match beginning of line (aux section has its own indented variant)
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
        varName=c("ChiSqCategoricalPearson_Value", "ChiSqCategoricalPearson_DF", "ChiSqCategoricalPearson_PValue"),
        regexPattern=c("^\\s*Value", "Degrees of Freedom", "^\\s*P-Value"),
        varType=c("dec", "int", "dec"), stringsAsFactors=FALSE
      ),
      data.frame(
        varName=c("ChiSqCategoricalLRT_Value", "ChiSqCategoricalLRT_DF", "ChiSqCategoricalLRT_PValue"),
        regexPattern=c("^\\s*Value", "Degrees of Freedom", "^\\s*P-Value"),
        varType=c("dec", "int", "dec"), stringsAsFactors=FALSE
      ),
      data.frame(
        varName=c("ChiSqMCARUnrestrictedPearson_Value", "ChiSqMCARUnrestrictedPearson_DF", "ChiSqMCARUnrestrictedPearson_PValue"),
        regexPattern=c("^\\s*Value", "Degrees of Freedom", "^\\s*P-Value"),
        varType=c("dec", "int", "dec"), stringsAsFactors=FALSE
      ),
      data.frame(
        varName=c("ChiSqMCARUnrestrictedLRT_Value", "ChiSqMCARUnrestrictedLRT_DF", "ChiSqMCARUnrestrictedLRT_PValue"),
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
      data.frame( #Information Criteria (v8 now includes DIC and pD here)
        varName=c("AIC", "BIC", "aBIC", "DIC", "pD"),
        regexPattern=c("Akaike \\(AIC\\)", "Bayesian \\(BIC\\)", "Sample-Size Adjusted BIC", "Deviance \\(DIC\\)", "Estimated Number of Parameters \\(pD\\)"),
        varType=c("dec", "dec", "dec", "dec", "dec"), stringsAsFactors=FALSE
      ),
      data.frame(
        varName=c("ParametersWithAux"),
        regexPattern=c("Number of Free Parameters"),
        varType=c("int"), stringsAsFactors=FALSE
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
        varName=c("ObsRepChiSqDiff_95CI_LB", "ObsRepChiSqDiff_95CI_UB", "PostPred_PValue", "PriorPostPred_PValue"),
        regexPattern=c("+2:the Observed and the Replicated Chi-Square Values", "+2:the Observed and the Replicated Chi-Square Values", "^\\s*Posterior Predictive P-Value", "Prior Posterior Predictive P-Value"),
        varType=c("dec[1]", "dec[2]", "dec", "dec"), stringsAsFactors=FALSE
      ),
      data.frame( #Information Criterion (singular name under Mplus Bayes v7. Corrected to "Criteria" in v8)
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
    } else if (grepl("threelevel", arglist$AnalysisType, ignore.case=TRUE)) {
      modelFitSectionHeaders <- append(modelFitSectionHeaders, "SRMR \\(Standardized Root Mean Square Residual\\)")
      
      modelFitSectionFields <- c(modelFitSectionFields,
        list(data.frame(
            varName=c("SRMR.Within", "SRMR.Between.L2", "SRMR.Between.L3"),
            regexPattern=c("Value for Within", "Value for Between Level 2", "Value for Between Level 3"),
            varType=c("dec", "dec", "dec"), stringsAsFactors=FALSE
          ))
      )
    } else {
      
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
  class(warnerr$errors) <- c("mplus.errors", "list")
  class(warnerr$warnings) <- c("mplus.warnings", "list")
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
  class(input) <- c("mplus.inp", "list")
  
  startInput <- grep("^\\s*INPUT INSTRUCTIONS\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)
  if (length(startInput) == 0L) {
    warning("Could not find beginning of input for: ", filename)
    attr(input, "start.line") <- attr(input, "end.line") <- -1L
    return(input)
  } else { startInput <- startInput[1L] + 1L } #skip input instructions line itself
  
  endInput <- grep("^\\s*(INPUT READING TERMINATED NORMALLY|\\*\\*\\* WARNING.*|\\d+ (?:ERROR|WARNING)\\(S\\) FOUND IN THE INPUT INSTRUCTIONS|\\*\\*\\* ERROR.*)\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)
  if (length(endInput) == 0L) {
    #In Mplus v6.12 (and perhaps at some other point in the evolution), the input parser output was not included.
    #In such cases, try to fall back to the first line of the TITLE: XXX line, which is reprinted after input
    title1 <- grep("\\s*TITLE:\\s*(.*)$", outfiletext[1:100], perl=TRUE) #assume it lives in first 100 lines
    if (length(title1)==1L && length((endinputTitle <- grep(sub("\\s*TITLE:\\s*(.*)$", "^\\\\s*\\1\\\\s*$", outfiletext[title1], perl=TRUE), outfiletext)) == 1L)) {
      endInput <- endinputTitle - 1L
    } else {
      warning("Could not find end of input for: ", filename)
      attr(input, "start.line") <- attr(input, "end.line") <- -1
      return(input)
    }
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
  
  if (!is.null(input$data.imputation)) {
    arglist$NImputedDatasets <- input$data.imputation$ndatasets #number of imputed datasets
  }
  #End input instructions processing
  
  #BEGIN ANALYSIS SUMMARY PROCESSING
  analysisSummarySection <- getSection("^\\s*SUMMARY OF ANALYSIS\\s*$", outfiletext)
  
  arglist$Estimator <- extractValue(pattern="^\\s*Estimator\\s*", analysisSummarySection, filename, type="str")
  arglist$Observations <- extractValue(pattern="^\\s*Number of observations\\s*", analysisSummarySection, filename, type="int")
  # Fix for multigroup models, where Observations were not parsed correctly
  if(is.na(arglist$Observations)){
    arglist$Observations <- extractValue(pattern="^\\s*Total sample size\\s*", analysisSummarySection, filename, type="int")
  }
  arglist$NGroups <- extractValue(pattern="^\\s*Number of groups\\s*", analysisSummarySection, filename, type="int")
  arglist$NDependentVars <- extractValue(pattern="^\\s*Number of dependent variables\\s*", analysisSummarySection, filename, type="int")
  arglist$NIndependentVars <- extractValue(pattern="^\\s*Number of independent variables\\s*", analysisSummarySection, filename, type="int")
  arglist$NContinuousLatentVars <- extractValue(pattern="^\\s*Number of continuous latent variables\\s*", analysisSummarySection, filename, type="int")
  arglist$NCategoricalLatentVars <- extractValue(pattern="^\\s*Number of categorical latent variables\\s*", analysisSummarySection, filename, type="int")
  arglist$InformationMatrix <- extractValue(pattern="^\\s*Information matrix\\s*", analysisSummarySection, filename, type="int")
  
  #END ANALYSIS SUMMARY PROCESSING
  
  #BEGIN MODEL FIT STATISTICS PROCESSING
  #handle EFA output, which has separate model fit sections within each file
  #do this by extracting model fit sections for each and using an rbind call
  if (grepl("(?!MIXTURE|TWOLEVEL)\\s*EFA\\s+", arglist$AnalysisType, ignore.case=TRUE, perl=TRUE)) {

    EFASections <- grep(paste(c(
      "^\\s*(EXPLORATORY FACTOR ANALYSIS WITH \\d+ FACTOR\\(S\\):|",
      "EXPLORATORY FACTOR ANALYSIS WITH \\d+ WITHIN FACTOR\\(S\\) AND \\d+ BETWEEN FACTOR\\(S\\):|",
      "EXPLORATORY FACTOR ANALYSIS WITH \\d+ WITHIN FACTOR\\(S\\) AND UNRESTRICTED BETWEEN COVARIANCE:|",
      "EXPLORATORY FACTOR ANALYSIS WITH UNRESTRICTED WITHIN COVARIANCE AND \\d+ BETWEEN FACTOR\\(S\\):",
      ")\\s*$"), collapse=""), outfiletext, perl=TRUE)
    
    efa_nfac <- get_efa_nfactors(outfiletext[EFASections])

    mlefa <- nrow(efa_nfac) == 2L #a little error prone if we later build out the helper function, but okay for now
    
    if (!length(EFASections) > 0) { stop("Unable to locate section headers for EFA model fit statistics") }
    
    #need to convert from list to data.frame format to allow for proper handling of rbind below
    arglistBase <- as.data.frame(arglist, stringsAsFactors=FALSE)
    
    efaList <- list()
    for (thisFactor in 1:length(EFASections)) {
      #subset output by starting text to be searched at the point where factor output begins
      s_end <- ifelse(thisFactor==length(EFASections), length(outfiletext), EFASections[thisFactor+1])
      modelFitSection <- getSection_Blanklines("^(TESTS OF MODEL FIT|MODEL FIT INFORMATION)$", outfiletext[EFASections[thisFactor]:s_end])
      
      #N.B. Multilevel EFA outputs produce two top-level section headers for each combination of wi + bw factors
      #  But, MODEL FIT INFORMATION is only produced in the first section. Thus, the second section will not have this
      #  information, returning NULL for modelFitSection. This is no problem since we don't want to duplicate
      #  global fit information. Thus, a NULL indicates that we may safely skip to the next iteration.
      if (is.null(modelFitSection)) { next }
      
      efaList[[thisFactor]] <- extractSummaries_1section(modelFitSection, arglistBase, filename)
      if (isTRUE(mlefa)) {
        efaList[[thisFactor]]$NumFactors_Between <- efa_nfac["bw", thisFactor]
        efaList[[thisFactor]]$NumFactors_Within <- efa_nfac["wi", thisFactor]
      } else {
        efaList[[thisFactor]]$NumFactors <- efa_nfac["f", thisFactor]
      }
      
    }
    
    arglist <- do.call(rbind, efaList)
  } else if (length(multisectionMatches <- grep("^\\s*MODEL FIT INFORMATION FOR (?!THE LATENT CLASS INDICATOR MODEL PART).*", outfiletext, perl=TRUE, value=TRUE)) > 0L) {
    #use negative lookahead to ensure we don't grab the TECH10 output for LCA where it lists model fit info for latent class part
    #support Mplus v8 invariance testing outputs with one model fit section per variant (MODEL FIT INFORMATION FOR THE SCALAR MODEL etc.)
    #need to convert from list to data.frame format to allow for proper handling of rbind below
    arglistBase <- as.data.frame(arglist, stringsAsFactors=FALSE)
    multiList <- list()
    sectionNames <- sub("^\\s*MODEL FIT INFORMATION FOR\\s+(?:THE)*\\s*([\\w\\.]+)", "\\1", multisectionMatches, perl=TRUE)
    for (s in 1:length(multisectionMatches)) {
      fitinfo <- getSection(multisectionMatches[s], outfiletext)
      if (!is.null(fitinfo)) {
        multiList[[s]] <- extractSummaries_1section(fitinfo, arglistBase, filename, input)
      }      
    }
    arglist <- do.call(rbind, multiList)
    arglist$Model <- sectionNames #add model info
  } else {
    modelFitSection <- getSection("^(TESTS OF MODEL FIT|MODEL FIT INFORMATION)$", outfiletext)
    arglist <- extractSummaries_1section(modelFitSection, arglist, filename, input)
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
  class(arglist) <- c("mplus.summaries", "data.frame")
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
#' \item{ChiSqM_ScalingCorrection}{H0 Scaling Correction Factor}
#' \item{ObsRepChiSqDiff_95CI_LB}{Lower bound of 95\% confidence interval for the difference between observed and replicated chi-square values}
#' \item{ObsRepChiSqDiff_95CI_UB}{Upper bound of 95\% confidence interval for the difference between observed and replicated chi-square values}
#' \item{PostPred_PValue}{Posterior predictive p-value}
#' \item{PriorPostPred_PValue}{Prior Posterior Predictive P-Value}
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
  #message("This function is deprecated and will be removed from future versions of MplusAutomation. Please use readModels() instead.")
  message("extractModelSummaries has been deprecated. Please use readModels(\"nameofMplusoutfile.out\", what=\"summaries\")$summaries to replicate the old functionality.")
  
  return(invisible(NULL))
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

#' Helper subfunction to extract one section of OUTPUT: RESIDUALS
#' Can be called multiple times, as in the case of invariance testing
#' 
#' @param residSection The character vector containing strings for the residual section to be parsed
#' @param filename Character string containing the current output filename being parsed
#' @keywords internal
extractResiduals_1section <- function(residSection, filename) {
  #allow for multiple groups
  residSubsections <- getMultilineSection("ESTIMATED MODEL AND RESIDUALS \\(OBSERVED - ESTIMATED\\)( FOR .+)*",
                                          residSection, filename, allowMultiple=TRUE)
  
  matchlines <- attr(residSubsections, "matchlines")
  
  if (length(residSubsections) == 0) {
    warning("No sections found within residuals output.")
    return(list())
  } else if (length(residSubsections) > 1) {
    groupNames <- make.names(gsub("^\\s*ESTIMATED MODEL AND RESIDUALS \\(OBSERVED - ESTIMATED\\)( FOR (.+))*\\s*$", "\\2", residSection[matchlines], perl=TRUE))
  }
  
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
    targetList[["correlationEst"]] <- matrixExtract(residSubsections[[g]], "Model Estimated Correlations", filename)
    targetList[["correlationResid"]] <- matrixExtract(residSubsections[[g]], "Residuals for Correlations", filename)
    targetList[["slopeEst"]] <- matrixExtract(residSubsections[[g]], "Model Estimated Slopes", filename)
    targetList[["slopeResid"]] <- matrixExtract(residSubsections[[g]], "Residuals for Slopes", filename)
    
    if (length(residSubsections) > 1) {
      class(targetList) <- c("mplus.residuals", "list")
      residList[[groupNames[g]]] <- targetList
    } else{ 
      residList <- targetList
    }
    
  }
  
  class(residList) <- c("mplus.residuals", "list")
  if (length(residSubsections) > 1) { attr(residList, "group.names") <- groupNames }
  
  return(residList)
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
  #allow multiple model residual sections
  #mimic extractParameters_1file
  if (length(multisectionMatches <- grep("^\\s*RESIDUAL OUTPUT FOR .*", outfiletext, perl=TRUE, value=TRUE)) > 0L) {
    sectionNames <- make.names(sub("^\\s*RESIDUAL OUTPUT FOR\\s+(?:THE)*\\s*([\\w\\.]+)", "\\1", multisectionMatches, perl=TRUE))
    
    residList <- list()
    for (s in 1:length(sectionNames)) {
      residSection <- getSection(multisectionMatches[s], outfiletext)
      if (!is.null(residSection)) {
        residList[[ sectionNames[s] ]] <- extractResiduals_1section(residSection, filename) #[[1]]
      }
    }
  } else {
    residSection <- getSection("^RESIDUAL OUTPUT$", outfiletext)
    if (is.null(residSection)) { return(list()) } #no residuals output
    
    residList <- extractResiduals_1section(residSection) #[[1]]
  }
  
  # class(residList) <- c("mplus.residuals", "list")
  # if (length(residSubsections) > 1) { attr(residList, "group.names") <- groupNames }
  
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
  
  paramSpecSubsections <- getMultilineSection("PARAMETER SPECIFICATION( FOR [\\w\\d\\s\\.,_]+)*",
    tech1Section, filename, allowMultiple=TRUE)
  
  matchlines <- attr(paramSpecSubsections, "matchlines")
  
  paramSpecList <- list()
  if (length(paramSpecSubsections) == 0)
    warning ("No parameter specfication sections found within TECH1 output.")
  else if (length(paramSpecSubsections) > 1)
    groupNames <- make.names(gsub("^\\s*PARAMETER SPECIFICATION( FOR ([\\w\\d\\s\\.,_]+))*\\s*$", "\\2", tech1Section[matchlines], perl=TRUE))
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
    targetList[["delta"]] <- matrixExtract(paramSpecSubsections[[g]], "DELTA", filename)
    targetList[["gamma.c"]] <- matrixExtract(paramSpecSubsections[[g]], "GAMMA\\(C\\)", filename)
    targetList[["alpha.c"]] <- matrixExtract(paramSpecSubsections[[g]], "ALPHA\\(C\\)", filename)
    targetList[["new_additional"]] <- matrixExtract(paramSpecSubsections[[g]], "NEW/ADDITIONAL PARAMETERS", filename)
    
    #latent class indicator part includes subsections for each latent class, such as class-varying thresholds
    if (groupNames[g] == "LATENT.CLASS.INDICATOR.MODEL.PART") {
      tauLines <- grep("TAU\\(U\\) FOR LATENT CLASS \\d+", paramSpecSubsections[[g]], perl=TRUE, value=TRUE)
      uniqueLC <- unique(gsub("^\\s*TAU\\(U\\) FOR LATENT CLASS (\\d+)\\s*$", "\\1", tauLines, perl=TRUE))
      for (lc in uniqueLC) {
        targetList[[paste0("tau.u.lc", lc)]] <- matrixExtract(paramSpecSubsections[[g]], paste0("TAU\\(U\\) FOR LATENT CLASS ", lc), filename)
      }
    }
    
    if (length(paramSpecSubsections) > 1) {
      class(targetList) <- c("mplus.parameterSpecification", "list")
      paramSpecList[[groupNames[g]]] <- targetList
    }
    else
      paramSpecList <- targetList
  }
  
  class(paramSpecList) <- c("mplus.parameterSpecification", "list")
  if (length(paramSpecSubsections) > 1) attr(paramSpecList, "group.names") <- groupNames
  
  startValSubsections <- getMultilineSection("STARTING VALUES( FOR [\\w\\d\\s\\.,_]+)*",
    tech1Section, filename, allowMultiple=TRUE)
  
  matchlines <- attr(startValSubsections, "matchlines")
  
  startValList <- list()
  if (length(startValSubsections) == 0)
    warning ("No starting value sections found within TECH1 output.")
  else if (length(startValSubsections) > 1)
    groupNames <- make.names(gsub("^\\s*STARTING VALUES( FOR ([\\w\\d\\s\\.,_]+))*\\s*$", "\\2", tech1Section[matchlines], perl=TRUE))
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
    targetList[["delta"]] <- matrixExtract(startValSubsections[[g]], "DELTA", filename)
    targetList[["gamma.c"]] <- matrixExtract(startValSubsections[[g]], "GAMMA\\(C\\)", filename)
    targetList[["alpha.c"]] <- matrixExtract(startValSubsections[[g]], "ALPHA\\(C\\)", filename)
    targetList[["new_additional"]] <- matrixExtract(startValSubsections[[g]], "NEW/ADDITIONAL PARAMETERS", filename)
    
    #latent class indicator part includes subsections for each latent class, such as class-varying thresholds
    if (groupNames[g] == "LATENT.CLASS.INDICATOR.MODEL.PART") {
      tauLines <- grep("TAU\\(U\\) FOR LATENT CLASS \\d+", startValSubsections[[g]], perl=TRUE, value=TRUE)
      uniqueLC <- unique(gsub("^\\s*TAU\\(U\\) FOR LATENT CLASS (\\d+)\\s*$", "\\1", tauLines, perl=TRUE))
      for (lc in uniqueLC) {
        targetList[[paste0("tau.u.lc", lc)]] <- matrixExtract(startValSubsections[[g]], paste0("TAU\\(U\\) FOR LATENT CLASS ", lc), filename)
      }
    }
    
    if (length(startValSubsections) > 1) {
      class(targetList) <- c("mplus.startingValues", "list")
      startValList[[groupNames[g]]] <- targetList
    }
    else
      startValList <- targetList
  }
  
  class(startValList) <- c("mplus.startingValues", "list")
  if (length(startValSubsections) > 1) attr(startValList, "group.names") <- groupNames
  
  tech1List <- list(parameterSpecification=paramSpecList, startingValues=startValList)
  class(tech1List) <- c("mplus.tech1", "list")
  
  return(tech1List)
  
}

#' Helper function to extract the sample statistics from Mplus output
#' Depends on OUTPUT: SAMPSTAT
#' 
#' @param outfiletext The character vector containing all strings from output file
#' @param filename The current out file being parsed
#' @importFrom utils tail
#' @keywords internal
extractSampstat <- function(outfiletext, filename) {
  sampstatSection <- getSection("^SAMPLE STATISTICS$", outfiletext)
  if (is.null(sampstatSection)) {
    #try output from TYPE=BASIC, which places these in a section of a different name
    sampstatSection <- getSection("^RESULTS FOR BASIC ANALYSIS$", outfiletext)
  }
  
  if(!is.null(sampstatSection) & all(sampstatSection == "")){
    first_line <- (attr(outfiletext, "headerlines")[attr(outfiletext, "headerlines") > tail(attr(sampstatSection, "lines"), 1)][1]+1)
    final_line <- (attr(outfiletext, "headerlines")[attr(outfiletext, "headerlines") > tail(attr(sampstatSection, "lines"), 1)][2]-1)
    sampstatSection <- outfiletext[first_line:final_line]
  }
  
  sampstatList <- list()
  sampstatSubsections <- getMultilineSection("(ESTIMATED )*SAMPLE STATISTICS( FOR [\\w\\d\\s\\.,_]+)*",
    sampstatSection, filename, allowMultiple=TRUE)
  
  matchlines <- attr(sampstatSubsections, "matchlines")
  
  if(is.na(sampstatSubsections[1])){
    sampstatSubsections <- list(sampstatSection)
    matchlines <- attr(sampstatSubsections, "lines")
  }
  
  if (length(sampstatSubsections) == 0) {
    warning ("No sample statistics sections found within SAMPSTAT output.")
  } else if (length(sampstatSubsections) > 1) {
    groupNames <- make.names(gsub("^\\s*(?:ESTIMATED )*SAMPLE STATISTICS( FOR ([\\w\\d\\s\\.,_]+))*\\s*$", "\\2", sampstatSection[matchlines], perl=TRUE))
  } else { #just one section, no groups
    groupNames <- ""
  }
  
  for (g in 1:length(sampstatSubsections)) {
    targetList <- list()
    
    targetList[["means"]] <- matrixExtract(sampstatSubsections[[g]], "Means", filename)
    targetList[["covariances"]] <- matrixExtract(sampstatSubsections[[g]], "Covariances", filename)
    targetList[["correlations"]] <- matrixExtract(sampstatSubsections[[g]], "Correlations", filename)
    targetList[["correlations.vardiag"]] <- matrixExtract(sampstatSubsections[[g]], "CORRELATION MATRIX \\(WITH VARIANCES ON THE DIAGONAL\\)", filename, ignore.case=TRUE)
    
    #these seem to show up in DATA: TYPE=IMPUTATION outputs (e.g., ex11.8part2.out)
    targetList[["means.intercepts.thresholds"]] <- matrixExtract(sampstatSubsections[[g]], "Means/Intercepts/Thresholds", filename, ignore.case=TRUE)
    targetList[["within.level.variance.covariance"]] <- matrixExtract(sampstatSubsections[[g]], "WITHIN LEVEL VARIANCE/COVARIANCE", filename, ignore.case=TRUE)
    targetList[["within.level.correlation"]] <- matrixExtract(sampstatSubsections[[g]], "WITHIN LEVEL CORRELATION", filename, ignore.case=TRUE)
    targetList[["between.level.variance.covariance"]] <- matrixExtract(sampstatSubsections[[g]], "BETWEEN LEVEL VARIANCE/COVARIANCE", filename, ignore.case=TRUE)
    targetList[["between.level.correlation"]] <- matrixExtract(sampstatSubsections[[g]], "BETWEEN LEVEL CORRELATION", filename, ignore.case=TRUE)
    
    #I think these are only in older outputs
    targetList[["covariances.correlations.resid_correlations"]] <- matrixExtract(sampstatSubsections[[g]], "Covariances/Correlations/Residual Correlations", filename)
    targetList[["slopes"]] <- matrixExtract(sampstatSubsections[[g]], "Slopes", filename)
    
    #latent class indicator part includes subsections for each latent class, such as class-varying thresholds
#    if (groupNames[g] == "LATENT.CLASS.INDICATOR.MODEL.PART") {
#      tauLines <- grep("TAU\\(U\\) FOR LATENT CLASS \\d+", sampstatSubsections[[g]], perl=TRUE, value=TRUE)
#      uniqueLC <- unique(gsub("^\\s*TAU\\(U\\) FOR LATENT CLASS (\\d+)\\s*$", "\\1", tauLines, perl=TRUE))
#      for (lc in uniqueLC) {
#        targetList[[paste0("tau.u.lc", lc)]] <- matrixExtract(sampstatSubsections[[g]], paste0("TAU\\(U\\) FOR LATENT CLASS ", lc), filename)
#      }
#    }
    
    if (length(sampstatSubsections) > 1) {
      class(targetList) <- c("mplus.sampstat", "list")
      sampstatList[[groupNames[g]]] <- targetList
    } else{
      sampstatList <- targetList
    }
    
  }
  
  ##Extract Univariate counts and proportions
  univariateCountsSection <- getSection("^UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES$", outfiletext)
  
  #remove warning lines, which throw off the parser (e.g., ex6.15.out)
  univariateCountsSection <- univariateCountsSection[!grepl("\\s*WARNING:.*", univariateCountsSection, perl=TRUE)]
  
  if (!is.null(univariateCountsSection)) {
    countSubsections <- getMultilineSection("Group\\s+([\\w\\d\\.,_]+)*",
      univariateCountsSection, filename, allowMultiple=TRUE)
    
    matchlines <- attr(countSubsections, "matchlines")
    
    if (!is.list(countSubsections) && is.na(countSubsections[1])) {
      countSubsections <- list(univariateCountsSection) #no sublists by group
    } else if (length(countSubsections) > 1)
      groupNames <- make.names(gsub("^\\s*Group\\s+([\\w\\d\\s\\.,_]+)\\s*$", "\\1", univariateCountsSection[matchlines], perl=TRUE))
    else #just one section, no groups
      stop("not sure how we got here")
    
    for (g in 1:length(countSubsections)) {
      targetList <- list()
      
      df <- data.frame(do.call(rbind, strsplit(trimSpace(parseCatOutput(countSubsections[[g]])), "\\s+", perl=TRUE)), stringsAsFactors=FALSE)
      names(df) <- c("variable", "proportion", "count")
      df$proportion <- as.numeric(df$proportion)
      df$count <- as.numeric(df$count)
      
      #divide variable column into variable and category for clarity
      df$category <- as.numeric(sub(".*\\.Cat\\.(\\d+)", "\\1", df$variable, perl=TRUE))
      df$variable <- sub("^(.*)\\.Cat\\.\\d+$", "\\1", df$variable, perl=TRUE)
      df <- df[,c("variable", "category", "proportion", "count")] #reorder df
      
      #targetList[["proportions.counts"]] <- df
      targetList <- df #just a single element at the moment
      
      class(targetList) <- c("mplus.propcounts.data.frame", "data.frame")
      
      if (length(countSubsections) > 1) {
        #class(targetList) <- c("mplus.propcounts", "list")
        sampstatList[[groupNames[g]]][["proportions.counts"]] <- targetList
      } else {
        sampstatList[["proportions.counts"]] <- targetList
      }
        
    }
  }

  # Extract univariate sample statistics ------------------------------------

  univariate_sampstat <- getSection("^UNIVARIATE SAMPLE STATISTICS$", outfiletext)
  if (!is.null(univariate_sampstat)) {
    
    #group-wise headings don't follow Mplus indentation conventions. Hack these by prefixing with X, then use getMultilineSection to parse
    has_groups <- any(which_g <- grepl("UNIVARIATE HIGHER-ORDER MOMENT DESCRIPTIVE STATISTICS(?: FOR [\\w\\d\\s\\.,_]+)*", univariate_sampstat, perl=TRUE))
    if (has_groups) {
      univariate_sampstat[which_g] <- sub("^(.*)$", "X\\1", univariate_sampstat[which_g], perl=TRUE)
      univariateSubsections <- getMultilineSection("X.*UNIVARIATE HIGHER-ORDER MOMENT DESCRIPTIVE STATISTICS(?: FOR [\\w\\d\\s\\.,_]+)*",
                                              univariate_sampstat, filename, allowMultiple=TRUE)
    } else {
      univariateSubsections <- NA
    }

    matchlines <- attr(univariateSubsections, "matchlines")
    
    if(is.na(univariateSubsections[1])) {
      univariateSubsections <- list(univariate_sampstat)
      matchlines <- attr(univariateSubsections, "lines")
    }
    
    if (length(univariateSubsections) == 0L) {
      warning ("No univariate statistics sections found within SAMPSTAT output.")
    } else if (length(univariateSubsections) > 1) {
      groupNames <- make.names(gsub("^X\\s*UNIVARIATE HIGHER-ORDER MOMENT DESCRIPTIVE STATISTICS( FOR ([\\w\\d\\s\\.,_]+))*\\s*$", "\\2", univariate_sampstat[matchlines], perl=TRUE))
    } else { #just one section, no groups
      groupNames <- ""
    }
    
    for (g in 1:length(univariateSubsections)) {
      thisSub <- univariateSubsections[[g]]
      stats <- lapply(thisSub[grepl("\\d$", thisSub)], function(x) { strsplit(trimws(x), split = "\\s+")[[1]] })
      if(length(stats) %% 2 == 0) {
        out <- cbind(do.call(rbind, stats[seq(1, length(stats), by = 2)]),
                     do.call(rbind, stats[seq(2, length(stats), by = 2)]))
        
        #headers <- univariate_sampstat[grepl("\\/", univariate_sampstat)]
        #headers <- gsub("%", " %", headers)
        #headers <- lapply(trimws(headers), function(x){strsplit(x, "\\s{2,}")[[1]]})
        #headers[[1]] <- c(gsub("\\/", "", headers[[1]][grepl("\\/", headers[[1]])]), gsub("\\/.*$", "", headers[[2]][grepl("\\/", headers[[2]])]))
        #headers[[2]] <- gsub("^.+?\\/", "", headers[[2]])
        #colnames(out) <- gsub(" %", "%", c(headers[[1]], headers[[2]]))
        var_names <- out[, 1]
        out <- gsub("%", "", out)
        out <- out[,-1, drop=FALSE] #drop variable column
        dd <- dim(out)
        out <- matrix(apply(out, 2, as.numeric), nrow=dd[1], ncol=dd[2]) #need to preserve dimensions in the single-row case
        colnames(out) <- c("Mean", "Skewness", "Minimum", "%Min", "20%", "40%", "Median", "Sample Size", "Variance", "Kurtosis", "Maximum", "%Max", "60%", "80%")
        rownames(out) <- var_names
        
        out <- out[, c("Sample Size", "Mean", "Variance", "Skewness", "Kurtosis", "Minimum", "Maximum", "%Min", "%Max", "20%", "40%", "Median", "60%", "80%"), drop=FALSE]
        
        if (length(univariateSubsections) > 1) {
          #class(targetList) <- c("mplus.propcounts", "list")
          sampstatList[[groupNames[g]]][["univariate.sample.statistics"]] <- out
        } else {
          sampstatList[["univariate.sample.statistics"]] <- out
        }
      }
    }
    
  }
  class(sampstatList) <- c("mplus.sampstat", "list")
  if (length(sampstatSubsections) > 1) attr(sampstatList, "group.names") <- groupNames
  
  return(sampstatList)
  
}

extractCovarianceCoverage <- function(outfiletext, filename) {
  #TODO: Return type is sometimes list, sometimes matrix; a bit inconsistent
  covcoverageSection <- getSection("^COVARIANCE COVERAGE OF DATA$", outfiletext)
  if (is.null(covcoverageSection)) { return(list()) } #no COVARIANCE COVERAGE OF DATA output
  
  covcoverageList <- list()
  
  covcoverageSubsections <- getMultilineSection("PROPORTION OF DATA PRESENT( FOR [\\w\\d\\s\\.,_]+)*",
    covcoverageSection, filename, allowMultiple=TRUE)
  
  matchlines <- attr(covcoverageSubsections, "matchlines")
  
  if (length(covcoverageSubsections) == 0 || is.na(covcoverageSubsections)) { #See UG ex9.7.out
    message("No PROPORTION OF DATA PRESENT sections found within COVARIANCE COVERAGE OF DATA output.")
    return(covcoverageList)
  } else if (length(covcoverageSubsections) > 1) {
    groupNames <- make.names(gsub("^\\s*PROPORTION OF DATA PRESENT( FOR ([\\w\\d\\s\\.,_]+))*\\s*$", "\\2", covcoverageSection[matchlines], perl=TRUE))
  } else { #just one section, no groups
    groupNames <- ""
  }
  
  for (g in 1:length(covcoverageSubsections)) {
    #targetList <- list()
    
    #for now, there is just one matrix extracted, so no need to label it or treat it as a list. Leaving scaffolding commented out if useful later
    #targetList[["covcoverage"]] <- matrixExtract(covcoverageSubsections[[g]], "Covariance Coverage", filename)
    
    targetList <- matrixExtract(covcoverageSubsections[[g]], "Covariance Coverage", filename)
    
    if (length(covcoverageSubsections) > 1) {
      #class(targetList) <- c("mplus.covcoverage", "list")
      covcoverageList[[groupNames[g]]] <- targetList
    }
    else
      covcoverageList <- targetList
  }
  
  if (is.list(covcoverageList)) { class(covcoverageList) <- c("mplus.covcoverage", "list")
  } else { class(covcoverageList) <- c("mplus.covcoverage", "matrix") } #single numeric matrix
  if (length(covcoverageSubsections) > 1) { attr(covcoverageList, "group.names") <- groupNames }
  
  return(covcoverageList)
  
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
  
  if (isEmpty(filename)) return(NULL)
  
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
  
  class(tech3List) <- c("mplus.tech3", "list")
  
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
  
  tech4Subsections <- getMultilineSection("ESTIMATES DERIVED FROM THE MODEL( FOR [\\w\\d\\s\\.,_]+)*",
    tech4Section, filename, allowMultiple=TRUE)
  
  matchlines <- attr(tech4Subsections, "matchlines")
  
  if (length(tech4Subsections) == 0) {
    warning("No sections found within TECH4 output.")
    return(list())
  }
  else if (length(tech4Subsections) > 1) {
    groupNames <- make.names(gsub("^\\s*ESTIMATES DERIVED FROM THE MODEL( FOR ([\\w\\d\\s\\.,_]+))*\\s*$", "\\2", tech4Section[matchlines], perl=TRUE))
  }
  
  for (g in 1:length(tech4Subsections)) {
    targetList <- list()
    
    targetList[["latMeansEst"]] <- matrixExtract(tech4Subsections[[g]], "ESTIMATED MEANS FOR THE LATENT VARIABLES", filename)
    targetList[["latCovEst"]] <- matrixExtract(tech4Subsections[[g]], "ESTIMATED COVARIANCE MATRIX FOR THE LATENT VARIABLES", filename)
    targetList[["latCorEst"]] <- matrixExtract(tech4Subsections[[g]], "ESTIMATED CORRELATION MATRIX FOR THE LATENT VARIABLES", filename)
    
    if (length(tech4Subsections) > 1) {
      class(targetList) <- c("mplus.tech4", "list")
      tech4List[[groupNames[g]]] <- targetList
    }
    else
      tech4List <- targetList
    
  }
  
  class(tech4List) <- c("mplus.tech4", "list")
  
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
      class(targetList) <- c("mplus.tech7", "list")
      tech7List[[groupNames[g]]] <- targetList
    }
    else
      tech7List <- targetList
  }
  
  class(tech7List) <- c("mplus.tech7", "list")
  
  return(tech7List)
}


#' Extract Technical 8 from Mplus
#'
#' The TECH8 option is used to print the optimization history of a model.
#' It also prints the potential scale reduction in Bayesian models.
#'
#' @param outfiletext the text of the output file
#' @param filename The name of the file
#' @return A list of class \dQuote{mplus.tech8}
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractTech8 <- function(outfiletext, filename) {
  #not sure whether there are sometimes multiple groups within this section.
  #for now, this function only extract PSR in Bayes models
  tech8Section <- getSection("^TECHNICAL 8 OUTPUT$", outfiletext)
  tech8List <- list()
  class(tech8List) <- c("mplus.tech8", "list")
  psr <- data.frame(); class(psr) <- c("mplus.psr.data.frame", "data.frame"); tech8List[["psr"]] <- psr
  
  if (is.null(tech8Section)) return(tech8List) #no tech8 output

  #psr extraction subfunction
  extractPSR <- function(text) {
    startline <- grep("ITERATION\\s+SCALE REDUCTION\\s+HIGHEST PSR", text, perl=TRUE)
    if (length(startline) > 0L) {
      firstBlank <- which(text == "")
      firstBlank <- firstBlank[firstBlank > startline][1L] #first blank after starting line
      toparse <- text[(startline+1):firstBlank]
      psr <- data.frame(matrix(as.numeric(unlist(strsplit(trimSpace(toparse), "\\s+", perl=TRUE))), ncol=3, byrow=TRUE, dimnames=list(NULL, c("iteration", "psr", "param.highest.psr"))))
      class(psr) <- c("mplus.psr.data.frame", "data.frame")
      return(psr)
    } else {
      return(NULL)
    }
  }    
  
  bayesPSR <- getMultilineSection("TECHNICAL 8 OUTPUT FOR BAYES ESTIMATION", tech8Section, filename, allowMultiple=FALSE)
  
  if (!is.na(bayesPSR[1L])) {
    #new outputs have "Iterations for model estimation" and "Iterations for computing PPPP"
    if (any(grepl("Iterations for computing PPPP", bayesPSR))) {
      pppp_text <- getSection("Iterations for computing PPPP", bayesPSR, headers = c("Iterations for computing PPPP", "Iterations for model estimation"))
      model_text <- getSection("Iterations for model estimation", bayesPSR, headers = c("Iterations for computing PPPP", "Iterations for model estimation"))
      tech8List[["psr"]] <- extractPSR(model_text)
      tech8List[["psr_pppp"]] <- extractPSR(pppp_text)
    } else {
      tech8List[["psr"]] <- extractPSR(bayesPSR)
    }
  }
  
  return(tech8List)
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
  class(tech9List) <- c("mplus.tech9", "list")
  
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
  
  bivarFit <- getSection("^\\s*BIVARIATE MODEL FIT INFORMATION\\s*$", outfiletext)
  
  if (is.null(bivarFit)) return(tech10List) 
  
  # Build data structures
  bivarFitData <- matrix(nrow=length(bivarFit), ncol=7)
  bivarFitStats <- matrix(nrow=0, ncol=4)
  
  # Skip header lines
  bivarFit <- bivarFit[6:length(bivarFit)]

  vars <- NULL
  lastPearson <- NULL
  mPos <- 1
    
  for (l in 1:length(bivarFit)) {
    if (grepl("^\\s*$", bivarFit[l], perl = TRUE)) { next }
    
    if (grepl("^\\s{5}\\S", bivarFit[l], perl = TRUE)) {
      # Parse new vars line
      vars <- unlist(strsplit(trimSpace(bivarFit[l]), "\\s+", perl = TRUE))
    }
    else if (grepl("Bivariate (Pearson|Log-Likelihood) Chi-Square", bivarFit[l], perl = TRUE)) {
      if (grepl("Overall", bivarFit[l], perl = TRUE)) { next } # Skip 'overall' values
      
      m <- unlist(regmatches(bivarFit[l], regexec("Bivariate (Pearson|Log-Likelihood) Chi-Square\\s+(\\S+)", bivarFit[l], perl = TRUE)))
      
      if (m[2] == 'Pearson') {
        lastPearson <- m[3]
      }
      else {
        bivarFitStats <- rbind(bivarFitStats, c(vars, lastPearson, m[3]))
      }
    }
    else {
      values <- unlist(strsplit(trimSpace(bivarFit[l]), "\\s{2,}", perl = TRUE))
      
      bivarFitData[mPos,] <-c(vars,values)
      mPos <- mPos + 1
    }
  }
  
  # Remove empty rows, and convert to data.frame
  bivarFitData <- bivarFitData[rowSums(is.na(bivarFitData)) != ncol(bivarFitData),]
  bivarFitData <- as.data.frame( bivarFitData, stringsAsFactors = FALSE )
  names(bivarFitData) <- c("var1", "var2", "cat1", "cat2", "h0", "h1", "z")
  
  # Fix data types
  bivarFitData[,c("h0", "h1", "z")] <- as.numeric(unlist(bivarFitData[,c("h0", "h1", "z")]))
  
  bivarFitStats <- setNames(data.frame(bivarFitStats, stringsAsFactors = FALSE), c("var1","var2","pearson","log-likelihood"))
  bivarFitStats[,c("pearson","log-likelihood")] <- as.numeric(unlist(bivarFitStats[,c("pearson","log-likelihood")]))
  
  tech10List$bivar_model_fit_info <- bivarFitData
  tech10List$bivar_chi_square <- bivarFitStats
  
  
  return(tech10List)
  
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
  #not sure whether there are sometimes multiple groups within this section.
  tech12Section <- getSection("^TECHNICAL 12 OUTPUT$", outfiletext)
  tech12List <- list()
  class(tech12List) <- c("mplus.tech12", "list")
  
  if (is.null(tech12Section)) return(tech12List) #no tech12 output
  
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
      class(targetList) <- c("mplus.tech12", "list")
      tech12List[[g]] <- targetList #no known case where there are many output sections
    }
    else
      tech12List <- targetList
  }
  
  class(tech12List) <- c("mplus.tech12", "list")
  
  return(tech12List)
}



#' Extract Technical 15 from Mplus
#'
#' The TECH15 option is used in conjunction with TYPE=MIXTURE to request conditional probabilities
#' for the latent class variables.
#'
#' @param outfiletext the text of the output file
#' @param filename The name of the file
#' @return A list of class \dQuote{mplus.tech15}
#' @keywords internal
#' @seealso \code{\link{matrixExtract}}
#' @examples
#' # make me!!!
extractTech15 <- function(outfiletext, filename) {
  tech15Section <- getSection("^TECHNICAL 15 OUTPUT$", outfiletext)
  tech15List <- list(conditional.probabilities = trimws(tech15Section[grepl("^\\s+?P\\(", tech15Section)]))
  class(tech15List) <- c("mplus.tech15", "list")
  
  if (is.null(tech15Section)) return(tech15List) #no tech15 output
  
  return(tech15List)
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
  class(fssList) <- c("mplus.facscorestats", "list")
  
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
extractClassCounts <- function(outfiletext, filename, summaries) {
  
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
  
  if (is.null(summaries) || missing(summaries) || summaries$NCategoricalLatentVars==1 || is.na(summaries$NCategoricalLatentVars)) {
    #Starting in Mplus v7.3 and above, formatting of the class counts appears to have changed...
    #Capture the alternatives here
    if (is.null(summaries) || missing(summaries) || is.null(summaries$Mplus.version) || as.numeric(summaries$Mplus.version) < 7.3) {
      modelCounts <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES$", outfiletext)
      ppCounts <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS$", outfiletext)
      mostLikelyCounts <- getSection("^CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP$", outfiletext)
    } else {
      modelCounts <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES$::^BASED ON THE ESTIMATED MODEL$", outfiletext)
      ppCounts <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES$::^BASED ON ESTIMATED POSTERIOR PROBABILITIES$", outfiletext)
      mostLikelyCounts <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES$::^BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP$", outfiletext)
    }
    
    countlist[["modelEstimated"]] <- getClassCols(modelCounts)
    countlist[["posteriorProb"]] <- getClassCols(ppCounts)
    countlist[["mostLikely"]] <- getClassCols(mostLikelyCounts)
    
    #most likely by posterior probability section
    mostLikelyProbs <- getSection("^Average Latent Class Probabilities for Most Likely Latent Class Membership \\((Row|Column)\\)$", outfiletext)
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
    #also, starting ~Mplus 7.3, the columns and rows appear to have switched in this and the logit section (hence the Column|Row syntax)
    classificationProbs <- getSection("^Classification Probabilities for the Most Likely Latent Class Membership \\((Column|Row)\\)$", outfiletext)
    if (length(classificationProbs) > 1L) { classificationProbs <- classificationProbs[-1L] } #remove line 1: "by Latent Class (Column)"
    
    countlist[["classificationProbs.mostLikely"]] <- unlabeledMatrixExtract(classificationProbs, filename)
    
    #same, but for classification probability logits
    classificationLogitProbs <- getSection("^Logits for the Classification Probabilities for the Most Likely Latent Class Membership \\((Column|Row)\\)$", outfiletext)
    if (length(classificationLogitProbs) > 1L) { classificationLogitProbs <- classificationLogitProbs[-1L] } #remove line 1: "by Latent Class (Column)"
    
    countlist[["logitProbs.mostLikely"]] <- unlabeledMatrixExtract(classificationLogitProbs, filename)
  } else {
    # Extract class_counts for multiple categorical latent variables.
    
    getClassCols_lta <- function(sectiontext) {
      numberLines <- grep("^\\s*([a-zA-Z0-9]+)?(\\s+[0-9\\.-]{1,}){1,}$", sectiontext, perl=TRUE)
      if (length(numberLines) > 0) {
        parsedlines <- strsplit(trimSpace(sectiontext[numberLines]), "\\s+", perl=TRUE)
        num_values <- sapply(parsedlines, length)
        if(length(unique(num_values)) == 1){
          counts <- data.frame(t(sapply(parsedlines, as.numeric)), stringsAsFactors = FALSE)
        } else {
          # Pad shorter lines with NA on the left side
          parsedlines[which(num_values != max(num_values))] <- lapply(parsedlines[which(num_values != max(num_values))], function(x){
              c(rep(NA, (max(num_values) - length(x))), x)
            })
          counts <- do.call(rbind, parsedlines)
          # Repeat existing values on subsequent rows in columns containing NAs
          counts[,1] <- inverse.rle(list(lengths = diff(c(which(!is.na(counts[,1])), (nrow(counts)+1))), values = counts[,1][complete.cases(counts[,1])]))
          counts <- data.frame(counts, stringsAsFactors = FALSE)
          counts[, 2:4] <- lapply(counts[, 2:4], as.numeric)
        }
        return(counts)
      } else {
        return(NULL)
      }
    }
    
    if (missing(summaries) || is.null(summaries$Mplus.version) || as.numeric(summaries$Mplus.version) < 7.3) {
      posteriorProb.patterns <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES$::^BASED ON ESTIMATED POSTERIOR PROBABILITIES$", outfiletext)
      mostLikely.patterns <- getSection("^CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN$", outfiletext)
      mostLikelyCounts <- getSection("CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN", outfiletext)
    } else {
      posteriorProb.patterns <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS$::^BASED ON ESTIMATED POSTERIOR PROBABILITIES$", outfiletext)
      mostLikely.patterns <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS$::^BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN$", outfiletext)
      mostLikelyCounts <- getSection("^FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE$::^BASED ON THEIR MOST LIKELY LATENT CLASS PATTERN$", outfiletext)
    }
    
    # Class counts
    countlist[["modelEstimated"]] <- getClassCols_lta(
      getSection(
        "^FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE$::^BASED ON THE ESTIMATED MODEL$",
        outfiletext
      )
    )
    countlist[["posteriorProb"]] <- getClassCols_lta(
      getSection(
        "^FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE$::^BASED ON ESTIMATED POSTERIOR PROBABILITIES$",
        outfiletext
      )
    )
    countlist[["mostLikely"]] <- getClassCols_lta(mostLikelyCounts)
    
    countlist[which(names(countlist) %in% c("modelEstimated", "posteriorProb", "mostLikely"))] <-
      lapply(countlist[which(names(countlist) %in% c("modelEstimated", "posteriorProb", "mostLikely"))],
        setNames,
        c("variable", "class", "count", "proportion"))
    
    # Patterns
    countlist[["modelEstimated.patterns"]] <-
      getClassCols_lta(
        getSection(
          "^FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS$::^BASED ON THE ESTIMATED MODEL$",
          outfiletext
        )
      )
    
    countlist[["posteriorProb.patterns"]] <- getClassCols_lta(posteriorProb.patterns)
    countlist[["mostLikely.patterns"]] <- getClassCols_lta(mostLikely.patterns)
    
    # Determine order of column names for categorical latent variables
    morder <- getSection("MODEL RESULTS USE THE LATENT CLASS VARIABLE ORDER", outfiletext)
    if (!is.null(morder)) {
      first_present <- which(morder != "")[1]
      catvar_names <- strsplit(trimSpace(morder[first_present]), "\\s+")[[1]]
    } else {
      catvar_names <- unique(countlist[["mostLikely"]]$variable) #fall back approximation
      #stop("Unable to determine names of categorical latent variables.")
    }
    
    rename_sections <- which(names(countlist) %in% c("modelEstimated.patterns", "posteriorProb.patterns", "mostLikely.patterns"))
    if (length(rename_sections) > 0L) {
      for (s in rename_sections) {
        countlist[[s]] <- setNames(countlist[[s]], c(catvar_names, "count", "proportion"))
      }
    }
    
    #Average latent class probabilities (not always present in outputs)
    avgProbs <- getSection("^Average Latent Class Probabilities for Most Likely Latent Class Pattern \\((Row|Column)\\)$::^by Latent Class Pattern \\((Row|Column)\\)$", outfiletext)
    
    if (!is.null(avgProbs)) {
      column_headers <- strsplit(trimws(grep("\\s*Latent Class\\s{2,}", avgProbs, value = TRUE)), "\\s+", perl=TRUE)[[1]][-1]
      variable_pattern_rows <- grep(paste(c("^(\\s{2,}\\d+){", length(column_headers), "}$"), collapse = ""), avgProbs, perl=TRUE)
      variable_pattern_rows <- variable_pattern_rows[!c(FALSE, diff(variable_pattern_rows) != 1)]
      variable_patterns <- avgProbs[variable_pattern_rows]
      variable_patterns <- data.frame(t(sapply(strsplit(trimws(variable_patterns), "\\s+", perl=TRUE), as.numeric)))
      names(variable_patterns) <- c("Latent Class Pattern No.", column_headers[-1])
      
      probs <- grep(paste(c("^\\s+\\d{1,}(\\s{2,}[0-9\\.-]+)+$"), collapse = ""), avgProbs[(variable_pattern_rows[length(variable_pattern_rows)]+1):length(avgProbs)], perl=TRUE, value = TRUE)
      # If the table is truncated, concatenate its parts
      if(length(probs) %% nrow(variable_patterns) > 1){
        for(i in 2:(length(probs) %% nrow(variable_patterns))){
          probs[1:(nrow(variable_patterns)+1)] <- 
            paste(probs[1:(nrow(variable_patterns)+1)],
                  substring(probs[((i-1)*(nrow(variable_patterns)+1)+1):(i*(nrow(variable_patterns)+1))], first = 8)
            )
        }
        probs <- probs[1:nrow(variable_patterns)]
      }
      probs <- t(sapply(strsplit(trimws(probs[-1]), "\\s+", perl=TRUE), as.numeric))[,-1]
      
      countlist[["avgProbs.mostLikely"]] <- probs
      countlist[["avgProbs.mostLikely.patterns"]] <- variable_patterns
    }
    
    # AFAIK this section is not reported for multiple categorical variables
    countlist[["classificationProbs.mostLikely"]] <- NULL
    # AFAIK this section is not reported for multiple categorical variables
    countlist[["logitProbs.mostLikely"]] <- NULL
    
    transitionProbs <- getSection("^LATENT TRANSITION PROBABILITIES BASED ON THE ESTIMATED MODEL$", outfiletext)
    if(!is.null(transitionProbs)){
      section_starts <- grep("\\(Columns\\)$", transitionProbs)
      transitionProbs <- mapply(FUN = function(begin, end){
          probs <- grep("^\\s+\\d{1,}(\\s{2,}[0-9\\.-]{2,}){1,}$", transitionProbs[begin:end], perl=TRUE, value = TRUE)
          probs <- do.call(rbind, strsplit(trimws(probs), "\\s+", perl=TRUE))[,-1]
          cbind(paste(gsub("\\s+(\\w+) Classes.*$", "\\1", transitionProbs[begin]) , ".", rep(c(1:nrow(probs)), ncol(probs)), sep = ""),
            paste(gsub(".+?by (\\w+) Classes.*$", "\\1", transitionProbs[begin]) , ".", as.vector(sapply(1:ncol(probs), rep, nrow(probs))), sep = ""),
            as.vector(probs))
        },
        begin = section_starts, end = c(section_starts[-1], length(transitionProbs)), SIMPLIFY = FALSE)
      if(length(transitionProbs) > 1){
        transitionProbs <- do.call(rbind, transitionProbs)
      } else {
        transitionProbs <- transitionProbs[[1]]
      }
      transitionProbs <- data.frame(transitionProbs, stringsAsFactors = FALSE)
      names(transitionProbs) <- c("from", "to", "probability")
      transitionProbs$probability <- as.numeric(transitionProbs$probability)
    }
    countlist[["transitionProbs"]] <- transitionProbs
  }
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
#' @param ignore.case Whether to ignore case of header line
#' @param expect_sig Whether to track value significance TRUE/FALSE (* in Mplus) as an attribute
#' @return a matrix
#' @keywords internal
#' @examples
#' # make me!!!
matrixExtract <- function(outfiletext, headerLine, filename, ignore.case=FALSE, expect_sig=FALSE) {
  
  matLines <- getMultilineSection(headerLine, outfiletext, filename, allowMultiple=TRUE, ignore.case=ignore.case)
  
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
      
      #May 2017: in Mplus v7*, there is a header on the beginning of each row, including for vectors such as NU,TAU, etc.
      #example:
      # NU
      #       Y             X1            X2            W
      #       ________      ________      ________      ________
      # 1           0             0             0             0
      
      #in Mplus v8, the "1" header on parameter vectors has been removed.
      # NU
      #    Y12T3         Y13T3         Y14T3
      #    ________      ________      ________
      #          0             0             0
      
      #To overcome this problem, check the number of columns in splitData compared to the number of column headers.
      #If the number of columns is equal to the number of column headers, add a "1" at the beginning to make parsing code
      #  consistent with v7 and expectation is matrix assembly in the aggMat section below.
      #Only add this tweak if the first element of v is not identical to any column header.
      #Otherwise this will add a "1" to some rows that are part of a matrix, not param vector.
      splitData <- lapply(splitData, function(v) {
          if (length(v) == length(colHeaders) && (! v[1L] %in% colHeaders)) { v <- c("1", v) }
          return(v)
        })
      
      #pull out row names from each element
      rowHeaders <- sapply(splitData, "[", 1)
      
      mat <- matrix(NA_real_, nrow=length(rowHeaders), ncol=length(colHeaders),
        dimnames=list(rowHeaders, colHeaders))
      
      if (isTRUE(expect_sig)) { attr(mat, "sig") <- array(NA, dim=dim(mat)) }
      
      for (r in 1:length(splitData)) {
        #use mplus_as.numeric to handle D+XX scientific notation in output and sig values
        line <- mplus_as.numeric(splitData[[r]][-1], expect_sig=expect_sig)
        if (length(line) == 0L) { next } #occasionally have blank rows, skip assignment into mat
        
        mat[r,1:length(line)] <- line
        if (isTRUE(expect_sig)) { attr(mat, "sig")[r,1:length(line)] <- attr(line, "sig") }
      }
      
      blockList[[m]] <- mat
    }
    
    #aggregate sections
    aggMatCols <- do.call("c", lapply(blockList, colnames))
    aggMatRows <- rownames(blockList[[1]]) #row names are shared across blocks in Mplus output
    aggMat <- matrix(NA, nrow=length(aggMatRows), ncol=length(aggMatCols), dimnames=list(aggMatRows, aggMatCols))
    attr(aggMat, "sig") <- array(NA, dim=dim(aggMat), dimnames = dimnames(aggMat))
    
    #Unfortunately, due to Mplus 8-character printing limits for matrix sections, row/col names are not guaranteed to be unique.
    #This causes problems for using name-based matching to fill the matrix.
    #We know that blocks are printed from left-to-right by column (i.e., the block 1 has the first X columns, block 2 has the next Y columns).
    #Thus, we should be able to use a counter and fill columns numerically. This does not get around a problem of non-unique row names since we
    #can't easily discern the rows represented in a block based on row numbering alone. Thus, this is an incomplete solution for now (Aug2015 MH)
    colCounter <- 1
    for (l in blockList) {
      aggMat[rownames(l), colCounter:(colCounter + ncol(l) - 1)] <- l #fill in just the block of the aggregate matrix represented in l
      if (isTRUE(expect_sig)) { attr(aggMat, "sig")[rownames(l), colCounter:(colCounter + ncol(l) - 1)] <- attr(l, "sig") } #add sig values
      colCounter <- colCounter + ncol(l)
    }
  } else {
    #warning("No lines identified for matrix extraction using header: \n  ", headerLine)
    aggMat <- NULL
  }
  
  #if sig is not used, ensure that we dump it as an attribute
  if (isFALSE(expect_sig)) { attr(aggMat, "sig") <- NULL }
  
  return(aggMat)
  
}

#EXTRACT DATA SUMMARY SECTION
#NB. This does not support three-level output yet!

#' Function to extract the SUMMARY OF DATA section from Mplus outputs
#'
#' @param outfiletext The text of the output file 
#' @param filename the name of the file containing textToScan. Used to make more intelligible warning messages.
#' @keywords internal
extractDataSummary <- function(outfiletext, filename) {
  dataSummarySection <- getSection("^\\s*SUMMARY OF DATA( FOR THE FIRST DATA SET)*\\s*$", outfiletext)
  if (is.null(dataSummarySection)) {
    empty <- list()
    class(empty) <- c("mplus.data_summary", "list")
    return(empty)
  }
  
  #detect groups
  multipleGroupMatches <- grep("^\\s*Group \\w+(?:\\s+\\(\\d+\\))*\\s*$", dataSummarySection, ignore.case=TRUE, perl=TRUE) #support Mplus v8 syntax Group G1 (0) with parentheses of numeric value
  if (length(multipleGroupMatches) > 0L) {
    groupNames <- sub("^\\s*Group (\\w+)(?:\\s+\\(\\d+\\))*\\s*$", "\\1", dataSummarySection[multipleGroupMatches], perl=TRUE)
    toparse <- list() #divide into a list by group
    for (i in 1:length(multipleGroupMatches)) {
      if (i < length(multipleGroupMatches)) {
        end <- multipleGroupMatches[i+1] - 1
      } else { end <- length(dataSummarySection) }
      section <- dataSummarySection[(multipleGroupMatches[i]+1):end]
      attr(section, "group.name") <- groupNames[i]
      toparse[[groupNames[i]]] <- section
    }    
  } else {
    attr(dataSummarySection, "group.name") <- "all"
    toparse <- list(all=dataSummarySection)
  }
  
  summaries <- c()
  iccs <- c()
  for (section in toparse) {
    summaries <- rbind(summaries, data.frame(
        NClusters = extractValue(pattern="^\\s*Number of clusters\\s*", section, filename, type="int"),
        NMissPatterns = extractValue(pattern="^\\s*Number of missing data patterns\\s*", section, filename, type="int"),
        AvgClusterSize = extractValue(pattern="^\\s*Average cluster size\\s*", section, filename, type="dec"),
        Group=attr(section, "group.name")
      ))
    
    #parse icc
    icc_start <- grep("^\\s*Estimated Intraclass Correlations for the Y Variables( for [\\w\\._]+ level)*\\s*$", section, perl=TRUE)
    
    iccout <- c()
    if (length(icc_start) > 0L) {
      to_parse <- trimSpace(section[(icc_start+1):length(section)]) #this assumes nothing comes afterwards in the section
      #problem: there is an unknown number of columns in this output. Example:
      #
      #           Intraclass              Intraclass              Intraclass
      #Variable  Correlation   Variable  Correlation   Variable  Correlation
      #Q22          0.173      Q38          0.320      Q39          0.127
      #Q40          0.270
      
      #solution: variables are always odd positions, correlations are always even
      
      repeat_line <- grep("(\\s*Variable\\s+Correlation\\s*)+", to_parse)
      if (length(repeat_line) == 1L) {
        #x <- to_parse[repeat_line] #not needed with odd/even solution
        #nrepeats <- length(regmatches(x, gregexpr("g", x)))
        
        icc_values <- strsplit(to_parse[(repeat_line+1):length(to_parse)], "\\s+")
        for (ss in icc_values) {
          if (length(ss) > 0L) {
            positions <- 1:length(ss)
            vars <- ss[positions[positions %% 2 != 0]]
            vals <- ss[positions[positions %% 2 == 0]]
            iccout <- rbind(iccout, data.frame(variable=vars, ICC=as.numeric(vals), stringsAsFactors=FALSE))
          }          
        }
        
        iccout$Group <- attr(section, "group.name")
        iccs <- rbind(iccs, iccout)
      }      
    }        
  }
  
  #trim out "all" in single group case
  if (length(multipleGroupMatches) == 0L) {
    summaries$Group <- NULL
    iccs$Group <- NULL    
  }
  retlist <- list(overall=summaries, ICCs=iccs) 
  class(retlist) <- c("mplus.data_summary", "list")
  return(retlist)   
}

#Caspar van Lissa code for extract invariance testing section
extractInvarianceTesting <- function(outfiletext, filename) {
  inv_test_firstline <- grep("^Invariance Testing$", outfiletext)
  if (length(inv_test_firstline) == 0L) { return(list()) } #invariance section not found
  
  inv_test_endline <- grep("^MODEL FIT INFORMATION", outfiletext)
  
  retlist <- list()
  inv_test_endline <- inv_test_endline[inv_test_endline > inv_test_firstline][1]
  
  inv_test <- outfiletext[(inv_test_firstline+2):(inv_test_endline-3)]

  #match words followed by four groups of numbers (Number of parameters, Chi-Square, Degrees of Freedom, P-Value)
  model_rows <- grep("^\\s+?\\w+(\\s{2,}[0-9.]+){4}$", inv_test, value = TRUE)
  model_rows <- t(sapply(model_rows, function(x){strsplit(trimws(x), "\\s+")[[1]]}, USE.NAMES = FALSE))
  model_rownames <- model_rows[, 1]
  model_rows <- apply(model_rows[, -1, drop=FALSE], c(1,2), as.numeric)
  row.names(model_rows) <- model_rownames
  colnames(model_rows) <- c("Parameters", "Chi-Square", "DF", "Pvalue")
  retlist$models <- model_rows
  
  
  test_rows <- grep("^\\s+?(\\w+\\s){3}(\\s{2,}[0-9.]+){3}$", inv_test, value = TRUE)
  test_rows <- t(sapply(test_rows, function(x){strsplit(trimws(x), "\\s{2,}")[[1]]}, USE.NAMES = FALSE))
  test_rownames <- test_rows[, 1]
  test_rows <- apply(test_rows[, -1, drop=FALSE], c(1,2), as.numeric)
  row.names(test_rows) <- test_rownames
  colnames(test_rows) <- c("Chi-Square", "DF", "Pvalue")
  retlist$compared <- test_rows
  
  return(retlist)
}
