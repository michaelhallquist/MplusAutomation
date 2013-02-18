readModels <- function(target=getwd(), recursive=FALSE, filefilter) {
  #large wrapper function to read summaries, parameters, and savedata from one or more output files.
	
	outfiles <- getOutFileList(target, recursive, filefilter)
	
	allFiles <- list()
	for (curfile in outfiles) {
    cat("Reading model: ", curfile, "\n")
		#if not recursive, then each element is uniquely identified (we hope!) by filename alone
		if (recursive==FALSE)	listID <- make.names(splitFilePath(curfile)$filename) #each list element is named by the respective file
		else listID <- make.names(curfile) #each list element is named by the respective file

		outfiletext <- scan(curfile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE)

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
    allFiles[[listID]]$tech9 <- extractTech9(outfiletext, curfile) #tech 9 output (errors and warnings for Monte Carlo output)
    allFiles[[listID]]$fac_score_stats <- extractFacScoreStats(outfiletext, curfile) #factor scores mean, cov, corr assoc with PLOT3 
    
    #aux(e) means
    allFiles[[listID]]$lcCondMeans <- extractAuxE_1file(outfiletext, curfile)
    
    #add class tag for use with compareModels
    class(allFiles[[listID]]) <- c("list", "mplus.model")
    attr(allFiles[[listID]], "filename") <- curfile
    
    #cleanup summary columns containing only NAs
    for (col in names(allFiles[[listID]]$summaries)) {
      if (all(is.na(allFiles[[listID]]$summaries[[col]]))) allFiles[[listID]]$summaries[[col]] <- NULL
    }
    
    #check for gh5 file, and load if possible
    gh5 <- list()
    gh5fname <- sub("^(.*)\\.out$", "\\1.gh5", curfile, ignore.case=TRUE, perl=TRUE)
    if (file.exists(gh5fname)) {
      if(suppressWarnings(require(hdf5))) {
        #use load=FALSE to return named list, which is appended to model object.
        gh5 <- hdf5load(file=gh5fname, load=FALSE, verbosity=0, tidy=TRUE)
      } else { warning("Unable to read gh5 file because hdf5 package not installed. Please install.packages(\"hdf5\")\n  Note: this depends on having an installation of the hdf5 library on your system.") }
    }
    allFiles[[listID]]$gh5 <- gh5
	}

  if (length(outfiles)==1) {
    allFiles <- allFiles[[1]] #no need for single top-level element when there is only one file    
  } else {
    class(allFiles) <- c("list", "mplus.model.list")
  }
  
  return(allFiles)		
}

extractValue <- function(pattern, textToScan, filename, type="int") {
#extractValue(pattern, outfile, type="int")
#
#   pattern: the exact text to be matched in the outfile that identifies the parameter of interest
#   textToScan: the chunk of Mplus output to be parsed, passed as a vector of character strings (from the scan command).
#		filename: the name of the file containing textToScan. Used to make more intelligible warning messages.
#   type: the data type of the parameter, which determines the regexp used. Currently can be "int", "dec", "str", or "calc".
#
#   Description: An internal function used by extractSummaries_1file to extract parameters from the output file using regular expressions.
#
  
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

#new approach to multiline section: retain spaces and look for next
#line that has identical indentation.
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
        if (allowMultiple==TRUE)
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


#sectionHeaders is a character vector with headers for each section of interest
#sectionFields is a list of data.frames where each data.frame specifies the fields to be extracted for that section
#make this a more generic function that accepts headers and fields in case it is useful outside the MODEL FIT section
extractSummaries_1plan <- function(arglist, sectionHeaders, sectionFields, textToParse, filename) {
  if (length(sectionHeaders) < 1) stop("No section headers provided.")
  
  #multiple sections
  for (header in 1:length(sectionHeaders)) {
    #a blank section header indicates to match anywhere in the textToParse
    if (sectionHeaders[header] == "") sectionText <- textToParse
    
		#could be pretty inefficient if the same section header is repeated several times.
		#could build a list with divided output and check whether a section is present in the list before extracting
		else sectionText <- getMultilineSection(sectionHeaders[header], textToParse, filename)
    
    #process all fields for this section
    sectionFieldDF <- sectionFields[[header]]
    #browser()
    for (i in 1:nrow(sectionFieldDF)) {
      thisField <- sectionFieldDF[i,] 
      arglist[[ thisField$varName ]] <- extractValue(pattern=thisField$regexPattern, sectionText, filename, type=thisField$varType)
    }
  }

  return(arglist)
  
}


extractSummaries_1section <- function(modelFitSection, arglist, filename) {
	#function to extract model fit statistics from a section
	#wrapped to allow for multiple fit sections, as in EFA files.
	
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
				"WRMR \\(Weighted Root Mean Square Residual\\)"
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
        "Information Criterion" #somehow singular for bayes output?
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
            varName=c("DIC", "pD"),
            regexPattern=c("Deviance \\(DIC\\)", "Estimated Number of Parameters \\(pD\\)"),
            varType=c("dec", "dec"), stringsAsFactors=FALSE
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

divideIntoFields <- function(section.text, required) {
  #helper function to divide an input section into key-value pair list
  #taken from mplus2lavaan
  if (is.null(section.text)) { return(NULL) }
  section.split <- strsplit(paste(section.text, collapse=" "), ";", fixed=TRUE)[[1]]
  
  section.divide <- list()
  
  for (cmd in section.split) {
    if (grepl("^\\s*!.*", cmd, perl=TRUE)) next #skip comment lines
    if (grepl("^\\s+$", cmd, perl=TRUE)) next #skip blank lines
    
    #force text matches at word boundary, or just split on = (\b doesn't work for =)
    cmd.split <- strsplit(cmd[1L], "(\\b(IS|ARE|is|are|Is|Are)\\b|=)", perl=TRUE)[[1]]
    if (length(cmd.split) < 2L) { 
      #anomaly: no is/are/=
      if (tolower(cmd.spacesplit <- strsplit(trimSpace(cmd[1L]), "\\s+", perl=TRUE)[[1L]])[1L] == "usevariables") {
        #for now, tolerate syntax usevariables x1-x10;
        cmd.split[1L] <- cmd.spacesplit[1L]
        cmd.split[2L] <- paste(cmd.spacesplit[2L:length(cmd.spacesplit)], collapse=" ") #rejoin rhs with spaces
      } else {
        stop("First line not dividing into LHS and RHS: ", cmd[1L])        
      }
    } else if (length(cmd.split) > 2L) {
      #probably more than one equals sign, such as knownclass: KNOWNCLASS = g (grp = 1 grp = 2 grp = 3)
      cmd.split[2L] <- paste(cmd.split[-1L], collapse="=") #pull out lhs [-1] and join other strings with =
      cmd.split <- cmd.split[1L:2L] #just retain the lhs and (joined) rhs elements
    }
        
    cmdName <- trimSpace(cmd.split[1L]) #lhs
    cmdArgs <- trimSpace(cmd.split[2L]) #rhs
    
    section.divide[[make.names(tolower(cmdName))]] <- cmdArgs
    
  }
  
  if (!missing(required)) { stopifnot(all(required %in% names(section.divide))) }
  return(section.divide)
}

extractWarningsErrors_1file <- function(outfiletext, filename, input) {
  warnerr <- list(warnings=list(), errors=list())
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
  
  startWarnErr <- attr(input, "end.line") + 1L
  
  endWarnErr <- grep("^\\s*(INPUT READING TERMINATED NORMALLY|\\*\\*\\* WARNING.*|\\d+ (?:ERROR|WARNING)\\(S\\) FOUND IN THE INPUT INSTRUCTIONS|\\*\\*\\* ERROR.*)\\s*$", outfiletext, ignore.case=TRUE, perl=TRUE)
  if (length(endWarnErr) == 0L) {
    return(warnerr) #unable to find end of warnings (weird), or there are none.
  }
  
  #The above will match all of the possible relevant lines.
  #To identify warnings section, need to go to first blank line after the final warning or error. (look in next 100 lines)
  lastWarn <- endWarnErr[length(endWarnErr)]
  blank <- which(outfiletext[lastWarn:(lastWarn + 100 )] == "")[1L] + lastWarn - 1 
  
  warnerrtext <- outfiletext[startWarnErr[1L]:(blank-1)]
  
  lines <- friendlyGregexpr("^\\s*(\\*\\*\\* WARNING|\\*\\*\\* ERROR).*\\s*$", warnerrtext, perl=TRUE)
  
  w <- 1
  e <- 1
  
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
  
  return(warnerr)
  
}

extractInput_1file <- function(outfiletext, filename) {
  #function to extract and parse mplus input syntax from the output file. 
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

extractSummaries_1file <- function(outfiletext, filename, input, extract=c("Title", "LL", "BIC", "AIC", "AICC",
  "Parameters", "Observations", "BLRT", "RMSEA", "CFI", "TLI", "ChiSqModel", "aBIC", 
  "Estimator", "SRMR", "WRMR", "ChiSqBaseline"))
{
#extractSummaries_1file(outfiletext, filename)
#   outfiletext: this is the output file in string form to be parsed. Passed in from extractModelSummaries.
#   filename: name of the file being parsed. Used in case of bad model, prints a warning.
#
#   Description: This function parses an output file for specific model details.
#   It returns a list of model details for a single output file.

  #preallocates list
  #arglist = vector("list", length(extract))  
  arglist <- list()
  
  #obtain mplus software version
  if ((mplus.version <- regexpr("\\s*Mplus VERSION ([\\d\\.]+)\\s*", outfiletext[1L], perl=TRUE)) > 0L) {
    arglist$Mplus.version <- substr(outfiletext[1L], attr(mplus.version, "capture.start")[1L], attr(mplus.version, "capture.start")[1L] + attr(mplus.version, "capture.length")[1L] - 1)    
  }

  ###Copy some elements of the input instructions into the summaries
  
  #copy title into arglist
  if ("Title" %in% extract && !is.null(input$title)) {
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
  } else {
    arglist$DataType <- "INDIVIDUAL" #Data type not specified, default to individual
  }

	#End input instructions processing  


  #BEGIN ANALYSIS SUMMARY PROCESSING
  analysisSummarySection <- getSection("^\\s*SUMMARY OF ANALYSIS\\s*$", outfiletext)

  if ("Estimator" %in% extract)
    arglist$Estimator <- extractValue(pattern="^\\s*Estimator\\s*", analysisSummarySection, filename, type="str")

  if ("Observations" %in% extract)
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
  
  
  #the BLRT keyword serves as a placeholder for extracting many fields, like km1likelihood

#tech14headers <- c(
#    "Random Starts Specifications for the k-1 Class Analysis Model",
#    "Random Starts Specification for the k-1 Class Model for Generated Data",
#    "Random Starts Specification for the k Class Model for Generated Data",
#    "Number of bootstrap draws requested",
#    "PARAMETRIC BOOTSTRAPPED LIKELIHOOD RATIO TEST FOR \\d+ \\(H0\\) VERSUS \\d+ CLASSES"
#
#)
#tech14fields <- list(
#    data.frame(
#        varName=c("T11_KM1Starts", "T11_KM1Final"),
#        regexPattern=c("Number of initial stage random starts", "Number of final stage optimizations"),
#        varType=c("int", "int"), stringsAsFactors=FALSE
#    ),
#    data.frame(
#        varName=c("T11_KM1LL", "T11_VLMR_2xLLDiff", "T11_VLMR_ParamDiff", "T11_VLMR_Mean", "T11_VLMR_SD", "T11_VLMR_PValue"), 
#        regexPattern=c("H0 Loglikelihood Value", "2 Times the Loglikelihood Difference", "Difference in the Number of Parameters", "Mean", "Standard Deviation", "P-Value"), 
#        varType=c("dec", "dec", "int", "dec", "dec", "dec"), stringsAsFactors=FALSE
#    ),
#    data.frame(
#        varName=c("T11_LMR_Value", "T11_LMR_PValue"), 
#        regexPattern=c("^\\s*Value", "^\\s*P-Value"), 
#        varType=c("dec", "dec"), stringsAsFactors=FALSE
#    )
#)
  if ("BLRT" %in% extract) {
    
    #locate the beginning of the BLRT section
    matchlines <- grep("TECHNICAL 14 OUTPUT", outfiletext)
    
    if (length(matchlines) == 1) {
      #match up through the end of the file
      endRange <- grep("3463 Stoner Ave\\.", outfiletext)
      
      if (!length(endRange) == 1) {
        stop("Problem identifying end marker for BLRT")
      }
      
      blrtpiece <- outfiletext[matchlines:endRange]
      
      arglist$BLRT_KM1LL <- extractValue(pattern="H0 Loglikelihood Value", blrtpiece, filename, type="dec")
      arglist$BLRT_PValue <- extractValue(pattern="Approximate P-Value", blrtpiece, filename, type="dec")
      arglist$BLRT_Numdraws <- extractValue(pattern="Successful Bootstrap Draws", blrtpiece, filename, type="int")
    }
    else {
      #warning("Could not locate BLRT section, despite being requested")
      
      #need to pad the expected fields with NAs to keep the list length consistent, permitting correct rbind
      arglist$BLRT_KM1LL <- as.numeric(NA)
      arglist$BLRT_PValue <- as.numeric(NA)
      arglist$BLRT_Numdraws <- as.numeric(NA)
    }
  }

  
	#calculate adjusted AIC per Burnham & Anderson(2004), which is better than AIC for non-nested model selection
	#handle AICC calculation, requires AIC, Parameters, and observations
  if (all(c("AICC", "AIC", "Parameters", "Observations") %in% extract)) {
		if (!is.null(arglist$Parameters) && !is.na(arglist$Parameters) &&
				!is.null(arglist$AIC) && !is.na(arglist$AIC) &&
				!is.null(arglist$Observations) && !is.na(arglist$Observations))
			arglist$AICC <- arglist$AIC + (2*arglist$Parameters*(arglist$Parameters+1))/(arglist$Observations-arglist$Parameters-1)
		else
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


extractModelSummaries <- function(target=getwd(), recursive=FALSE, filefilter) {
#extractModelSummaries(target, recursive=FALSE)
#
#   target: the directory containing Mplus output files to read. Use forward slashes in directory name (e.g., "C:/Users/Mplus/"). Defaults to working directory.
#   recursive: specifies whether to parse output files in subdirectories beneath the specified directory. Defaults to FALSE. (TRUE or FALSE)
#
#   Description: This function identifies all Mplus .out files in the specified directory (directory parameter)
#   and reads basic model fit information from each file. The function combines fit details across models into a list.
#
#   Example: myModels <- extractModelSummaries("C:/Documents and Settings/Michael/My Documents/Mplus Stuff/", recursive=TRUE)
  
#	require(plyr)
	
	#retain working directory and reset at end of run
  curdir <- getwd()
  
	outfiles <- getOutFileList(target, recursive, filefilter)
	
  details <- list()
  
  #for each output file, use the extractSummaries_1file function to extract relevant data
  #note that extractSummaries_1file returns data as a list
  #rbind creates an array of lists by appending each extractSummaries_1file return value
  for (i in 1:length(outfiles)) {
    #read the file
    readfile <- scan(outfiles[i], what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE)
    
    #bomb out for EFA files
    if (length(grep("TYPE\\s+(IS|=|ARE)\\s+((MIXTURE|TWOLEVEL)\\s+)+EFA\\s+\\d+", readfile, ignore.case=TRUE, perl=TRUE)) > 0) {
      warning(paste("EFA, MIXTURE EFA, and TWOLEVEL EFA files are not currently supported by extractModelSummaries.\n  Skipping outfile: ", outfiles[i], sep=""))
      next #skip file
    }
    
    #append params for this file to the details array
    #note that this is a memory-inefficient solution because of repeated copying. Better to pre-allocate.
		
    inp <- extractInput_1file(readfile, outfiles[i])
    details[[i]] <- extractSummaries_1file(readfile, outfiles[i], inp)        
  }
  
  #if there are several output files, then use rbind.fill to align fields
  if (length(details) > 1L) details <- do.call(plyr::rbind.fill, details)
  else details <- details[[1L]]
  
  #reset working directory
  setwd(curdir)

	#cleanup columns containing only NAs
	for (col in names(details)) {
		if (all(is.na(details[[col]]))) details[[col]] <- NULL
	}
		
	return(details)
}


addHeaderToSavedata <- function(outfile, directory=getwd()) {
  
}


#a helper function to be used by wrappers that generate HTML, LaTex, and on-screen displays of summary statistics
subsetModelList <- function(modelList, keepCols, dropCols, sortBy) {
#  require(plyr)
  
  #if passed an mplus.model.list from readModels, then just extract summaries for disply
  if (inherits(modelList, "mplus.model.list")) {
    modelList <- do.call("rbind.fill", sapply(modelList, "[", "summaries"))
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
    
    if (length(keepCols) == 0) stop("All fields passed as keepCols are missing from data.frame\n  Fields in data.frame are:\n  ", paste(strwrap(paste(summaryNames, collapse=" ", sep=""), width=80, exdent=4), collapse="\n"))
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

#display summary table in a separate window
showSummaryTable <- function(modelList, keepCols, dropCols, sortBy, font="Courier 9") {
#  require(relimp)

  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)
  showData(MplusData, font=font, placement="+30+30", maxwidth=150, maxheight=50, rownumbers=FALSE, title="Mplus Summary Table")
}

#create HTML table
HTMLSummaryTable <- function(modelList, filename=file.path(getwd(), "Model Comparison.html"), keepCols, dropCols, sortBy, display=FALSE) {
#  require(xtable)
  #create HTML table and write to file.
  
  #ensure that the filename has a .html or .htm at the end
  if (!length(grep(".*\\.htm[l]*", filename)) > 0) {
    filename <- paste(filename, ".html", sep="")
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
    shell.exec(paste("file:///", filename, sep=""))
  }
 
}

LatexSummaryTable <- function(modelList, keepCols, dropCols, sortBy, label=NULL, caption=NULL) {
  #return latex table to caller
  #require(xtable)
  
  MplusData <- subsetModelList(modelList, keepCols, dropCols, sortBy)

  return(xtable(MplusData, label=label, caption=caption))
}

#removed input instructions from routine extraction
#dropCols=c("InputInstructions", "Observations")

createTable <- function(modelList, filename=file.path(getwd(), "Model Comparison.html"),
  sortby="AICC", display=TRUE, latex=FALSE, dropCols=c("Observations"), label=NULL) {
#createTable(directory, recursive=FALSE)
#
#   modelList: list of model details returned by extractModelSummaries.
#   basedir: directory in which to save the HTML table. Defaults to current directory.
#   filename: name of HTML table file. Defaults to model comparison.html
#   sortby: name of field on which to sort. Defaults to "AICC". "BIC" and "AIC" are options.
#   display: whether to load the HTML table in the browser after creating it. Defaults to TRUE. (TRUE/FALSE) 
#
#   Description: This function generates an HTML table from a list of models generated by extractModelSummaries.
#
#   Example: createTable(myModels, "C:/Documents and Settings/Michael/My Documents/Mplus Stuff/", "my comparison.html", sortby="BIC")
  
  #retain working directory to reset at end of run
  #curdir <- getwd()
  #setwd(basedir)
  
  #require(xtable)
  
  #convert modelList (which defaults to array of lists) to data frame
  dframe <- as.data.frame(modelList)
  
  #Process vector of columns to drop
  for (column in dropCols) {
    dframe[[column]] <- NULL
  }
  
  #sort the data frame according the sortby value
  sortTab <- dframe[order(unlist(dframe[, sortby])), ]
  
  #note that the sorting was previously not working because unlist automatically drops NULL values
  #switched code to use NAs, which is more appropriate.
  
  #ensure that the filename has a .html or .htm at the end
  if (!length(grep(".*\\.htm[l]*", filename)) > 0) {
    filename <- paste(filename, ".html", sep="")
  }                                      
  
  if (length(grep("[\\/]", filename)) == 0) {
    #Filename does not contain a path. Therefore, add the working directory
    filename <- file.path(getwd(), filename)
  }
  
  if (latex==FALSE) {
    print(
        x=xtable(sortTab),
        type="html",
        file=filename,
        include.rownames = FALSE,
        NA.string = "."
    )
    
    if (display) {
      #load table in browser
      shell.exec(paste("file:///", filename, sep=""))
    }
  }  
  
  #reset working directory
  #setwd(curdir)
  
  if (latex==TRUE) return(xtable(sortTab, label=label))
  
}

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
    
    targetList[["meanEst"]] <- matrixExtract(residSubsections[[g]], "Model Estimated Means/Intercepts/Thresholds", filename)
    targetList[["meanResid"]] <- matrixExtract(residSubsections[[g]], "Residuals for Means/Intercepts/Thresholds", filename)
    targetList[["meanResid.std"]] <- matrixExtract(residSubsections[[g]], "Standardized Residuals \\(z-scores\\) for Means/Intercepts/Thresholds", filename)
    targetList[["meanResid.norm"]] <- matrixExtract(residSubsections[[g]], "Normalized Residuals for Means/Intercepts/Thresholds", filename)
    targetList[["covarianceEst"]] <- matrixExtract(residSubsections[[g]], "Model Estimated Covariances/Correlations/Residual Correlations", filename)
    targetList[["covarianceResid"]] <- matrixExtract(residSubsections[[g]], "Residuals for Covariances/Correlations/Residual Correlations", filename)
    targetList[["covarianceResid.std"]] <- matrixExtract(residSubsections[[g]], "Standardized Residuals \\(z-scores\\) for Covariances/Correlations/Residual Corr", filename)
    targetList[["covarianceResid.norm"]] <- matrixExtract(residSubsections[[g]], "Normalized Residuals for Covariances/Correlations/Residual Correlations", filename)
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

#Function for reading "free" output where a sequence of values populates a matrix









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
  
  values <- scan(savedataFile, what="character", strip.white=FALSE, blank.lines.skip=FALSE)
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

extractTech10 <- function(outfiletext, filename) {
  tech10Section <- getSection("^TECHNICAL 10 OUTPUT$", outfiletext)
  if (is.null(tech10Section)) return(list()) #no tech4 output
  
  tech10List <- list()
  
}

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
  
  mostLikelyProbs <- getSection("^Average Latent Class Probabilities for Most Likely Latent Class Membership \\(Row\\)$", outfiletext)
  
  if (length(mostLikelyProbs) > 0) {
    
    #Example:
    #Average Latent Class Probabilities for Most Likely Latent Class Membership (Row)
    #by Latent Class (Column)
    #
    #  			1        2
    #
    #  1   0.986    0.014
    #  2   0.030    0.970
    
    #A bit of a wonky section. Some notes: 
    # 1) Rows represent those hard classified into that class.
    # 2) Rows sum to 1.0 and represent the summed average posterior probabilities of all the class assignment possibilities.
    # 3) Columns represent average posterior probabilitity of being in class 1 for those hard classified as 1 or 2.
    # 4) High diagonal indicates that hard classification matches posterior probability patterns.
    
    #processing is also custom.
    #first line is "by Latent Class (Column)"
    #second line is blank
    #third line contains the number of classes, which is useful for matrix setup.
    
    classLabels <- as.numeric(strsplit(trimSpace(mostLikelyProbs[3]), "\\s+", perl=TRUE)[[1]])
    mlpp_probs <- matrix(NA, nrow=length(classLabels), ncol=length(classLabels), dimnames=
            list(hardClassified=paste("ml.c", classLabels, sep=""),
                posteriorProb=paste("pp.c", classLabels, sep="")))
    
    #fourth line is blank
    #fifth line begins probabilities
    firstProbLine <- 5
    blankLine <- which(mostLikelyProbs[firstProbLine:length(mostLikelyProbs)] == "")[1] + (firstProbLine - 1) #need to add offset to get absolute position in overall section
    probSection <- mostLikelyProbs[firstProbLine:(blankLine-1)]
    mlpp_probs[,] <- do.call(rbind, lapply(strsplit(trimSpace(probSection), "\\s+", perl=TRUE), as.numeric))[,-1]
    
    countlist[["avgProbs.mostLikely"]] <- mlpp_probs
  }

  return(countlist)
}

#main worker function for extracting Mplus matrix output
#where matrices are spread across blocks to keep within width constraints
#example: tech1 matrix output.
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
