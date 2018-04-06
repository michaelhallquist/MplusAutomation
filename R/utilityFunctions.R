#' Lookup the matrix element for a give parameter number
#'
#' The \code{lookupTech1Parameter} function identifies the position in the Mplus model
#' matrices corresponding to a given parameter defined in the TECHNICAL 1 PARAMETER
#' SPECIFICATION OUTPUT. The goal of this function is to aid in identifying problematic parameters
#' often printed in the warnings and errors section of Mplus output.
#'
#' @param tech1Output The object corresponding to the TECH1 parameter specification from readModels.
#' @param paramNumber The parameter number to lookup
#' @return A \code{data.frame} containing the row(s) and column(s) of TECH1 parameter specification matching
#' the requested \code{paramNumber}.
#' @author Michael Hallquist
#' @export
#' @keywords interface
#' @seealso \code{\link{readModels}}
#' @examples
#' \dontrun{
#'   models <- readModels("test1.out")
#'   param <- lookupTech1Parameter(models$tech1, 16)
#' }
lookupTech1Parameter <- function(tech1Output, paramNumber) {
  #accept mplus model object
  if (inherits(tech1Output, c("mplus.model"))) {
    tech1Output <- tech1Output$tech1
  }
  
  #or accept $tech1
  if (!inherits(tech1Output, c("mplus.parameterSpecification", "mplus.tech1"))) {
    warning("tech1Output passed into lookupTech1Parameter does not appear to be the right data type.")
    return(NULL)
  }

  if (inherits(tech1Output, "mplus.tech1")) tech1Output <- tech1Output$parameterSpecification

  matchFound <- FALSE
  for (mat in 1:length(tech1Output)) {
    matchPos <- which(tech1Output[[mat]] == paramNumber, arr.ind=TRUE)
    if (nrow(matchPos) > 0) {
      matchFound <- TRUE
      matName <- names(tech1Output)[mat]
      matchVars <- cbind(row=rownames(tech1Output[[mat]])[matchPos[,"row"]],
          col=colnames(tech1Output[[mat]])[matchPos[,"col"]])
    }
  }

  if (matchFound) {
    cat("Matrix name:", matName, "\n\n")
    print(as.data.frame(matchVars))
    return(invisible(as.data.frame(matchVars)))
  } else {
    cat("Unable to find matching parameter in TECH1 output for parameter number:", paramNumber)
    return(invisible(NULL))
  }
}

#' Test inequality-constrained hypothesis for two parameters based on iterations of MCMC chains
#'
#' Tests a simple inequality-constrained hypothesis (van de Schoot, Hoijtink, Hallquist, & Boelen, in press) based on
#' draws from the posterior distribution of the model parameters, which provides information about the proportion
#' of the distribution that is in agreement with a given hypothesis. This function is used for simple hypothesis
#' for two parameters, whereas testBParamCompoundConstraint gives full access to multiple parameters and R's logic
#' syntax.
#' This function accepts a bparameters object containing iterations of the MCMC chains (rows) for each model parameter (columns)
#' and prints out the number and proportion of draws that are consistent with the requested hypothesis test.
#' The \code{coef1}, \code{operator}, and \code{coef2} arguments are appended in sequence, so that the hypothesis test is
#' constructed from left-to-right. e.g., \code{testBParamConstraint(bparamsDF, "MGM.TRT1", ">", "MGM.EX2")}.
#'
#' @param bparams An object containing draws from the posterior distribution (class \code{mplus.model} or \code{mplus.bparameters}).
#'   Obtained by SAVEDATA:BPARAMETERS in Mplus and \code{\link{getSavedata_Bparams}} or \code{\link{readModels}} in \code{MplusAutomation}.
#' @param coef1 The name of the first parameter to be compared. Example: \code{"MGM.TRT1"}
#' @param operator A logical operator to compare the two parameters. Should be one of \code{>=, >, <, or <=}.
#'   Example: \code{">="}
#' @param coef2 The name of the first parameter to be compared. Example: \code{"MGM.EX2"}
#' @return No value is returned by this function. Instead, two summary tables are printed to the screen containing the number and proportion
#' of draws consistent with the hypothesis.
#' @author Michael Hallquist
#' @seealso \link{testBParamCompoundConstraint}
#' @export
#' @keywords interface
#' @examples
#' \dontrun{
#'   #using bparameters directly
#'   btest <- getSavedata_Bparams("model vb1_simpel_b.out"))
#'   testBParametersConstraint(btest, "STDYX_STAITOT.ON.CG", ">", "STDYX_STAITOT.ON.UCG")
#'
#'   #or using readModels
#'   btest <- readModels("model vb1_simpel_b.out"))
#'   testBParametersConstraint(btest, "STDYX_STAITOT.ON.CG", ">", "STDYX_STAITOT.ON.UCG")
#' }
testBParamConstraint <- function(bparams, coef1, operator, coef2) {
  #allow input to be mplus.model
  if (inherits(bparams, "mplus.model")) {
    bparams <- bparams$bparameters
  }

  #restrict to valid draws (if not already dropped)
  if (length(bparams) == 2 && names(bparams) == c("burn_in", "valid_draw")) {
    bparams <- bparams$valid_draw
  }

  #combine into one data.frame
  bparams <- as.data.frame(do.call(rbind, bparams))

  cat("Number of iterations: ", nrow(bparams), "\n")
  ineq <- eval(parse(text=paste("bparams$", coef1, operator, "bparams$", coef2, sep="")))
  counts <- table(ineq)
  names(counts) <- c("Constraint not met", "Constraint met")
  proportions <- prop.table(counts)
  cat("\n---\nFrequency table of constraint test:\n\n")
  print(counts)
  cat("\n---\nProportion table of constraint test:\n\n")
  print(proportions)
}

#' Test inequality-constrained hypothesis for two or more parameters based on iterations of MCMC chains
#'
#' Tests an inequality-constrained hypothesis (van de Schoot, Hoijtink, Hallquist, & Boelen, in press) based on
#' draws from the posterior distribution of the model parameters, which provides information about the proportion
#' of the distribution that is in agreement with a given hypothesis. This function is used for more complex hypotheses
#' about three or more parameters, whereas testBParamConstraint tests a simple two-parameter hypothesis.
#'
#' This function accepts a bparameters object containing iterations of the MCMC chains (rows) for each model parameter (columns)
#' and prints out the number and proportion of draws that are consistent with the requested hypothesis test.
#'
#' The \code{test} argument is evaluated directly as \code{R} code, with the \code{bparams} object attached so that
#' variable names are available directly in the environment. Because the goal is to evaluate the test for each
#' draw from the posterior distribution, remember to use vector-based logic operators, not boolean operators. That is,
#' stick to \code{&} or \code{|} for joining tests of parameters, rather than \code{&&} or \code{||} since the latter will
#' return a single TRUE/FALSE, which is irrelevant.
#'
#' An example test in R logic would be \code{"(STAITOT.ON.CG > STAITOT.ON.UCG) & (BDIM.ON.CG > BDIM.ON.UCG)"}.
#'
#' @param bparams An object containing draws from the posterior distribution (class \code{mplus.model} or \code{mplus.bparameters}).
#'   Obtained by SAVEDATA:BPARAMETERS in Mplus and \code{\link{getSavedata_Bparams}} or \code{\link{readModels}} in \code{MplusAutomation}.
#' @param test The \code{R} code defining the parameter test of three or more parameters. Example:
#'   \code{"(STAITOT.ON.CG > STAITOT.ON.UCG) & (BDIM.ON.CG > BDIM.ON.UCG)"}.
#' @return No value is returned by this function. Instead, two summary tables are printed to the screen containing the number and proportion
#' of draws consistent with the hypothesis.
#' @author Michael Hallquist
#' @seealso \code{\link{testBParamConstraint}}
#' @export
#' @keywords interface
#' @examples
#' \dontrun{
#'   #using bparameters directly
#'   btest <- getSavedata_Bparams("model vb1_simpel_b.out")
#'   testBParametersCompoundConstraint(btest,
#'   "(STDYX_STAITOT.ON.CG > STDYX_STAITOT.ON.UCG) & (STDYX_BDIM.ON.CG > STDYX_BDIM.ON.UCG)")
#'
#'   #or using readModels
#'   btest <- readModels("model vb1_simpel_b.out")
#'   testBParametersCompoundConstraint(btest,
#'   "(STDYX_STAITOT.ON.CG > STDYX_STAITOT.ON.UCG) & (STDYX_BDIM.ON.CG > STDYX_BDIM.ON.UCG)")
#' }
testBParamCompoundConstraint <- function(bparams, test) {
  #allow input to be mplus.model
  if (inherits(bparams, "mplus.model")) {
    bparams <- bparams$bparameters
  }

  #restrict to valid draws (if not already dropped)
  if (length(bparams) == 2 && names(bparams) == c("burn_in", "valid_draw")) {
    bparams <- bparams$valid_draw
  }

  #combine into one data.frame
  bparams <- as.data.frame(do.call(rbind, bparams))

  cat("Number of iterations: ", nrow(bparams), "\n")
  testResult <- with(bparams, eval(parse(text=test)))

  counts <- table(testResult)
  names(counts) <- c("Constraint not met", "Constraint met")
  proportions <- prop.table(counts)
  cat("\n---\nFrequency table of constraint test:\n\n")
  print(counts)
  cat("\n---\nProportion table of constraint test:\n\n")
  print(proportions)
}

#' Friendly Regular Expression
#'
#' Creates data frame documenting the start and end of all tags.
#'
#' @param pattern The pattern to search for
#' @param charvector Character vector
#' @param perl A logical whether or not to use perl based
#'   regular expressions.  Defaults to \code{TRUE}.
#' @return A \code{data.frame}
#' @author Michael Hallquist
#' @keywords internal
#' @examples
#' ## make me
friendlyGregexpr <- function(pattern, charvector, perl=TRUE) {
  #require(plyr)
  #now create data frame documenting the start and end of all tags
  #rather than ldply, need a usual loop to track element number (in cases where charvector is a vector)
  regexpMatches <- gregexpr(pattern, charvector, perl=perl)

  convertMatches <- c()
  for (i in 1:length(regexpMatches)) {
    thisLine <- regexpMatches[[i]]
    #only append if there is at least one match on this line
    if (thisLine[1] != -1) {
      convertMatches <- rbind(convertMatches, data.frame(element=i, start=thisLine, end=thisLine+attr(thisLine, "match.length")-1))
    }
  }

  #if no matches exist, return null (otherwise, will break adply)
  if (is.null(convertMatches)) return(NULL)

  #okay, now we have a data frame with the line, starting position, and ending position of every tag

  #time to classify into simple, array, iterator, and conditional

  #first, add the actual tag to the data.frame to make it easier to parse
  #using adply (is this not its intended use?) to iterate over rows and apply func
  convertMatches <- adply(convertMatches, 1, function(row) {
        row$tag <- substr(charvector[row$element], row$start, row$end)
        #for some reason, adply does not respect the stringsAsFactors here
        return(as.data.frame(row, stringAsFactors=FALSE))
      })

  convertMatches$tag <- as.character(convertMatches$tag)
  return(convertMatches)
}

#expose as root-level function to be used by model summary extraction
#METHOD NOW DEPRECATED. USING KEYWORD-MATCHING APPROACH BELOW
getSection_Blanklines <- function(sectionHeader, outfiletext) {
  #helper sub-function to extract a model section given a certain header.
  #the logic here is pretty convoluted. In general, Mplus results sections end with two blank lines
  #but there are problematic exceptions, like Example 9.7. Bengt has said that this formatting error will be fixed
  #in the next edition, but I've gone ahead and implemented a more nuanced (but excessively complicated) logic.

  #the end of the model results section is demarcated by two blank lines
  #this is not a reliable marker!! See Example 9.7. Breaks down with twolevel model.

  #27Aug2010: beginSection may match multiple headers and end up with length > 1.
  #Example: STDYX Standardization appears in the standardized model results section
  #and in the total, direct, and indirect sections.
  #this results in a screwy looping sequence below that will not work.
  #solution: just use the first element (most proximal match)
  beginSection <- grep(sectionHeader, outfiletext)[1]

  #if section header cannot be found, then bail out
  if (is.na(beginSection)) return(NULL)

  endSection <- 0
  for (row in beginSection+1:length(outfiletext)) {
    #note short circuit && ensures that we will not go outside subscript bounds for outfiletext
    #check for current line and line+1 blank (two consecutive blank lines)
    if (row < length(outfiletext) && outfiletext[row] == "" && outfiletext[row+1] == "") {
      #if there are two blank lines, but no data thereafter, treat this as the end of the section
      if (length(outfiletext) == row+1) {
        endSection <- row
        break
      }

      #otherwise, double check that line following the double blank is all baps
      #given problems with example 9.7, also test that row+2 is a line of all capital letters
      #start by deleting all spaces
      capsLine <- gsub("\\s+", "", outfiletext[row+2], perl=TRUE)

      #now search for any non-capital letter (also allow for hyphens for R-SQUARE and numbers for TECHNICAL 1 OUTPUT)
      hasLowercase <- regexpr("[^0-9A-Z-]", capsLine, perl=TRUE) #will be -1 if all caps

      #actually, the caps check is breaking down for standardized output.
      #for stdyx, the stdy section is next, but begins with "STDY Standardization" (not all caps).
      #adding exception to logic below... getting kind of kludgy, but Mplus output is just not consistent.

      #if the next line is not all capitals, then continue reading output
      #even this could choke on a line like FACTOR BY, but that shouldn't happen because the logic requires two blank lines above
      if (hasLowercase < 0 || regexpr("STD[YX]*Standardization", capsLine, perl=TRUE) > 0) {
        endSection <- row
        break
      }
    }
  }

  if (!endSection > 0) stop("Could not locate results section end for header:\n  ", outfiletext[beginSection])

  modelSection <- outfiletext[(beginSection+1):(endSection-1)]

  return(modelSection)

}


#IRT PARAMETERIZATION IN TWO-PARAMETER LOGISTIC (or PROBIT) METRIC
#LOGISTIC REGRESSION ODDS RATIO RESULTS

#' Get an Output Section
#'
#' @param sectionHeader Header section
#' @param outfiletext Output file text
#' @param headers Can pass custom headers but defaults to a standard set.
#' @return Section
#' @keywords internal
#' @examples
#' # make me!!!
getSection <- function(sectionHeader, outfiletext, headers="standard") {
  #encode the top-level major headers here, but allow for custom headers to be passed in
  
  #use cached headers from initial parsing to speed up search for the appropriate section
  h <- attr(outfiletext, "headerlines")
  if (!headers[1L] == "standard") {
    #parse custom headers. form alternation pattern for regular expression (adds leading and trailing spaces permission to each header)
    headerRegexpr <- paste("(", paste(gsub("(.*)", "^\\\\s*\\1\\\\s*$", headers, perl=TRUE), sep="", collapse="|"), ")", sep="")
    h <- grep(headerRegexpr, outfiletext, perl=TRUE)
  } else if (is.null(h)) {
    stop("Standard headers not parsed")
  }
  
  #allow for syntax to include :: to specify a header that spans 2 rows. Example:
  #FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES
  #BASED ON THE ESTIMATED MODEL
  
  #note that this will identify a unique match for the target sectionHeader, but the search for
  #subsequent headers just uses the one-row list above. For now, this works in all cases I know of.
  if (grepl("::", sectionHeader, fixed=TRUE)) {
    firstLine <- sub("^(.*)::.*$", "\\1", sectionHeader, perl=TRUE)
    nextLine <- sub("^.*::(.*)$", "\\1", sectionHeader, perl=TRUE)
    candidates <- grep(firstLine, outfiletext[h], perl=TRUE) #will return NA if no match
    bothMatch <- grep(nextLine, outfiletext[ h[candidates]+1 ], perl=TRUE)[1] #return first match among candidates
    if (!is.na(bothMatch)) { beginSection <- h[candidates[bothMatch]] + 1 #since it's a two-line header, skip the first to match typical case
    } else { beginSection <- NA } #could not find section with both rows
  } else {
    beginSection <- h[ grep(sectionHeader, outfiletext[h], perl=TRUE)[1] ]
  }
  
  #if section header cannot be found, then bail out
  if (is.na(beginSection)) return(NULL)
  
  #identify headers after this section to find end of section
  subsequentHeaders <- which(h > beginSection)

  if (length(subsequentHeaders) == 0) { nextHeader <- length(outfiletext) #just return the whole enchilada
  } else { nextHeader <- h[subsequentHeaders[1]] - 1 }
  
  section.found <- outfiletext[(beginSection+1):nextHeader]
  attr(section.found, "lines") <- beginSection:nextHeader
  
  return(section.found)  
}

#master function to parse text into sections
parse_into_sections <- function(outfiletext) {
  headers <- c("INPUT INSTRUCTIONS", "SUMMARY OF ANALYSIS",
      "SUMMARY OF DATA FOR THE FIRST DATA SET", "SUMMARY OF DATA FOR THE FIRST REPLICATION",
      "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST REPLICATION",
      "SUMMARY OF MISSING DATA PATTERNS",
      "COVARIANCE COVERAGE OF DATA FOR THE FIRST REPLICATION",
      "COVARIANCE COVERAGE OF DATA", "UNIVARIATE SAMPLE STATISTICS",
      "THE MODEL ESTIMATION TERMINATED NORMALLY",
      "SAMPLE STATISTICS", "SAMPLE STATISTICS FOR THE FIRST REPLICATION",
      "RESULTS FOR BASIC ANALYSIS",
      "CROSSTABS FOR CATEGORICAL VARIABLES", "UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES",
      "SUMMARY OF CENSORED LIMITS", "COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES",
      "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES",
      "TESTS OF MODEL FIT", "MODEL FIT INFORMATION", "MODEL FIT INFORMATION FOR .*", "CLASSIFICATION QUALITY",
      "SUMMARY OF MODEL FIT INFORMATION", "RESULTS FOR EXPLORATORY FACTOR ANALYSIS",
      "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES",
      "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS",
      "LATENT TRANSITION PROBABILITIES BASED ON THE ESTIMATED MODEL",
      "FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE",
      "CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP",
      "Average Latent Class Probabilities for Most Likely Latent Class Membership \\(Row\\)",
      "Classification Probabilities for the Most Likely Latent Class Membership \\(Row\\)",
      "Classification Probabilities for the Most Likely Latent Class Membership \\(Column\\)",
      "Logits for the Classification Probabilities for the Most Likely Latent Class Membership \\(Row\\)",
      "Logits for the Classification Probabilities for the Most Likely Latent Class Membership \\(Column\\)",
      "MODEL RESULTS", "MODEL RESULTS FOR .*", "LOGISTIC REGRESSION ODDS RATIO RESULTS", "RESULTS IN PROBABILITY SCALE",
      "IRT PARAMETERIZATION IN TWO-PARAMETER LOGISTIC METRIC",
      "IRT PARAMETERIZATION IN TWO-PARAMETER PROBIT METRIC",
      "IRT PARAMETERIZATION",
      "BRANT WALD TEST FOR PROPORTIONAL ODDS",
      "BETWEEN-LEVEL FACTOR SCORE COMPARISONS",
      "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION",
      "LATENT CLASS ODDS RATIO RESULTS", "LOGRANK OUTPUT", "STANDARDIZED MODEL RESULTS",
      "WITHIN-LEVEL STANDARDIZED MODEL RESULTS FOR CLUSTER \\d+",
      "R-SQUARE", "QUALITY OF NUMERICAL RESULTS", "QUALITY OF NUMERICAL RESULTS FOR .*", "TECHNICAL OUTPUT", "TECHNICAL \\d+ OUTPUT",
      "TECHNICAL \\d+ OUTPUT FOR .*", "TECHNICAL 5/6 OUTPUT",
      "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
      "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS FOR LATENT RESPONSE VARIABLES",
      "TOTAL, INDIRECT, AND DIRECT EFFECTS BASED ON COUNTERFACTUALS \\(CAUSALLY-DEFINED EFFECTS\\)",
      "STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", "CONFIDENCE INTERVALS OF MODEL RESULTS",
      "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS",
      "CREDIBILITY INTERVALS OF MODEL RESULTS",
      "CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS",
      "CREDIBILITY INTERVALS OF STANDARDIZED MODEL RESULTS",
      "CONFIDENCE INTERVALS IN PROBABILITY SCALE",
      "CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
      "CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT,", #omitted "AND DIRECT EFFECTS" in v7
      "CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", #fit onto 1 line in v8!
      "EQUALITY TESTS OF MEANS ACROSS CLASSES USING POSTERIOR PROBABILITY-BASED",
      "EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE BCH PROCEDURE",
      "EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE 3-STEP PROCEDURE",
      "EQUALITY TESTS OF MEANS/PROBABILITIES ACROSS CLASSES",
      "THE FOLLOWING DATA SET\\(S\\) DID NOT RESULT IN A COMPLETED REPLICATION:",
      "RESIDUAL OUTPUT", "MODEL MODIFICATION INDICES", "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES",
      "SUMMARIES OF PLAUSIBLE VALUES \\(N = NUMBER OF OBSERVATIONS * NUMBER OF IMPUTATIONS\\)",
      "SUMMARY OF PLAUSIBLE STANDARD DEVIATION \\(N = NUMBER OF OBSERVATIONS\\)",
      "Available post-processing tools:",
      "FACTOR SCORE INFORMATION \\(COMPLETE DATA\\)", "SUMMARY OF FACTOR SCORES", "PLOT INFORMATION", "SAVEDATA INFORMATION",
      "RESULTS SAVING INFORMATION", "SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES", "DIAGRAM INFORMATION",
      "Beginning Time:\\s*\\d+:\\d+:\\d+", "MUTHEN & MUTHEN"
  )
  
  #form alternation pattern for regular expression (currently adds leading and trailing spaces permission to each header)
  headerRegexpr <- paste("(", paste(gsub("(.*)", "^\\\\s*\\1\\\\s*$", headers, perl=TRUE), sep="", collapse="|"), ")", sep="")
  headerLines <- grep(headerRegexpr, outfiletext, perl=TRUE)
  
  attr(outfiletext, "headerlines") <- headerLines
  return(outfiletext)
}

#' Extract a multiline section from Mplus output
#'
#' New approach to multiline section: retain spaces and look for next line that has identical indentation.
#'
#' @param header Header section
#' @param outfiletext Output file text
#' @param filename The name of the file
#' @param allowMultiple Logical indicating whether to allow multiple sections. Defaults to \code{FALSE}.
#' @param allowSpace Logical indicating whether to allow spaces. Defaults to \code{TRUE}.
#' @param ignore.case Logical whether or not to ignore the case.  Defaults to \code{FALSE}.
#' @return A list of sections
#' @keywords internal
#' @examples
#' # make me!!!
getMultilineSection <- function(header, outfiletext, filename, allowMultiple=FALSE, allowSpace=TRUE, ignore.case=FALSE) {
  # TODO: May2017: update this function to return an empty list in the case of match failure instead of NA_character_.
  #                Will also need to update behavior of all calls accordingly
  # Apr2015: Need greater flexibility in how a section is defined. For certain sections, indentation is unhelpful. Example:

  # Chi-Square Test of Model Fit for the Binary and Ordered Categorical
  # (Ordinal) Outcomes
  #
  # Pearson Chi-Square
  #
  # Value                             13.286
  # Degrees of Freedom                     9
  # P-Value                           0.1501
  #
  # Likelihood Ratio Chi-Square
  #
  # Value                             16.731
  # Degrees of Freedom                     9
  # P-Value                           0.0531

  # Likewise, in the above example there is a second line to the header that should be skipped before developing the section
  #
  # New syntax:
  # {+2i}Chi-Square Test of Model Fit for the Binary and Ordered Categorical::{+1b}Pearson Chi-Square
  #
  # +X specifies how many lines (after the header line itself) should be skipped prior to searching for the section end
  # i,b specifies whether to use identical indentation {i} (which has been the standard up to now) or to use a blank line {b} to identify the section end
  # If no curly braces are provided, assume {+1i}

  #allow for multiple depths (subsections) separated by ::
  #will just extract from deepest depth
  header <- strsplit(header, "::", fixed=TRUE)[[1]]
  
  sectionList <- list()
  targetText <- outfiletext
  for (level in 1:length(header)) {
    if ((searchCmd <- regexpr("^\\{(\\+\\d+)*([ib])*\\}", header[level], perl=TRUE)) > 0) {
      if ((o_start <- attr(searchCmd, "capture.start")[1]) > 0) {
        offset <- substr(header[level], o_start, o_start + attr(searchCmd, "capture.length")[1] - 1)
        offset <- as.integer(sub("+", "", offset, fixed=TRUE)) #remove + sign
      } else {
        offset <- 1
      }

      if ((s_start <- attr(searchCmd, "capture.start")[2]) > 0) {
        stype <- substr(header[level], s_start, s_start + attr(searchCmd, "capture.length")[2] - 1)
        stopifnot(nchar(stype) == 1 && stype %in% c("i", "b"))
      } else {
        stype <- "i"
      }

      #remove search type information from header
      header[level] <- substr(header[level], searchCmd[1] + attr(searchCmd, "match.length"), nchar(header[level]))
    } else {
      offset <- 1
      stype <- "i"
    }

    if (allowSpace==TRUE) headerRow <- grep(paste("^\\s*", header[level], "\\s*$", sep=""), targetText, perl=TRUE, ignore.case=ignore.case)
    else headerRow <- grep(paste("^", header[level], "$", sep=""), targetText, perl=TRUE, ignore.case=ignore.case) #useful for equality of means where we just want anything with 0 spaces

    if (length(headerRow) == 1L || (length(headerRow) > 0L && allowMultiple==TRUE)) {
      for (r in 1:length(headerRow)) {
        #locate the position of the first non-space character
        numSpacesHeader <- regexpr("\\S+.*$", targetText[headerRow[r]], perl=TRUE) - 1

        sectionStart <- headerRow[r] + offset #skip header row itself

        if (stype == "i") {
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
            } else if (readStart+19 >= length(targetText)) {
              sameLevelMatch <- TRUE
              sectionEnd <- length(targetText)
            } else { readStart <- readStart + 20 } #process next batch

            #if (readStart > 100000) browser()#stop ("readStart exceeded 100000. Must be formatting problem.")
          }
        } else if (stype == "b") {
          blankFound <- FALSE
          i <- 0
          while(!grepl("^\\s*$", targetText[sectionStart+i], perl=T) && i <= length(targetText) - sectionStart) { #ensure that i doesn't go beyond length of section
            i <- i + 1
            if (i > 10000) { stop("searched for next blank line on 10000 rows without success.") }
          }
          if (i == 0) {
            #first line of section was blank, so just set start and end to same
            #could force search to look beyond first line since it would be rare that a blank line after header match should count as empty
            sectionEnd <- sectionStart
          } else {
            sectionEnd <- sectionStart + i - 1 #line prior to blank
          }

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

    } else {
      targetText <- NA_character_
      if (length(headerRow) > 1L) warning(paste("Multiple matches for header: ", header, "\n  ", filename, sep=""))
      break
      #else if (length(headerRow) < 1) warning(paste("Could not locate section based on header: ", header, "\n  ", filename, sep=""))
    }

  }

  if (length(sectionList) > 0L && allowMultiple) {
    attr(sectionList, "matchlines") <- headerRow
    return(sectionList)
  } else { return(targetText) }
}


#' Parse Categorical Output
#'
#' Helper function for parsing output with variables and categories.
#'
#' @param text The output to parse.
#' @return The parsed output
#' @author Michael Hallquist
#' @export
#' @keywords interface
#' @examples
#' "
#' Example:
#' UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES
#'
#' SOP2A
#'   Category 1    0.254      631.000
#'   Category 2    0.425     1056.000
#'   Category 3    0.174      432.000
#'   Category 4    0.147      365.000
#'
#' Or Item Categories in IRT Parameterization
#'
#' Item Categories
#'  U1
#'    Category 1         0.000      0.000      0.000      1.000
#'    Category 2        -0.247      0.045     -5.534      0.000
#'    Category 3         0.699      0.052     13.325      0.000
#'    Category 4        -0.743      0.057    -12.938      0.000
#'    Category 5         0.291      0.052      5.551      0.000
#' "
parseCatOutput <- function(text) {
  hlines <- grep("^\\s*([\\w_\\d+\\.#\\&]+)\\s*$", text, perl=TRUE)
  if (any(grepl("Category", text[hlines]))) {
    stop("Failed to parse categorical output")
  }

  reformat <- c()
  for (vv in 1:length(hlines)) {
    vname <- text[hlines[vv]]
    startLine <- hlines[vv]+1
    endLine <- ifelse(hlines[vv] < max(hlines), hlines[vv+1]-1, length(text))
    reformat <- c(reformat, sub("Category (\\d+)", paste0(vname, ".Cat.\\1"), text[startLine:endLine]))
  }
  return(reformat)
}


#' Get Output File List
#'
#' This is a helper function used by extractModelSummaries and extractModelParameters.
#' It determines whether the target is a single file or a directory.
#' If it is a directory, all .out files are returned (perhaps recursively)
#' It also permits the files to be filtered using a certain regular expression.
#'
#' @param target The target file or directory
#' @param recursive A logical value whether to search recursively.
#'   Defaults to \code{FALSE}.
#' @param filefilter A regular expression passed to \code{grep}
#'   used to filter the output files.
#' @return A character vector of the output files
#' @keywords internal
#' @examples
#' # make me!!!
getOutFileList <- function(target, recursive=FALSE, filefilter) {
#could this also be used by runModels to locate input files?
#seems like that function would do well to allow for directories and single files, too.

  #determine whether target is a file or a directory
  if (file.exists(target)) {
    if (file.info(target)$isdir == TRUE) {

      #obtain list of all files in the specified directory
      filelist <- list.files(path=target, recursive=recursive, full.names=TRUE)

      #retain only .out files
      outfiles <- filelist[grep(".*\\.out$", filelist, ignore.case=TRUE)]

      if (!missing(filefilter)) {
        dropOutExtensions <- sapply(outfiles, function(x) {
              if (nchar(x) >= 4) return(tolower(substr(x, 1, (nchar(x)-4))))
            })
        outfiles <- outfiles[grep(paste(".*", filefilter, ".*", sep=""), dropOutExtensions, ignore.case=TRUE, perl=TRUE)]
      }
    }
    else {
      #ensure that target is a single output file.
      if (nchar(target) >= 4 && !substr(target, nchar(target) - 3, nchar(target)) == ".out") stop("Specified target is not an output file.\n  Target:", target)

      #outfiles collection is just one file
      outfiles <- target
    }
  }
  else stop("Specified target does not exist.\n  Target: ", target)

  if (length(outfiles) == 0) {
    warning("No output files detected in this directory.")
    return(NULL)
  }

  return(outfiles)
}

#' Split File and Path into Separate Parts
#'
#' This is a helper function to split path into path and filename.
#' Code adapted from R.utils filePath command.
#'
#' @param abspath A character string of the file path
#' @return A list with elements for the directory, filename,
#'   and absolute path.
#' @keywords internal
#' @examples
#' # make me!!!
splitFilePath <- function(abspath) {
  if (!is.character(abspath)) stop("Path not a character string")
  if (nchar(abspath) < 1 || is.na(abspath)) stop("Path is missing or of zero length")

  #trailing slash screws up file.exists call on Windows: https://bugs.r-project.org/bugzilla/show_bug.cgi?id=14721
  abspath <- sub("(\\\\|/)?$", "", abspath, perl=TRUE)

  components <- strsplit(abspath, split="[\\/]")[[1]]
  lcom <- length(components)

  stopifnot(lcom > 0)

  #the file is the last element in the list. In the case of length == 1, this will extract the only element.
  relFilename <- components[lcom]
  absolute <- FALSE

  if (lcom == 1) {
    dirpart <- NA_character_
  }
  else if (lcom > 1) {
    #drop the file from the list (the last element)
    components <- components[-lcom]
    dirpart <- do.call("file.path", as.list(components))

    #if path begins with C:, /, ~/, //, or \\, then treat as absolute
    if (grepl("^([A-Z]{1}:|~/|/|//|\\\\)+.*$", dirpart, perl=TRUE)) absolute <- TRUE
  }

  return(list(directory=dirpart, filename=relFilename, absolute=absolute))
}


#' Detect Column Names
#'
#' Helper function to detect model results columns.
#'
#' @param filename The file name
#' @param modelSection The model section
#' @param sectionType A character string.  Defaults to \dQuote{model_results}.
#' @return A list with elements for the directory, filename,
#'   and absolute path.
#' @keywords internal
#' @examples
#' # make me!!!
detectColumnNames <- function(filename, modelSection, sectionType="model_results") {

  detectionFinished <- FALSE
  line <- 1
  while(detectionFinished == FALSE) {
    thisLine <- strsplit(modelSection[line], "\\s+", perl=TRUE)[[1]] #assumes that lines are trimmed of leading/trailing whitespace
    if (line < length(modelSection)) nextLine <- strsplit(modelSection[line+1], "\\s+", perl=TRUE)[[1]]
    else nextLine <- NA_character_

    if (sectionType == "model_results") {

      #detect common Mplus output formats
      #not especially flexible code, but hard to perfect it when names span two lines and headers have changed over versions
      #Would be ideal to build element-by-element, but not feasible given ambiguity across versions and two-line headers

      #Bayesian (ESTIMATOR=BAYES) 6-column output
      if (identical(thisLine, c("Posterior", "One-Tailed", "95%", "C.I.")) &&
          identical (nextLine, c("Estimate", "S.D.", "P-Value", "Lower", "2.5%", "Upper", "2.5%")))
        varNames <- c("param", "est", "posterior_sd", "pval", "lower_2.5ci", "upper_2.5ci")

      #Bayesian 6-column output for R-SQUARE (Just has "Variable" on the front)
      if (identical(thisLine, c("Posterior", "One-Tailed", "95%", "C.I.")) &&
          identical (nextLine, c("Variable", "Estimate", "S.D.", "P-Value", "Lower", "2.5%", "Upper", "2.5%")))
        varNames <- c("param", "est", "posterior_sd", "pval", "lower_2.5ci", "upper_2.5ci")

      #Bayesian (ESTIMATOR=BAYES) 7-column output (Mplus v7)
      else if (identical(thisLine, c("Posterior", "One-Tailed", "95%", "C.I.")) &&
          identical (nextLine, c("Estimate", "S.D.", "P-Value", "Lower", "2.5%", "Upper", "2.5%", "Significance")))
        varNames <- c("param", "est", "posterior_sd", "pval", "lower_2.5ci", "upper_2.5ci", "sig")

      #Monte Carlo output (e.g., UG ex12.4)
      else if (identical(thisLine, c("ESTIMATES", "S.", "E.", "M.", "S.", "E.", "95%", "%", "Sig")) &&
          identical (nextLine, c("Population", "Average", "Std.", "Dev.", "Average", "Cover", "Coeff")))
        varNames <- c("param", "population", "average", "population_sd", "average_se", "mse", "cover_95", "pct_sig_coef")

      #Multiple imputation output (I think format introduced in v7)
      else if (identical(thisLine, c("Two-Tailed", "Rate", "of")) &&
          identical(nextLine, c("Estimate", "S.E.", "Est./S.E.", "P-Value", "Missing")))
        varNames <- c("param", "est", "se", "est_se", "pval", "rate_missing")

      #Multiple imputation output for R2 including rate of missing (introduced in v8, I think)
      else if (identical(thisLine, c("Observed", "Two-Tailed", "Rate", "of")) &&
          identical(nextLine, c("Variable", "Estimate", "S.E.", "Est./S.E.", "P-Value", "Missing")))
        varNames <- c("param", "est", "se", "est_se", "pval", "rate_missing")
            
      #Usual five-column output that applies to most unstandardized and standardized sections in Mplus 5 and later
      else if (identical(thisLine, c("Two-Tailed")) &&
          identical(nextLine, c("Estimate", "S.E.", "Est./S.E.", "P-Value")))
        varNames <- c("param", "est", "se", "est_se", "pval")

      #Five-column output for R-Square that applies to most unstandardized and standardized sections in Mplus 5 and later
      else if ((identical(thisLine, c("Observed", "Two-Tailed")) || identical(thisLine, c("Latent", "Two-Tailed"))) &&
          identical(nextLine, c("Variable", "Estimate", "S.E.", "Est./S.E.", "P-Value")))
        varNames <- c("param", "est", "se", "est_se", "pval")

      #6-column R-Square output for model with scale factors
      else if ((identical(thisLine, c("Observed", "Two-Tailed", "Scale")) || identical(thisLine, c("Latent", "Two-Tailed", "Scale"))) &&
          identical(nextLine, c("Variable", "Estimate", "S.E.", "Est./S.E.", "P-Value", "Factors")))
        varNames <- c("param", "est", "se", "est_se", "pval", "scale_f")

      #6-column R-Square output for model with residual variances
      else if ((identical(thisLine, c("Observed", "Two-Tailed", "Residual")) || identical(thisLine, c("Latent", "Two-Tailed", "Residual"))) &&
          identical(nextLine, c("Variable", "Estimate", "S.E.", "Est./S.E.", "P-Value", "Variance")))
        varNames <- c("param", "est", "se", "est_se", "pval", "resid_var")

      #2-column R-Square without estimates of uncertainty and p-values
      else if ((identical(thisLine, c("Observed")) || identical(thisLine, c("Latent"))) &&
          identical(nextLine, c("Variable", "Estimate")))
        varNames <- c("param", "est")

      #3-column R-Square output for model with residual variances
      else if ((identical(thisLine, c("Observed", "Scale")) || identical(thisLine, c("Latent", "Scale"))) &&
          identical(nextLine, c("Variable", "Estimate", "Factors")))
        varNames <- c("param", "est", "scale_f")

      #3-column R-Square output for model with scale factors (WLSMV)
      else if ((identical(thisLine, c("Observed", "Residual")) || identical(thisLine, c("Latent", "Residual"))) &&
          identical(nextLine, c("Variable", "Estimate", "Variance")))
        varNames <- c("param", "est", "resid_var")


      #Just estimate available, such as in cases of nonconverged models
      else if (identical(thisLine, c("Estimate")))
        varNames <- c("param", "est")

      #Old 5-column standardized output from Mplus 4.2
      else if (identical(thisLine, c("Estimates", "S.E.", "Est./S.E.", "Std", "StdYX")))
        #in cases where combined raw and std, should split out results into list form
        varNames <- c("param", "est", "se", "est_se", "std", "stdyx")

      #Old 3-column output from Mplus 4.2
      else if (identical(thisLine, c("Estimates", "S.E.", "Est./S.E.")))
        #in cases where combined raw and std, should split out results into list form
        varNames <- c("param", "est", "se", "est_se")

      #MUML estimator or WLS estimators with covariates do not allow std. errors or StdY for standardized output
      #run 9.1b with MUML and OUTPUT:STANDARDIZED
      else if (identical(thisLine, c("StdYX", "Std")) && identical (nextLine, c("Estimate", "Estimate")))
        varNames <- c("param", "stdyx", "std")

      #if user specifically requests just stdyx STANDARDIZED(STDYX);
      else if (identical(thisLine, c("StdYX")) && identical (nextLine, c("Estimate")))
        varNames <- c("param", "stdyx")

      #if user specifically requests just stdy STANDARDIZED(STDY);
      else if (identical(thisLine, c("StdY")) && identical (nextLine, c("Estimate")))
        varNames <- c("param", "stdy")

      #if user specifically requests just std STANDARDIZED(STD);
      else if (identical(thisLine, c("Std")) && identical (nextLine, c("Estimate")))
        varNames <- c("param", "std")

      #Also, even with new versions of Mplus (e.g., 6.11), sometimes have stdyx, stdy, and std in old-style column format
      #The current case I'm aware of is the use of bootstrapped confidence intervals (BOOTSTRAP + OUTPUT:CINTERVAL).
      else if (identical(thisLine, c("StdYX", "StdY", "Std")) && identical (nextLine, c("Estimate", "Estimate", "Estimate")))
        varNames <- c("param", "stdyx", "stdy", "std")

    }
    else if (sectionType == "mod_indices") {
      if (identical(thisLine, c("M.I.", "E.P.C.", "Std", "E.P.C.", "StdYX", "E.P.C."))) {
        varNames <- c("modV1", "operator", "modV2", "MI", "EPC", "Std_EPC", "StdYX_EPC")
      } else if (identical(thisLine, c("M.I.", "E.P.C."))) {
        varNames <- c("modV1", "operator", "modV2", "MI", "EPC") }

    }
    else if (sectionType == "confidence_intervals"){
      if (identical(thisLine, c("Lower",".5%","Lower","2.5%","Lower","5%",
              "Estimate","Upper","5%","Upper","2.5%","Upper",".5%" ))) {
        varNames <- c("param", "low.5", "low2.5", "low5", "est", "up5", "up2.5", "up.5")
      } else if (identical(thisLine, c("Lower",".5%","Lower","2.5%",
              "Estimate","Upper","2.5%","Upper",".5%" ))) {
        varNames <- c("param", "low.5", "low2.5", "est", "up2.5", "up.5")
      }
    }
    else if (sectionType == "auxe") { #currently unused
      if (identical(thisLine, c("Mean", "S.E.", "Mean", "S.E."))) {
        varNames <- c("Mean", "SE", "Mean", "SE")
      } else if (identical(thisLine, c("Mean", "S.E."))) {
        varNames <- c("Mean", "SE") }
    }

    line <- line + 1
    if (exists("varNames"))
      detectionFinished <- TRUE
    else if (line > length(modelSection)) {
      warning("Unable to determine column names for section ", sectionType, ".\n  ", filename)
      return(NULL)
    }
  }

  return(varNames)
}

#' Trim White Space
#'
#' Helper function to remove white space from a character vector
#'
#' @param string The character vector to trim white space from.
#' @return A character vector with the white space removed.
#' @keywords internal
#' @examples
#' MplusAutomation:::trimSpace(c("    test", "another    "))
trimSpace <- function(string) {
  stringTrim <- sapply(string, function(x) {
        x <- sub("^\\s*", "", x, perl=TRUE)
        x <- sub("\\s*$","", x, perl=TRUE)
        return(x)
      }, USE.NAMES=FALSE)
  return(stringTrim)
}

#' Convert Mplus Number to Numeric
#'
#' Helper function to convert strings formatted in Mplus
#' Fortran-style scientific notation using D to indicate double.
#'
#' @param vec A character vector of Mplus numbers
#'   to convert to numeric
#' @return A numeric vector
#' @keywords internal
#' @examples
#' MplusAutomation:::mplus_as.numeric("3.1D2")
mplus_as.numeric <- function(vec) {
  vec <- sub("D", "E", vec, fixed=TRUE)
  as.numeric(vec)
}

#' Separate Hyphenated Variable Strings
#'
#' This code is a simplified form of \code{expandCmd} from the \pkg{lavaan} package.
#' It separates hyphenated variable strings into a list of vectors, while ignoring
#' hyphens that may be used in numbers.
#'
#' Note that this is an internal function only.
#'
#' @param cmd A character string
#' @return The character string if no hyphens, or a list of vectors if there are hyphens.
#' @author Michael Hallquist revised by Joshua Wiley
#' @keywords interface
#' @examples
#'
#' MplusAutomation:::separateHyphens("x1x4")
#' MplusAutomation:::separateHyphens("x1-x4")
#' MplusAutomation:::separateHyphens("x1-x4; x1*-1; v1-v3;")
separateHyphens <- function(cmd) {
  hyphens <- gregexpr(
    "(?!<(\\*|\\.))\\w+(?!(\\*|\\.))\\s*-\\s*(?!<(\\*|\\.))\\w+(?!(\\*|\\.))",
    cmd, perl=TRUE)[[1]]
  if (hyphens[1L] > 0) {
    lapply(seq_along(hyphens), function(v) {
      #match one keyword before and after hyphen
      strsplit(substr(cmd, hyphens[v], hyphens[v] + attr(hyphens, "match.length")[v] - 1),
               "\\s*-\\s*", perl=TRUE)[[1]][1:2]
    }) ## return variables separated by hyphens
  } else {
    return(cmd) ## no hyphens to expand
  }
}
