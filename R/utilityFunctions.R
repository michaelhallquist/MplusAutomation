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
  }
  else
    cat("Unable to find matching parameter in TECH1 output for parameter number:", paramNumber)

  return(as.data.frame(matchVars))

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


getSection <- function(sectionHeader, outfiletext, headers="standard", omit=NULL) {
  #encode the top-level major headers here, but allow for custom headers to be passed in
  #omit allows for one or more strings from headers not to be considered
  #just used for factor score statistics at the moment (these include a SAMPLE STATISTICS section)
  if (headers[1L] == "standard") headers <- c("INPUT INSTRUCTIONS", "SUMMARY OF ANALYSIS",
        "SUMMARY OF DATA FOR THE FIRST DATA SET", "SUMMARY OF DATA FOR THE FIRST REPLICATION",
        "SUMMARY OF MISSING DATA PATTERNS FOR THE FIRST REPLICATION",
        "SUMMARY OF MISSING DATA PATTERNS",
        "COVARIANCE COVERAGE OF DATA FOR THE FIRST REPLICATION",
        "SAMPLE STATISTICS", "SAMPLE STATISTICS FOR THE FIRST REPLICATION",
        "CROSSTABS FOR CATEGORICAL VARIABLES", "UNIVARIATE PROPORTIONS AND COUNTS FOR CATEGORICAL VARIABLES",
        "RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST LOGLIKELIHOOD VALUES",
        "TESTS OF MODEL FIT", "MODEL FIT INFORMATION", "CLASSIFICATION QUALITY",
        "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES",
        "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASS PATTERNS",
        "LATENT TRANSITION PROBABILITIES BASED ON THE ESTIMATED MODEL",
        "FINAL CLASS COUNTS AND PROPORTIONS FOR EACH LATENT CLASS VARIABLE",
        "CLASSIFICATION OF INDIVIDUALS BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP",
        "Average Latent Class Probabilities for Most Likely Latent Class Membership \\(Row\\)",
        "MODEL RESULTS", "LOGISTIC REGRESSION ODDS RATIO RESULTS", "RESULTS IN PROBABILITY SCALE",
        "IRT PARAMETERIZATION IN TWO-PARAMETER LOGISTIC METRIC",
        "IRT PARAMETERIZATION IN TWO-PARAMETER PROBIT METRIC",
        "ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION",
        "LATENT CLASS ODDS RATIO RESULTS", "LOGRANK OUTPUT", "STANDARDIZED MODEL RESULTS",
        "R-SQUARE", "QUALITY OF NUMERICAL RESULTS", "TECHNICAL OUTPUT", "TECHNICAL \\d+ OUTPUT",
        "TECHNICAL 5/6 OUTPUT",
        "TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
        "STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS", "CONFIDENCE INTERVALS OF MODEL RESULTS",
        "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS",
        "CREDIBILITY INTERVALS OF MODEL RESULTS",
        "CONFIDENCE INTERVALS OF STANDARDIZED MODEL RESULTS",
        "CREDIBILITY INTERVALS OF STANDARDIZED MODEL RESULTS",
        "CONFIDENCE INTERVALS IN PROBABILITY SCALE",
        "CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS",
        "CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT,", #omitted "AND DIRECT EFFECTS"
        "EQUALITY TESTS OF MEANS",
        "THE FOLLOWING DATA SET\\(S\\) DID NOT RESULT IN A COMPLETED REPLICATION:",
        "RESIDUAL OUTPUT", "MODEL MODIFICATION INDICES", "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES",
        "Available post-processing tools:",
        "FACTOR SCORE INFORMATION \\(COMPLETE DATA\\)", "SUMMARY OF FACTOR SCORES", "PLOT INFORMATION", "SAVEDATA INFORMATION",
        "RESULTS SAVING INFORMATION", "SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES", "DIAGRAM INFORMATION",
        "Beginning Time:\\s*\\d+:\\d+:\\d+", "MUTHEN & MUTHEN"
    )
  
  if (!is.null(omit)) headers <- headers[which(!headers %in% omit)] #drop omit
  
  beginSection <- grep(sectionHeader, outfiletext, perl=TRUE)[1]
  
  #if section header cannot be found, then bail out
  if (is.na(beginSection)) return(NULL)
  
  #form alternation pattern for regular expression (currently adds leading and trailing spaces permission to each header)
  headerRegexpr <- paste("(", paste(gsub("(.*)", "^\\\\s*\\1\\\\s*$", headers, perl=TRUE), sep="", collapse="|"), ")", sep="")
  headerLines <- grep(headerRegexpr, outfiletext, perl=TRUE)
  subsequentHeaders <- which(headerLines > beginSection)
  
  if (length(subsequentHeaders) == 0) nextHeader <- length(outfiletext) #just return the whole enchilada
  else nextHeader <- headerLines[subsequentHeaders[1]] - 1
  
  section.found <- outfiletext[(beginSection+1):nextHeader]
  attr(section.found, "lines") <- beginSection:nextHeader
  
  return(section.found)
  
}

#could this also be used by runModels to locate input files?
#seems like that function would do well to allow for directories and single files, too.
getOutFileList <- function(target, recursive=FALSE, filefilter) {
  #This is a helper function used by extractModelSummaries and extractModelParameters.
  #It determines whether the target is a single file or a directory.
  #If it is a directory, all .out files are returned (perhaps recursively)
  #It also permits the files to be filtered using a certain regular expression.

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

#helper function
splitFilePath <- function(abspath) {
  #function to split path into path and filename
  #code adapted from R.utils filePath command
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


#helper function to detect model results columns
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

      #Usual five-column output that applies to most unstandardized and standardized sections in Mplus 5 and later
      else if (identical(thisLine, c("Two-Tailed")) &&
          identical(nextLine, c("Estimate", "S.E.", "Est./S.E.", "P-Value")))
        varNames <- c("param", "est", "se", "est_se", "pval")

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
                        "Estimate","Upper","5%","Upper","2.5%","Upper",".5%" )))
        varNames <- c("param", "low.5", "low2.5", "low5", "est", "up5", "up2.5", "up.5")
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
    else if (line > length(modelSection))
      stop("Unable to determine column names for section ", sectionType, ".\n  ", filename)

  }

  return(varNames)

}

trimSpace <- function(string) {
  stringTrim <- sapply(string, function(x) {
        x <- sub("^\\s*", "", x, perl=TRUE)
        x <- sub("\\s*$","", x, perl=TRUE)
        return(x)
      }, USE.NAMES=FALSE)
  return(stringTrim)
}
