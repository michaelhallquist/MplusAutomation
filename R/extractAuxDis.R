#' Extract Auxiliary condition means and comparisons.
#'
#' To do: add details.
#'
#' @param outfiletext character vector of Mplus output file from which to extract the AUX section
#' @param filename filename of the Mplus output file being processed
#' @return A data frame
#' @importFrom stats reshape
#' @keywords internal
extractAux <- function(outfiletext, filename) {
  if (isEmpty(outfiletext)) stop("Missing mean equality to parse.\n ", filename)

  ppSection <- getSection("EQUALITY TESTS OF MEANS ACROSS CLASSES USING POSTERIOR PROBABILITY-BASED", outfiletext)
  bchSection <- getSection("EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE BCH PROCEDURE", outfiletext)
  step3Section <- getSection("EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE 3-STEP PROCEDURE", outfiletext)
  dconSection <- getSection("EQUALITY TESTS OF MEANS/PROBABILITIES ACROSS CLASSES", outfiletext)
  
  step3 <- bch <- dcon <- FALSE
  if (!is.null(step3Section)) {
    sectionToParse <- step3Section
    step3 <- TRUE
  } else if (!is.null(bchSection)) {
    sectionToParse <- bchSection
    bch <- TRUE
  } else if (!is.null(dconSection)) {
    dcon <- TRUE
    sectionToParse <- dconSection
  } else if (!is.null(ppSection)) {
    sectionToParse <- ppSection
  } else {
    return(NULL) #model does not contain mean equality check
  }
  
  if (dcon) {
    sectionToParse <- sectionToParse[2:length(sectionToParse)]
    chip_pattern <- "Chi-Square\\s+P-Value\\s+Degrees of Freedom"
  } else if (bch || step3) {    
    #appears this section does not have a pairwise df listed but is otherwise similar
    dfOmnibus <- as.numeric(sub("^.*WITH (\\d+) DEGREE\\(S\\) OF FREEDOM FOR THE OVERALL TEST.*$", "\\1", sectionToParse[1], perl=TRUE))
    dfPairwise <- NA_integer_
    sectionToParse <- sectionToParse[3:length(sectionToParse)]
    chip_pattern <- "Chi-Square\\s+P-Value"
  } else {
    #check for whether there is one or two more trailing lines
    twoGroupsOnly <- grepl("MULTIPLE IMPUTATIONS WITH 1 DEGREE(S) OF FREEDOM FOR THE OVERALL TEST", sectionToParse[1], fixed=TRUE)
    
    if (twoGroupsOnly) {
      dfOmnibus <- 1
      dfPairwise <- NA_integer_
      #drop single df line and blank line
      sectionToParse <- sectionToParse[3:length(sectionToParse)]
    }
    else {
      #get degrees of freedom
      dfLines <- paste(sectionToParse[1:2], collapse=" ")
      dfOmnibus <- as.numeric(sub("^.*MULTIPLE IMPUTATIONS WITH (\\d+) DEGREE\\(S\\) OF FREEDOM FOR THE OVERALL TEST.*$", "\\1", dfLines, perl=TRUE))
      dfPairwise <- as.numeric(sub("^.*AND (\\d+) DEGREE OF FREEDOM FOR THE PAIRWISE TESTS.*$", "\\1", dfLines, perl=TRUE))
      #drop two subsequent df lines and blank line
      sectionToParse <- sectionToParse[2:length(sectionToParse)]
    }
    
    chip_pattern <- "Chi-Square\\s+P-Value"
  }
  
  #need to handle case of 4+ classes, where it becomes four-column output...
  #columnNames <- c("Mean", "S.E.")

  #obtain any section that begins with no spaces (i.e., each variable)
  variableSections <- getMultilineSection("\\S+", sectionToParse, filename, allowMultiple=TRUE, allowSpace=FALSE)
  if (section_is_missing(variableSections)) {
    ret <- list()
    class(ret) <- c("mplus.auxE", "list")
    return(ret)
  }

  varnames <- sectionToParse[section_matchlines(variableSections)]

  extract_step_shift <- function(step_lines) {
    class_prob_idx <- grep(
      "^\\s*Classification Probabilities for the Step 1 Most Likely",
      step_lines, perl = TRUE
    )
    counts_lines <- if (length(class_prob_idx) > 0L) {
      step_lines[seq_len(class_prob_idx[1L] - 1L)]
    } else {
      step_lines
    }

    count_matches <- extract_pcre_groups(
      counts_lines,
      "^\\s*(\\d+)\\s+([0-9.\\-]+)\\s+([0-9.\\-]+)\\s+([0-9.\\-]+)\\s+([0-9.\\-]+)\\s*$"
    )
    count_matches <- Filter(function(x) length(x) == 6L, count_matches)
    step_counts <- if (length(count_matches) > 0L) {
      mat <- do.call(rbind, lapply(count_matches, `[`, 2:6))
      data.frame(
        class = as.integer(mat[, 1L]),
        first_step_count = as.numeric(mat[, 2L]),
        first_step_prop = as.numeric(mat[, 3L]),
        last_step_count = as.numeric(mat[, 4L]),
        last_step_prop = as.numeric(mat[, 5L]),
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }

    step_class <- NULL
    if (length(class_prob_idx) > 0L && (class_prob_idx[1L] + 2L) <= length(step_lines)) {
      prob_lines <- step_lines[(class_prob_idx[1L] + 2L):length(step_lines)]
      step_class <- unlabeledMatrixExtract(prob_lines, filename)
    }

    if (is.null(step_counts) && is.null(step_class)) return(NULL)
    list(counts = step_counts, classification = step_class)
  }

  vc <- list()
  vcat <- list()
  vomnibus <- list()
  vpairwise <- list()
  vstepshift <- list()
  for (v in 1:length(variableSections)) {
    thisSection <- variableSections[[v]]
    var_name <- trimSpace(varnames[v])

    # Mplus 9.0+ appends Step 1 vs. Last Step class count & classification tables
    # for 3-step auxiliary settings (D3STEP, DU3STEP, D3STEPC, (e)). Slice them off
    # before parsing equality tests so "First Step vs. Last Step" does not collide.
    stepCountsLine <- grep(
      "^\\s*Final Class Counts and Proportions for the Latent Class Patterns\\s*$",
      thisSection, perl = TRUE
    )
    if (length(stepCountsLine) > 0L) {
      step_shift <- extract_step_shift(thisSection[stepCountsLine[1L]:length(thisSection)])
      if (!is.null(step_shift)) vstepshift[[var_name]] <- step_shift
      thisSection <- thisSection[seq_len(stepCountsLine[1L] - 1L)]
    }

    if(any(grepl("Prob\\s*S\\.E\\.\\s*Odds Ratio", thisSection))){
      # This is a categorical auxiliary variable, handle accordingly
      thisSection <- thisSection[!thisSection == ""]
      props <- thisSection[grepl("^\\s*Class\\s+", thisSection, perl = TRUE) | grepl("^\\s*Category\\s+", thisSection, perl = TRUE)]
      props <- strsplit(trimSpace(props), "\\s+")
      lengths <- sapply(props, length)
      if(!(all(c(2, 8) %in% lengths))){
        message("Could not extract categorical conditional variables.")
      }
      labs <- sapply(props[lengths == 2], `[`, 2)
      props <- data.frame(do.call(rbind, props[lengths == 8]), stringsAsFactors = FALSE)
      props$X2 <- as.integer(props$X2)
      which_lab <- 1
      props$X1[1] <- labs[which_lab]
      if (nrow(props) >= 2L) {
        for(i in 2:nrow(props)){
          if(!sign(props$X2[i]-props$X2[i-1] > 0)){
            which_lab <- which_lab + 1
          }
          props$X1[i] <- labs[which_lab]
        }
      }
      names(props) <- c("class", "category", "prob", "se", "or", "se_or", "ci_lower", "ci_upper")
      num_cols <- c("prob", "se", "or", "se_or", "ci_lower", "ci_upper")
      props[num_cols] <- lapply(props[num_cols], function(col) suppressWarnings(mplus_as.numeric(col)))
      
      tests <- thisSection[grepl("^\\s*Overall test\\b", thisSection, perl = TRUE) | grepl("^\\s*Class\\s+\\d+\\s+vs\\.\\s+\\d+", thisSection, perl = TRUE)]
      tests <- strsplit(trimSpace(tests), "\\s+")
      lengths <- sapply(tests, length)
      tests[lengths == 5] <- lapply(tests[lengths == 5], function(x){
        c("", x[1], "", x[2], x[2:5])
      })
      tests <- data.frame(do.call(rbind, tests), stringsAsFactors = FALSE)
      tests <- tests[, -c(1, 3, 4), drop = FALSE]
      names(tests)[1:5] <- c("class", "versus", "chi_square", "p", "df")
      tests$chi_square <- suppressWarnings(mplus_as.numeric(tests$chi_square))
      tests$p <- suppressWarnings(mplus_as.numeric(tests$p))
      tests$df <- as.integer(tests$df)
      if(any(grepl("NUMBER OF OBSERVATIONS USED FOR THE AUXILIARY VARIABLE", thisSection, fixed = TRUE))){
        tests <- cbind(tests, n = as.integer(gsub("^.+?(\\d{1,}).*?$", "\\1", grep("NUMBER OF OBSERVATIONS USED FOR THE AUXILIARY VARIABLE", thisSection, fixed = TRUE, value = TRUE))))
      }
      
      vcat[[var_name]] <- list(parameters = props, tests = tests)
    } else {
      # If it is not categorical, extract the conditional means. This is the 'old' code
      #mean s.e. match
      meanSELine <- grep("^\\s*Mean\\s*S\\.E\\.\\s*$", thisSection, perl=TRUE)
      meanSE_twoColumn <- FALSE
      
      #check for side-by-side output
      if (length(meanSELine) == 0) {
        meanSELine <- grep("^\\s*Mean\\s+S\\.E\\.\\s+Mean\\s+S\\.E\\.\\s*$", thisSection, perl=TRUE)
        if (length(meanSELine) > 0) {
          meanSE_twoColumn <- TRUE #side-by-side output
        } else {
          print(paste("Couldn't match mean and s.e. for variable", var_name, "It may not be a continous variable. Support for categorical variables has not yet been implemented."))
          next
        }
      }
      
      chiPLine <- grep(paste0("^\\s*", chip_pattern, "\\s*$"), thisSection, perl=TRUE)
      chiP_twoColumn <- FALSE
      
      if (length(chiPLine) == 0L) {
        chiPLine <- grep(paste0("^\\s*", chip_pattern, "\\s+", chip_pattern, "\\s*$"), thisSection, perl=TRUE)
        if (length(chiPLine) > 0) {
          chiP_twoColumn <- TRUE #side-by-side output
        } else {
          message(paste("Couldn't match chi-square and p-value for variable", var_name, "It may not be a continous variable. Support for categorical variables has not yet been implemented."))
          next
        }
      }
      
      means <- thisSection[(meanSELine[1]+1):(chiPLine[1]-1)]
      chip <- thisSection[(chiPLine[1]+1):length(thisSection)]
      
      #handle cases where there is wide side-by-side output (for lots of classes)
      if (meanSE_twoColumn) {      
        #pre-process means to divide two-column output
        splitMeans <- friendlyGregexpr("Class", means)
        el <- unique(splitMeans$element)
        meansReparse <- c()
        pos <- 1
        for (i in el) {
          match <- splitMeans[splitMeans$element==i, , drop=FALSE]
          if (nrow(match) > 1L) {
            for (j in 1:nrow(match)) {
              #if not on last match for this line, then go to space before j+1 match. Otherwise, end of line
              end <- ifelse(j < nrow(match), match[j+1,"start"] - 1, nchar(means[i]))
              meansReparse[pos] <- trimSpace(substr(means[i], match[j, "start"], end))
              pos <- pos+1
            }
          } else if (nrow(match) == 1L) {
            meansReparse[pos] <- trimSpace(means[i])
            pos <- pos+1
          }
        }
        
        means <- meansReparse
      }
      
      if (chiP_twoColumn) {
        #divide chi square and p section into unique entries for each comparison
        splitChiP <- friendlyGregexpr("(Overall|Class)", chip)
        el <- unique(splitChiP$element)
        chipReparse <- c()
        pos <- 1
        #split into first half of columns and second half
        for (i in el) {
          match <- splitChiP[splitChiP$element==i, , drop=FALSE]
          if (nrow(match) > 1L) {
            for (j in 1:nrow(match)) {
              #if not on last match for this line, then go to space before j+1 match. Otherwise, end of line
              end <- ifelse(j < nrow(match), match[j+1,"start"] - 1, nchar(chip[i]))
              chipReparse[pos] <- trimSpace(substr(chip[i], match[j, "start"], end))
              pos <- pos+1
            }
          } else if (nrow(match) == 1L) {
            chipReparse[pos] <- trimSpace(chip[i])
            pos <- pos+1
          }
        }
        
        chip <- chipReparse
      }
      
      mean_matches <- extract_pcre_groups(means, "^\\s*Class\\s+(\\d+)\\s+([0-9.\\-*]+)\\s+([0-9.\\-*]+)")
      mean_matches <- Filter(function(x) length(x) == 4L, mean_matches)
      class.M.SE <- lapply(mean_matches, function(x) {
        c(
          class = as.integer(x[2L]),
          m = suppressWarnings(mplus_as.numeric(x[3L])),
          se = suppressWarnings(mplus_as.numeric(x[4L]))
        )
      })
      
      #need to trap overall test versus pairwise comparisons -- this could be much more elegant, but works for now
      overallLine <- grep("^\\s*Overall test\\s+.*$", chip, perl=TRUE)
      if (length(overallLine) > 0L) {
        if (dcon) {
          #DCON output has degrees of freedom inline as a third column
          om_match <- extract_pcre_groups(
            chip[overallLine[1L]],
            "^\\s*Overall test\\s+([0-9.\\-*]+)\\s+([0-9.\\-*]+)\\s+([0-9.\\-*]+)"
          )[[1L]]
          class.chi.p.omnibus <- if (length(om_match) == 4L) {
            data.frame(
              chisq = suppressWarnings(mplus_as.numeric(om_match[2L])),
              df = as.numeric(om_match[4L]),
              p = suppressWarnings(mplus_as.numeric(om_match[3L])),
              var = var_name,
              stringsAsFactors = FALSE
            )
          } else {
            NULL
          }
        } else {
          om_match <- extract_pcre_groups(
            chip[overallLine[1L]],
            "^\\s*Overall test\\s+([0-9.\\-*]+)\\s+([0-9.\\-*]+)"
          )[[1L]]
          class.chi.p.omnibus <- if (length(om_match) == 3L) {
            data.frame(
              chisq = suppressWarnings(mplus_as.numeric(om_match[2L])),
              df = as.numeric(dfOmnibus),
              p = suppressWarnings(mplus_as.numeric(om_match[3L])),
              var = var_name,
              stringsAsFactors = FALSE
            )
          } else {
            NULL
          }
        }
        chip <- chip[-overallLine] #drop omnibus line from subsequent pairwise parsing
      } else {
        class.chi.p.omnibus <- NULL
      }
      
      if (dcon) {
        #DCON output has degrees of freedom inline as a third column
        pw_matches <- extract_pcre_groups(
          chip,
          "^\\s*Class\\s+(\\d+)\\s+vs\\.\\s+(\\d+)\\s+([0-9.\\-*]+)\\s+([0-9.\\-*]+)\\s+([0-9.\\-*]+)"
        )
        pw_matches <- Filter(function(x) length(x) == 6L, pw_matches)
        class.chi.p.pairwise <- lapply(pw_matches, function(x) {
          data.frame(
            classA = as.character(x[2L]),
            classB = as.character(x[3L]),
            chisq = suppressWarnings(mplus_as.numeric(x[4L])),
            df = as.integer(x[6L]),
            p = suppressWarnings(mplus_as.numeric(x[5L])),
            var = var_name,
            stringsAsFactors = FALSE
          )
        })
      } else {
        pw_matches <- extract_pcre_groups(
          chip,
          "^\\s*Class\\s+(\\d+)\\s+vs\\.\\s+(\\d+)\\s+([0-9.\\-*]+)\\s+([0-9.\\-*]+)"
        )
        pw_matches <- Filter(function(x) length(x) == 5L, pw_matches)
        class.chi.p.pairwise <- lapply(pw_matches, function(x) {
          data.frame(
            classA = as.character(x[2L]),
            classB = as.character(x[3L]),
            chisq = suppressWarnings(mplus_as.numeric(x[4L])),
            df = as.integer(dfPairwise),
            p = suppressWarnings(mplus_as.numeric(x[5L])),
            var = var_name,
            stringsAsFactors = FALSE
          )
        })
      }
      
      class.chi.p.pairwise <- if (length(class.chi.p.pairwise) > 0L) {
        do.call("rbind", class.chi.p.pairwise)
      } else {
        list()
      }
      
      #build data.frame
      if (length(class.M.SE) > 0L) {
        class.M.SE <- data.frame(do.call("rbind", class.M.SE), var=var_name, stringsAsFactors = FALSE)
        vc[[var_name]] <- class.M.SE
        vomnibus[[var_name]] <- class.chi.p.omnibus
        vpairwise[[var_name]] <- class.chi.p.pairwise
      }
    }
    
  }
  ret <- list()
  if(length(vc) > 0){
    allMeans <- data.frame(do.call("rbind", vc), row.names=NULL)
    allMeans <- allMeans[,c("var", "class", "m", "se")]
    
    allMeans <- reshape(allMeans, idvar="var", timevar="class", direction="wide")
    allOmnibus <- data.frame(do.call("rbind", vomnibus), row.names=NULL)
    allMeans <- merge(allMeans, allOmnibus, by="var")
    if (! all(sapply(vpairwise, length) == 0L)) {
      vpw_nonempty <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, vpairwise)
      allPairwise <- data.frame(do.call("rbind", vpw_nonempty), row.names=NULL, stringsAsFactors = FALSE)
      allPairwise <- allPairwise[,c("var", "classA", "classB", "chisq", "df", "p")]
    } else {
      allPairwise <- data.frame(var=factor(character(0)),
                                classA=factor(character(0)),
                                classB=factor(character(0)),
                                chisq=numeric(0),
                                df=numeric(0),
                                p=numeric(0))
    }
    
    ret <- list(overall=allMeans, pairwise=allPairwise)
  }
  if(length(vcat) > 0){
    ret <- c(ret, vcat)
  }
  if (length(vstepshift) > 0L) {
    ret$step_shift <- vstepshift
  }
  class(ret) <- c("mplus.auxE", "list")

  return(ret)
}

#' Extract output of R3STEP procedure
#'
#' @param outfiletext character vector of Mplus output file from which to extract the AUX section
#' @param filename filename of the Mplus output file being processed
#' @return A list containing the parsed R3STEP sections
#' @keywords internal
extractR3step <- function(outfiletext, filename) {
  allSections <- list() # holds parameters for all identified sections

  parR3stepSection <- getSection(
    "^(?:TESTS OF )?CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS USING$::^THE 3-STEP(?: \\([^)]+\\))? PROCEDURE$",
    outfiletext
  )
  if (is.null(parR3stepSection)) return(allSections) # nothing
  
  # The highest class value is the default reference, so we need to add 1 to the highest alternative reference class number to infer the first class
  refMatches <- extract_pcre_groups(parR3stepSection, "Parameterization using Reference Class (\\d+)")
  classNums <- vapply(
    Filter(function(x) length(x) == 2L, refMatches),
    function(x) as.numeric(x[2L]),
    numeric(1L)
  )
  firstLine <- if (length(classNums) > 0L) {
    paste("Parameterization using Reference Class", max(classNums) + 1)
  } else {
    character(0)
  }
  
  parR3stepSection <- c(firstLine, parR3stepSection)
  allSections <- appendListElements(allSections, extractParameters_1section(filename, parR3stepSection, "multinomialTests"))
  
  # odds ratios
  parR3oddsSection <- getSection(
    "^ODDS RATIOS FOR (?:TESTS OF )?CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS$::^USING THE 3-STEP(?: \\([^)]+\\))? PROCEDURE$",
    outfiletext
  )
  
  if (!is.null(parR3oddsSection)) {
    parR3oddsSection <- c(firstLine, parR3oddsSection)
    allSections <- appendListElements(allSections, extractParameters_1section(filename, parR3oddsSection, "multinomialOdds"))
  }
  
  # confidence intervals for 3-step procedure
  ciR3stepSection <- getSection(
    "^CONFIDENCE INTERVALS FOR (?:TESTS OF )?CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS$::^USING THE 3-STEP(?: \\([^)]+\\))? PROCEDURE$",
    outfiletext
  )

  if (!is.null(ciR3stepSection)) {
    ciR3stepSection <- c(firstLine, ciR3stepSection)
    hline <- detectColumnNames(filename, ciR3stepSection, "confidence_intervals")
    allSections <- appendListElements(allSections, extractParameters_1section(filename, ciR3stepSection, "ci.unstandardized"))
  }
  
  # confidence intervals odds ratios for 3-step procedure
  ciR3stepOddsSection <- getSection(
    "^CONFIDENCE INTERVALS OF ODDS RATIOS FOR (?:TESTS OF )?CATEGORICAL LATENT VARIABLE MULTINOMIAL$::^LOGISTIC REGRESSIONS USING THE 3-STEP(?: \\([^)]+\\))? PROCEDURE$",
    outfiletext
  )
  
  if (!is.null(ciR3stepOddsSection) && !is.null(ciR3stepSection)) {
    # at present, the CI header is not reprinted in the odds ratio section, so we need to copy-paste header line for parsing
    ciR3stepOddsSection <- c(firstLine, ciR3stepSection[attr(hline, "header_lines")], ciR3stepOddsSection)
    allSections <- appendListElements(allSections, extractParameters_1section(filename, ciR3stepOddsSection, "ci.odds"))
  }
  
  return(allSections)

}
