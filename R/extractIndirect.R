#' Extract Indirect Effects output
#'
#' This function parses both unstandardized and standardized indirect effects
#' It returns a list composed of $unstandardized and $standardized.
#' The base structure of each is a list containing $overall and $specific effects (as data.frames)
#'
#' @param outfiletext a character vector containing the indirect effects output section returned by getSection 
#' @param curFile the name of the current output file being parsed
#' @return An mplus.indirect object (inherits list) containing $overall and $specific 
#' @keywords internal
extractIndirect <- function(outfiletext, curfile) {
  indirect_results <- list()
  
  indirectSection <- getSection("^TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS( FOR LATENT RESPONSE VARIABLES)*$", outfiletext)
  if (!is.null(indirectSection)) { indirect_results[["unstandardized"]] <- extractIndirect_section(indirectSection, curfile, sectionType="model_results") }
 
  ciSection <- getSection("^CONFIDENCE INTERVALS OF TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS*$", outfiletext)
  if (!is.null(ciSection)) { indirect_results[["ci.unstandardized"]] <- extractIndirect_section(ciSection, curfile, sectionType="confidence_intervals") }
  
  stdindirectSection <- getSection("^STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS$", outfiletext)
  if (!is.null(stdindirectSection)) {
    #check for each subsection
    
    stdheaders <- grep("STDY?X? Standardization", stdindirectSection, perl=TRUE) 
    if (length(stdheaders) > 0L) {
      for (ix in 1:length(stdheaders)) {
        endsection <- ifelse(ix == length(stdheaders), length(stdindirectSection), stdheaders[ix+1])
        thissection <- stdindirectSection[(stdheaders[ix]+1):(endsection-1)]
        secname <- tolower(sub("(STDY?X?)\\s+Standardization", "\\1", stdindirectSection[stdheaders[ix]], perl=TRUE))
        indirect_results[[paste0(secname, ".standardized")]] <- extractIndirect_section(thissection, curfile, sectionType="model_results")    
      }
    }
  }
  
  cistdindirectSection <- getSection("^CONFIDENCE INTERVALS OF STANDARDIZED TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT,( AND DIRECT EFFECTS)*$", outfiletext) #last part is omitted before Mplus v8
  if (!is.null(cistdindirectSection)) {
    #check for each subsection
    
    stdheaders <- grep("STDY?X? Standardization", cistdindirectSection, perl=TRUE) 
    if (length(stdheaders) > 0L) {
      for (ix in 1:length(stdheaders)) {
        endsection <- ifelse(ix == length(stdheaders), length(cistdindirectSection), stdheaders[ix+1])
        thissection <- cistdindirectSection[(stdheaders[ix]+1):(endsection-1)]
        secname <- tolower(sub("(STDY?X?)\\s+Standardization", "\\1", cistdindirectSection[stdheaders[ix]], perl=TRUE))
        indirect_results[[paste0("ci.", secname, ".standardized")]] <- extractIndirect_section(thissection, curfile, sectionType="confidence_intervals")    
      }
    }
  }
  
  #mirror parameters
  listOrder <- c("unstandardized", "ci.unstandardized",
    "stdyx.standardized", "ci.stdyx.standardized",
    "stdy.standardized", "ci.stdy.standardized",
    "std.standardized", "ci.std.standardized")
  listOrder <- listOrder[listOrder %in% names(indirect_results)]
  
  #only re-order if out of order
  if(!identical(names(indirect_results), listOrder)) indirect_results <- indirect_results[listOrder]
  
  return(indirect_results)
  
}

#' Extract Indirect Effects output
#'
#' This function parses a given indirect section
#' It returns a list composed of $overall and $specific effects
#'
#' @param indirectSection a character vector containing the indirect effects for a specific section (e.g., stdyx)  
#' @param curFile the name of the current output file being parsed
#' @return An mplus.indirect object (inherits list) containing $overall and $specific 
#' @keywords internal
extractIndirect_section <- function(indirectSection, curfile, sectionType) {
  
  columnNames <- detectColumnNames(curfile, trimSpace(indirectSection[1:50]), sectionType) #assume that column headers are somewhere in the first 50 lines
  columnNames[1] <- "outcome" #rename param -> outcome for clarity
  
  multipleGroupMatches <- grep("^\\s*Group \\w+(?:\\s+\\(\\d+\\))*\\s*$", indirectSection, ignore.case=TRUE, perl=TRUE) #support Mplus v8 syntax Group G1 (0) with parentheses of numeric value
  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos))) #courtesy of https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
  
  #this will return the entire section as the first element of a list if no groups are found
  isplit <- splitAt(indirectSection, multipleGroupMatches)
  
  indirectOutput <- vector("list", length(isplit))
  
  for (idx in 1:length(isplit)) {
    i_element <- isplit[[idx]]
    i_out <- list()
    effectHeaders <- grep("^Effects from [A-z_0-9]+ to [A-z_0-9]+$", i_element, ignore.case=TRUE, perl=TRUE)
    if (length(effectHeaders) == 0L) { next } #nothing to parse -- skip to next iteration
    
    #determine group name, if relevant
    if (grepl("^\\s*Group .*$", i_element[1L])) {
      groupName <- sub("^\\s*Group (\\w+)(?:\\s+\\(\\d+\\))*\\s*$", "\\1", i_element[1L], perl=TRUE)  
    } else {
      groupName <- NULL
    }
    
    for (e in 1:length(effectHeaders)) {
      elist <- list()
      elist$pred <- sub("^Effects from ([A-z_0-9]+) to [A-z_0-9]+$", "\\1", i_element[effectHeaders[e]], ignore.case=TRUE, perl=TRUE)
      elist$outcome <- sub("^Effects from [A-z_0-9]+ to ([A-z_0-9]+)$", "\\1", i_element[effectHeaders[e]], ignore.case=TRUE, perl=TRUE)
      
      end <- ifelse (e < length(effectHeaders), effectHeaders[e+1]-1, length(i_element))
      esection <- i_element[(effectHeaders[e]+1):end]
      
      #parse total, total indirect, specific indirect, and direct
      totalLine <- trimSpace(grep("Total\\s+[\\-0-9\\.]+.*$", esection, ignore.case=TRUE, perl=TRUE, value=TRUE))
      if (length(totalLine) > 0L) {
        totalLine <- as.list(strsplit(totalLine, "\\s+", perl=TRUE)[[1]])
        names(totalLine) <- columnNames; totalLine$summary <- "Total"; totalLine$outcome <- NULL
      }
      
      totalIndirectLine <- trimSpace(grep("(Indirect|(Total|Sum of) indirect)\\s+[\\-0-9\\.]+.*$", esection, ignore.case=TRUE, perl=TRUE, value=TRUE))
      if (length(totalIndirectLine) > 0L) {
        totalIndirectLine <- as.list(strsplit(totalIndirectLine, "\\s+", perl=TRUE)[[1]])
        if (paste(unlist(totalIndirectLine[1:3]), collapse=" ") == "Sum of indirect") { #mplus v6 output? Not sure what generates sum versus total
          totalIndirectLine <- totalIndirectLine[-1:-3]
          hname <- "Sum of indirect"
        } else if (paste(unlist(totalIndirectLine[1:2]), collapse=" ") == "Total indirect") {
          totalIndirectLine <- totalIndirectLine[-1:-2]
          hname <- "Total indirect"
        } else if (unlist(totalIndirectLine[1]) == "Indirect") {
          totalIndirectLine <- totalIndirectLine[-1]
          hname <- "Indirect"
        } else { stop("Unable to parse header from total indirect line: ", totalIndirectLine)}
        names(totalIndirectLine) <- columnNames[-1]; #don't include "outcome" from columnNames since this is added en masse to summaries below
        totalIndirectLine$summary <- hname #relabel according to Mplus output
      }
      
      #mplus v8 appears to output a line 'Direct effect' with parameters, whereas prior versions placed this under specific indirect 
      directLine <- trimSpace(grep("Direct effect\\s+[\\-0-9\\.]+.*$", esection, ignore.case=TRUE, perl=TRUE, value=TRUE))
      if (length(directLine) > 0L) {
        direct <- as.list(strsplit(directLine, "\\s+", perl=TRUE)[[1]])
        direct <- direct[-2] #drop 'line' element for matching column names
        names(direct) <- columnNames; direct$summary <- "Direct"; direct$outcome <- NULL
      } else {
        directSection <- strsplit(trimSpace(getMultilineSection("Direct", esection, curfile)), "\\s+")
        useful <- which(sapply(directSection, length) > 1L)
        if (length(useful) == 1L) {
          direct <- as.list(directSection[[useful]])
          names(direct) <- columnNames
          direct$summary <- "Direct"; direct$outcome <- NULL
        } else {
          direct <- list()
        }
      }
      
      elist$summaries <- data.frame(pred=elist$pred, outcome=elist$outcome, 
        rbind(unlist(totalLine), unlist(totalIndirectLine), unlist(direct)), row.names=NULL, stringsAsFactors=FALSE)
      
      #reorder columns to put pred, outcome, summary first. Use columnNames vector without "outcome" to place remainder in order
      elist$summaries <- elist$summaries[,c("pred", "outcome", "summary", columnNames[-1])]
      
      #use white space to demarcate ending of specific indirect subsection
      specSection <- trimSpace(getMultilineSection("Specific indirect", esection, curfile))
      blanks <- which(specSection=="")
      if (length(blanks) > 0L) {
        thisEffect <- NULL
        
        for (i in 1:length(blanks)) {
          if (i < length(blanks)) {
            startLine <- blanks[i]+1
            endLine <- blanks[i+1]-1
            if (startLine >= endLine) { next } #occurs with consecutive blanks -> skip out
            toparse <- specSection[startLine:endLine] 
          } else { 
            if (blanks[i]+1 < length(specSection)) { 
              toparse <- specSection[(blanks[i]+1):length(specSection)]
            } else { next } #nothing to parse, just a trailing blank
          }
          #if (length(toparse) < 2L) { next } #double blank line problem
          outcome <- toparse[1] #first variable is the "outcome" (i.e., the variable furthest downstream) (X IND Y)
          source <- strsplit(toparse[length(toparse)], "\\s+")[[1]] #this should always be the earliest variable in the chain; it should have the statistics on it
          names(source) <- columnNames
          names(source)[1] <- "pred" #for specific effects, the first column of source is actually the predictor, not outcome (bit of a kludge here)
          source <- data.frame(as.list(source), stringAsFactors=FALSE)
          intervening <- toparse[2:(length(toparse)-1)]
          thisEffect <- rbind(thisEffect, data.frame(source[,"pred", drop=F], intervening = paste(intervening, collapse="."), outcome=outcome, source[,-1*which(names(source)=="pred")]))
        }
        elist$specific <- thisEffect
      }
      
      if (!is.null(groupName)) { #add group name if multiple groups analysis
        elist$summaries$group <- groupName
        if (!is.null(elist$specific)) { elist$specific$group <- groupName } #some sections don't have specific indirects
      }
      
      i_out[[e]] <- elist #list of outputs for this section
    }
    
    indirectOutput[[idx]] <- i_out
  }
  
  #name list elements according to predictor and outcome 
  #names(indirectOutput) <- sapply(indirectOutput, function(el) { paste(el$pred, el$outcome, sep=".") })
  
  #change format to return two data.frames, one with all summaries, the other with all specific
  summarydf <- do.call(rbind, lapply(indirectOutput, function(i_out) {
        do.call(rbind, lapply(i_out, function(el) { el$summaries }))
      }))
  
  specificdf <- do.call(rbind, lapply(indirectOutput, function(i_out) {
        do.call(rbind, lapply(i_out, function(el) { el$specific }))
      }))
  
  row.names(summarydf) <- NULL; row.names(specificdf) <- NULL 
  toreturn <- list(overall=summarydf, specific=specificdf)
  class(toreturn) <- "mplus.indirect"
  
  return(toreturn)
}