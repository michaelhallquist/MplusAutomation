#' Extract Indirect Effects output
#'
#' This function parses the section titles TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS
#' It returns a list composed of $overall and $specific effects
#'
#' @param outfiletext a character vector containing the indirect effects output section returned by getSection 
#' @param curFile the name of the current output file being parsed
#' @return An mplus.indirect object (inherits list) containing $overall and $specific 
#' @keywords internal
extractIndirect <- function(outfiletext, curfile) {
  indirectSection <- getSection("^TOTAL, TOTAL INDIRECT, SPECIFIC INDIRECT, AND DIRECT EFFECTS( FOR LATENT RESPONSE VARIABLES)*$", outfiletext)
  if (is.null(indirectSection)) return(list()) #no indirect output
  
  columnNames <- detectColumnNames(curfile, trimSpace(indirectSection[1:50]), "model_results") #assume that column headers are somewhere in the first 50 lines
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
      
      elist$summaries <- data.frame(pred=elist$pred, outcome=elist$outcome, rbind(totalLine, totalIndirectLine, direct), row.names=NULL)
      
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
          source <- toparse[1] #first variable is the "source" (i.e., the variable furthest upstream) (X IND Y)
          outcome <- strsplit(toparse[length(toparse)], "\\s+")[[1]] #this should always be the outcome and should have the statistics on it
          names(outcome) <- columnNames
          outcome <- data.frame(as.list(outcome))
          intervening <- toparse[2:(length(toparse)-1)]
          thisEffect <- rbind(thisEffect, data.frame(pred=source, intervening = paste(intervening, collapse="."), outcome))
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