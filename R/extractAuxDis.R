#' Extract Auxillary
#'
#' To do: add details.
#'
#' @param outfiletext
#' @param filename
#' @return A data frame
#' @keywords internal
extractAux <- function(outfiletext, filename) {
  require(gsubfn)
  if (missing(outfiletext) || is.na(outfiletext) || is.null(outfiletext)) stop("Missing mean equality to parse.\n ", filename)
  
  meanEqSection <- getSection("EQUALITY TESTS OF MEANS", outfiletext)
  
  if (is.null(meanEqSection)) return(NULL) #model does not contain mean equality check
  
  #check for whether there is one or two more trailing lines
  twoGroupsOnly <- grepl("MULTIPLE IMPUTATIONS WITH 1 DEGREE(S) OF FREEDOM FOR THE OVERALL TEST", meanEqSection[1], fixed=TRUE)
  
  if (twoGroupsOnly) {
    dfOmnibus <- 1
    dfPairwise <- NA_integer_
    #drop single df line and blank line
    meanEqSection <- meanEqSection[3:length(meanEqSection)]
  }
  else {
    #get degrees of freedom
    dfLines <- paste(meanEqSection[1:2], collapse=" ")
    dfOmnibus <- as.numeric(sub("^.*MULTIPLE IMPUTATIONS WITH (\\d+) DEGREE\\(S\\) OF FREEDOM FOR THE OVERALL TEST.*$", "\\1", dfLines, perl=TRUE))
    dfPairwise <- as.numeric(sub("^.*AND (\\d+) DEGREE OF FREEDOM FOR THE PAIRWISE TESTS.*$", "\\1", dfLines, perl=TRUE))
    #drop two subsequent df lines and blank line
    meanEqSection <- meanEqSection[2:length(meanEqSection)]
  }
  
  #need to handle case of 4+ classes, where it becomes four-column output...
  columnNames <- c("Mean", "S.E.")
  
  #obtain any section that begins with no spaces (i.e., each variable)
  variableSections <- getMultilineSection("\\S+", meanEqSection, filename, allowMultiple=TRUE, allowSpace=FALSE)
  
  varnames <- meanEqSection[attr(variableSections, "matchlines")]
  
  vc <- list()
  vp <- list()
  for (v in 1:length(variableSections)) {
    thisSection <- variableSections[[v]]
    #mean s.e. match
    meanSELine <- grep("^\\s*Mean\\s*S\\.E\\.\\s*$", thisSection, perl=TRUE)
    
    twoColumn <- FALSE
    #check for side-by-side output
    if (length(meanSELine) == 0) {
      meanSELine <- grep("^\\s*Mean\\s*S\\.E\\.\\s+Mean\\s+S\\.E\\.\\s*$", thisSection, perl=TRUE)
      if (length(meanSELine) > 0) twoColumn <- TRUE
      else 
      {print(paste("Couldn't match mean and s.e. for variable", varnames[[v]], "It may not be a continous variable. Support for categorical variables has not yet been implemented."))
       next}
    }
    
    chiPLine <- grep("\\s*Chi-Square\\s*P-Value\\s*", thisSection, perl=TRUE)
    means <- thisSection[(meanSELine[1]+1):(chiPLine[1]-1)]
    
    #get Overall Chi-2 test
    chip <- thisSection[(chiPLine[1]+1):length(thisSection)]
    chip <- chip[2]
    
    if (twoColumn) {
      chip <- trimSpace(chip) #make sure strsplit doesn't have dummies for leading and trailing
      splitMeans <- strsplit(chip, split="\\s+", perl=TRUE)
      chip <- c()
      pos <- 1
      for(i in 1:length(splitMeans)) {
        if (length(splitMeans[[i]]) > 0) {
          chip[pos] <- paste(splitMeans[[i]][3], collapse=" ")
          chip[pos+1] <- paste(splitMeans[[i]][4], collapse=" ")
        }
      }
    }
    
    
    
    if (twoColumn) {
      #pre-process means to divide two-column output (just insert returns at the right spots
      means <- trimSpace(means) #make sure strsplit doesn't have dummies for leading and trailing
      splitMeans <- strsplit(means, split="\\s+", perl=TRUE)
      means <- c()
      pos <- 1
      for(i in 1:length(splitMeans)) {
        if (length(splitMeans[[i]]) > 0) {
          means[pos] <- paste(splitMeans[[i]][1:4], collapse=" ")
          means[pos+1] <- paste(splitMeans[[i]][5:8], collapse=" ")
          pos <- pos+2
        }
      }
    }
    
    class.M.SE <- strapply(means, "^\\s*Class\\s+(\\d+)\\s+([\\d\\.-]+)\\s+([\\d\\.-]+)", function(class, m, se) {
      return(c(class=as.integer(class), m=as.numeric(m), se=as.numeric(se)))
    }, simplify=FALSE)
    
    #drop nulls
    class.M.SE[sapply(class.M.SE, is.null)] <- NULL
    
    #build data.frame
    class.M.SE <- data.frame(do.call("rbind", class.M.SE), var=varnames[v])
    ChiP <- data.frame(rbind(chip))
    names(ChiP) <- c("chi2","p") 
    vc[[varnames[v]]] <- class.M.SE
    vp[[varnames[v]]] <- ChiP
  }
  
  result1 <- data.frame(do.call("rbind", vc), row.names=NULL)
  result1<- reshape(result1, idvar="var", timevar="class", direction="wide")
  result2 <- data.frame(do.call("rbind", vp), row.names=NULL)
  result <- cbind(result1, result2)
  return(result)
}
