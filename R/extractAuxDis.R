#' Extract Auxiliary condition means and comparisons.
#'
#' To do: add details.
#'
#' @param outfiletext
#' @param filename
#' @return A data frame
#' @importFrom gsubfn strapply
#' @keywords internal
extractAux <- function(outfiletext, filename) {
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
  vomnibus <- list()
  vpairwise <- list()
  for (v in 1:length(variableSections)) {
    thisSection <- variableSections[[v]]
    #mean s.e. match
    meanSELine <- grep("^\\s*Mean\\s*S\\.E\\.\\s*$", thisSection, perl=TRUE)

    twoColumn <- FALSE
    #check for side-by-side output
    if (length(meanSELine) == 0) {
      meanSELine <- grep("^\\s*Mean\\s+S\\.E\\.\\s+Mean\\s+S\\.E\\.\\s*$", thisSection, perl=TRUE)
      if (length(meanSELine) > 0) {
        twoColumn <- TRUE #side-by-side output
        chiPLine <- grep("\\s*Chi-Square\\s+P-Value\\s+Chi-Square\\s+P-Value\\s*$", thisSection, perl=TRUE)
      } else {
        print(paste("Couldn't match mean and s.e. for variable", varnames[[v]], "It may not be a continous variable. Support for categorical variables has not yet been implemented."))
        next
      }
    } else {
      chiPLine <- grep("\\s*Chi-Square\\s*P-Value\\s*", thisSection, perl=TRUE)
    }

    means <- thisSection[(meanSELine[1]+1):(chiPLine[1]-1)]
    chip <- thisSection[(chiPLine[1]+1):length(thisSection)]

    #handle cases where there is wide side-by-side output (for lots of classes)
    if (twoColumn) {
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

      chip <- chipReparse
      means <- meansReparse
    }

    class.M.SE <- strapply(means, "^\\s*Class\\s+(\\d+)\\s+([\\d\\.-]+)\\s+([\\d\\.-]+)", function(class, m, se) {
          return(c(class=as.integer(class), m=as.numeric(m), se=as.numeric(se)))
        }, simplify=FALSE)

    #drop nulls
    class.M.SE[sapply(class.M.SE, is.null)] <- NULL

    #need to trap overall test versus pairwise comparisons -- this could be much more elegant, but works for now
    overallLine <- grep("^\\s*Overall test\\s+.*$", chip, perl=TRUE)
    if (length(overallLine) > 0L) {
      class.chi.p.omnibus <- strapply(chip[overallLine], "^\\s*Overall test\\s+([\\d\\.-]+)\\s+([\\d\\.-]+)", function(chisq, p) {
            #return(c(class1="Overall", class2="", chisq=as.numeric(chisq), p=as.numeric(p)))
            return(c(chisq=as.numeric(chisq), df=as.numeric(dfOmnibus), p=as.numeric(p)))
          }, simplify=FALSE)[[1L]]
      chip <- chip[-overallLine] #drop omnibus line from subsequent pairwise parsing
    } else {
      class.chi.p.omnibus <- list()
    }

    class.chi.p.pairwise <- strapply(chip, "^\\s*Class\\s+(\\d+)\\s+vs\\.\\s+(\\d+)\\s+([\\d\\.-]+)\\s+([\\d\\.-]+)", function(classA, classB, chisq, p) {
          return(c(classA=as.character(classA), classB=as.character(classB), chisq=as.numeric(chisq), df=as.numeric(dfPairwise), p=as.numeric(p)))
        }, simplify=FALSE)

    class.chi.p.pairwise[sapply(class.chi.p.pairwise, is.null)] <- NULL

    if (length(class.chi.p.pairwise) > 0L) { class.chi.p.pairwise <- data.frame(do.call("rbind", class.chi.p.pairwise), var=varnames[v]) }
    if (length(class.chi.p.omnibus) > 0L) { class.chi.p.omnibus <- data.frame(cbind(t(class.chi.p.omnibus), var=varnames[v])) }

    #build data.frame
    class.M.SE <- data.frame(do.call("rbind", class.M.SE), var=varnames[v])
    vc[[varnames[v]]] <- class.M.SE
    vomnibus[[varnames[v]]] <- class.chi.p.omnibus
    vpairwise[[varnames[v]]] <- class.chi.p.pairwise
  }


  allMeans <- data.frame(do.call("rbind", vc), row.names=NULL)
  allMeans <- allMeans[,c("var", "class", "m", "se")]

  allMeans <- reshape(allMeans, idvar="var", timevar="class", direction="wide")
  allOmnibus <- data.frame(do.call("rbind", vomnibus), row.names=NULL)
  allMeans <- merge(allMeans, allOmnibus, by="var")
  if (! all(sapply(vpairwise, length) == 0L)) {
    allPairwise <- data.frame(do.call("rbind", vpairwise), row.names=NULL)
    allPairwise$chisq <- as.numeric(as.character(allPairwise$chisq))
    allPairwise$df <- as.integer(as.character(allPairwise$df))
    allPairwise$p <- as.numeric(as.character(allPairwise$p))
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
  class(ret) <- c("list", "mplus.auxE")

  return(ret)
}
