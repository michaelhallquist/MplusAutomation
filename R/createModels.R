# TODO: enforce use of #iterator syntax in init section for vars with length > 1
# TODO: Check array tags used in the init and body sections for validity.
# TODO: Make sure that classify tags accurately interprets all tags and errors if uninterpretable tag.
# TODO: Allow for conditional tags to use a list, such as [[nclass#class == 5]]

#note that there's a bit of trickery in interpreting list tags
#they varnames are stored as only the prefix in the initCollection (no #iterator)
#and they are referenced in the body as var#iterator
#At this point, doesn't enforce proper use of iterator with a list

#setwd("C:/Users/Michael Hallquist/Documents/Automation_Sandbox")
#createModels("C:/Users/Michael Hallquist/Documents/Automation_Sandbox/LSPD Covariate Template.txt")
#system.time(createModels("C:/Users/Michael Hallquist/Documents/Automation_Sandbox/LSPD Template.txt"))
#createModels("C:/Users/Michael Hallquist/Documents/Automation_Sandbox/LSPD Template New Init.txt")
#createModels("C:/Users/Michael Hallquist/Documents/Automation_Sandbox/L2 Multimodel Template No iter.txt")

#need to sort out why is.na is working for lookupValue in replaceBodyTags
#in particular, why isn't the current value carrying over from the previous looping iteration?

#SOME THOUGHTS RE DOCUMENTATION
#foreach tags may only be with respect to an iterator... could not have some random foreach var


#' Split a data frame into a list by rows
#'
#' Takes a data frame and returns a list with an element for each row of the data frame.
#' This is an internal function.
#'
#' @param df An object inheriting from class \code{data.frame}
#'
#' @return A list where each element is a one row data frame
#' @keywords internal
#' @examples
#' # small example using built in data
#' MplusAutomation:::splitDFByRow(mtcars)
splitDFByRow <- function(df) {
  stopifnot(inherits(df, "data.frame"))
  lapply(seq.int(nrow(df)), function(i) df[i, ])
}

#' Classifies Tags
#'
#' Accepts a vector of tags to be classified as well as the iterators.
#' Tags are classified as \sQuote{iterator}, \sQuote{array}, \sQuote{conditional}, or
#' \sQuote{simple}. This is an internal function.
#'
#' @param tagVector A vector of tags to be classified
#' @param iteratorsVector a vector of the iterators to correctly classify tags
#' @return A character vector the same length as the vectors to be tagged
#' @keywords internal
classifyTags <- function(tagVector, iteratorsVector) {
  #accepts a vector of tags to be classified
  #also needs a vector of the iterators to correctly classify tags
  #returns a vector of tag types

  #creates an empty character vector of the same length as tagVector (each element defaults to "")
  tagType <- vector(mode="character", length=length(tagVector))

  #default to missing for tag type (replaced below)
  #tagData$tagType <- NA_character_

  # named list of the regexs to match for
  # the names of each elements are used later to classify tags
  RegEx <- list(
    iterator = paste0("\\[\\[\\s*(", paste(iteratorsVector, collapse="|"), ")\\s*\\]\\]"),
    array = paste0("\\[\\[\\s*\\b([\\w\\.]+)#(", paste(iteratorsVector, collapse="|"), ")\\b\\s*\\]\\]"),

    #optional forward slash for closing tags
    #could the alternation syntax be problematic if variable names overlaps
    #(e.g., x matching xy)? Use word boundaries?
    #any reason to limit this to iterators?!
    conditional = paste0("\\[\\[\\s*/*(", paste(iteratorsVector, collapse="|"), ")\\s*[!><=]+\\s*\\d+\\s*\\]\\]"),

    #simple tags -- not wrt iterators, not conditional
    #use negative lookahead to skip tags that are iterators
    simple = paste0("\\[\\[\\s*(?!", paste(iteratorsVector, collapse="|"), ")[\\w+\\.]+\\s*\\]\\]"))

  Positions <- lapply(RegEx, grep, x = tagVector, perl = TRUE)

  # assert no duplicates, i.e., tag cannot match multiples classes
  stopifnot(!any(duplicated(unlist(Positions))))

  for (n in names(Positions)) {
    tagType[Positions[[n]]] <- n
  }

  return(tagType)
}

#' Get Initial Tags
#'
#' An internal function
#'
#' @param initCollection A list?
#' @return The initMatches
#' @keywords internal
getInitTags <- function(initCollection) {
  initMatches <- c()
  for (i in 1:length(initCollection)) {
    if (storage.mode(initCollection[[i]]) == "character") {
      matches <- friendlyGregexpr("\\[\\[\\s*[\\s\\w=><!#/]+\\s*\\]\\]", initCollection[[i]], perl=T)
      #if there are matches for this item, add its position in the list pos
      #the idea is that the list has elements and the elements can be vectors
      #thus, a match may occur for initCollection[[5]][3] if the fifth element of the list is a vector
      #and the match is the third element.
      if (!is.null(matches)) matches$listpos <- i
      initMatches <- rbind(initMatches, matches)
    }
  }

  #successfully creates a data.frame of the sort below.
#   element start end                          tag listpos
#1        1     1  11                  [[classes]]      14
#2        1    19  38         [[groupnames#group]]      14
#3        1    40  63     [[outcomenames#outcome]]      14
#4        1    65  84         [[modelnames#model]]      14
#5        1    85 112 [[zeroclassnames#zeroclass]]      14
#6        1     6  29     [[outcomenames#outcome]]      15
#7        1    31  50         [[groupnames#group]]      15
#8        1    73  92         [[modelnames#model]]      15
#9        1     1   9                    [[hello]]      17
#10       2     1  10                   [[hello2]]      17

  #classify tags in terms of simple, array, iterator, conditional, foreach
  if (!is.null(initMatches) && nrow(initMatches) > 0) {
    initMatches$tagType <- classifyTags(initMatches$tag, initCollection$iterators)

    #chop off the [[ ]] portion of the tags, along with any leading or trailing space
    #this makes it easier to use the sub function to update current values
    initMatches$tag <- sapply(initMatches$tag, function(tag) {
      return(sub("\\[\\[\\s*([\\s\\w=><!#/]+)\\s*\\]\\]", "\\1", tag, perl=TRUE))
    })
  }

  #return empty data frame if no matches
  if (is.null(initMatches)) return(data.frame())
  else return(initMatches)
}

#' Parses tags in the body section
#'
#' Parses tags in the body section (character vector) and
#' init collection (list of vars defined in the init section).
#' This is an internal function.
#'
#' @param bodySection The body
#' @param initCollection The initial collection
#' @return A list with three elements, where each list represents the location,
#' start character, end character, tag type, etc. of each tag.
#' \describe{
#'   \item{initTags}{initMatches}
#'   \item{bodyTags}{bodyMatches}
#'   \item{bodyText}{bodySection}
#' }
#' @keywords internal
parseTags <- function(bodySection, initCollection) {
  #first handle init tags
  initMatches <- getInitTags(initCollection)

  initMatches$currentValue <- NA_character_

  bodyTagRegex <- "\\[\\[\\s*[\\s\\w=><!#/]+\\s*\\]\\]"
  bodyMatches <- friendlyGregexpr(bodyTagRegex, bodySection, perl=TRUE)

  if (is.null(bodyMatches)) stop("No tags found in body section of template file.")

  bodyMatches$tagType <- classifyTags(bodyMatches$tag, initCollection$iterators)
  #okay, now every tag is categorized
  #the notion here is to substitute in the running value for a given variable
  #then we'll do a mass substitute for each model
  bodyMatches$currentValue <- NA_character_

  #chop off the [[ ]] portion of the tags, along with any leading or trailing space
  bodyMatches$tag <- sapply(bodyMatches$tag, function(tag) {
    return(sub("\\[\\[\\s*([\\s\\w=><!#/]+)\\s*\\]\\]", "\\1", tag, perl=TRUE))
  })

  #return a three-element list with constituent data frames for init and body tags.
  return(list(initTags=initMatches, bodyTags=bodyMatches, bodyText=bodySection))
}

#' Create Mplus Input Files from Template
#'
#' The \code{createModels} function processes a single Mplus template file and creates a group of related
#' model input files. Definitions and examples for the template language are provided in the MplusAutomation
#' vignette and are not duplicated here at the moment. See this PDF:
#' \url{http://cran.r-project.org/web/packages/MplusAutomation/vignettes/Vignette.pdf}
#'
#' @param templatefile The filename (absolute or relative path) of an Mplus template file to be processed. Example \dQuote{C:/MplusTemplate.txt}
#' @return No value is returned by this function. It is solely used to process an Mplus template file.
#' @author Michael Hallquist
#' @keywords interface
#' @export
#' @examples
#' \dontrun{
#'   createModels("L2 Multimodel Template No iter.txt")
#' }
createModels <- function(templatefile) {
# should probably have the function cd to wherever the template file is located (if given as abs path)
# todo: allow for direct runs?

  if (!file.exists(templatefile)) stop("Template file not found.")

  readfile <- scan(templatefile, what="character", sep="\n", strip.white=FALSE, blank.lines.skip=FALSE, quiet=TRUE)

  # divide into init versus body
  startinit <- grep("[[init]]", readfile, fixed=T)
  endinit <- grep("[[/init]]", readfile, fixed=T)

  if (length(startinit) != 1 || length(endinit) != 1) {
    stop("Unable to find init section in template file.")
  }

  # extract init section
  initSection <- readfile[(startinit+1):(endinit-1)]

  # extract body section
  bodySection <- readfile[(endinit+1):length(readfile)]

  # convert the init text into a list object containing parsed init instructions
  initCollection <- processInit(initSection)

  templateTags <- parseTags(bodySection, initCollection)

  # lookup values for simple tags, which won't vary by iterator
  templateTags <- lookupSimpleTags(templateTags, initCollection)

  # kick off the recursive replace
  if (length(initCollection$iterators) > 0) {
    recurseReplace(templateTags, initCollection)
  }
}

#' Simple tag lookup
#'
#' The purpose of this function is to set the currentValue column
#' for the bodyTags and initTags data.frames for simple tags only.
#' Most values will be replaced at the bottom level of recursion,
#' but simple tags do not change over iterations, so can be set one time.
#'
#' @param templateTags The template tags
#' @param initCollection The initial collection
#' @return A tag.
#' @keywords internal
lookupSimpleTags <- function(templateTags, initCollection) {
#  #locate simple tags in body
#  simpleBodyPositions <- which(templateTags$bodyTags$tagType=="simple")
#
#  #replace tag with value
#  templateTags$bodyTags$currentValue[simpleBodyPositions] <- sapply(templateTags$bodyTags$tag[simpleBodyPositions],
#      function(value) {
#        currentValue <- eval(parse(text=paste("initCollection$", value, sep="")))
#        if (regexpr("\\[\\[\\s*[\\s\\w=><!#/]+\\s*\\]\\]", currentValue, perl=TRUE) > 0) {
#          #The replacement tag itself contains additional tags.
#          #Thus, not a simple replacement. This replacement needs to be deferred until
#          #we have iterated to the bottom of the tree and have all needed information
#          #set a deferred value to be replace later
#          currentValue <- "..deferred.."
#        }
#        return(currentValue)
#      })

  #locate simple tags in init
  simpleInitPositions <- which(templateTags$initTags$tagType=="simple")

  templateTags$initTags$currentValue[simpleInitPositions] <- sapply(
    templateTags$initTags$tag[simpleInitPositions],
    function(value) {
      return(eval(parse(text=paste0("initCollection$", value))))
    })

  return(templateTags)
}

#' Updates current values
#'
#' Body tags currentValues are substituted at the bottom-most level
#' after init collection is finalized (recursively process any nested tags)
#'
#' @param templateTags The template tags
#' @param initCollection Initial collection
#' @return Updated current value or the original if no match.
#' @keywords internal
updateCurrentValues <- function(templateTags, initCollection) {
#Better idea: only updateCurrentValues for init tags collection
#And only update init collection for the respective iterator
#Only need to update values for a given iterator....
#The issue is that values for a given iterator shouldn't change when another iterator is active

  #need to replace array and iterator tags for this iterator

  #locate iterator tags in init
  initIteratorPositions <- which(
    templateTags$initTags$tagType=="iterator" &
    templateTags$initTags$tag == initCollection$curIteratorName)

  #set the current value to the position in the looping process for this iterator
  templateTags$initTags$currentValue[initIteratorPositions] <- initCollection$curItPos[initCollection$curIteratorDepth]

  #allow for iterator lookups here... just to an is.na check in the replaceBodyTags
  #locate iterator tags in body
  bodyIteratorPositions <- which(
    templateTags$bodyTags$tagType == "iterator" &
    templateTags$bodyTags$tag == initCollection$curIteratorName)

  templateTags$bodyTags$currentValue[bodyIteratorPositions] <- initCollection$curItPos[initCollection$curIteratorDepth]

  # Next, handle array tags
  # figure out the iterator for each array tag and only select
  # those that are relevant to the current iterator
  initArrayPositions <- which(templateTags$initTags$tagType=="array")

  # only update values if any array tags are found
  # (generates an error otherwise because of weird format from splitter_a
  if (length(initArrayPositions) > 0) {

    # use plyr's splitter_a function to divide dataset by row (builds a big list)
    # 20Jul2010: Had to call splitter function directly, ignoring namespace because plyr 1.0 hid this.
    # divideByRow <- plyr:::splitter_a(templateTags$initTags[initArrayPositions,], 1)
    # actually, splitter_a no longer has the same return type (it's now an environment)
    # would have to call row$data$tag... just replace with homespun function defined above.

    divideByRow <- splitDFByRow(templateTags$initTags[initArrayPositions,])


    #for each element of the list, check for a match with this iterator and return the value of interest
    #if the array tag is not for this iterator, return the current value unchanged
    templateTags$initTags$currentValue[initArrayPositions] <- unlist(sapply(divideByRow,
      function(row) {
        split <- strsplit(row$tag, split="#", fixed=TRUE)[[1]]
        if (length(split) != 2) stop("array tag missing iterator: ", row$tag)

        if (split[2] == initCollection$curIteratorName) {
          currentValue <- eval(parse(text =
            paste0("initCollection$", split[1], "[",
            initCollection$curItPos[initCollection$curIteratorDepth], "]")))

          if (is.null(currentValue)) {
            stop("When replacing tag: ", row$tag, ", could not find corresponding value.")
          }
          return(currentValue)
        } else {
          # return unchanged current value if not this iterator
          return(row$currentValue)
        }
      }))
  }

# for now, we don't use any current values for body tags collection (handled at bottom)
#  #conduct same process for body tags: locate array tags and update values for this iterator
#  bodyArrayPositions <- which(templateTags$bodyTags$tagType=="array")
#
#  #use plyr's splitter_a function to divide dataset by row (builds a big list)
#  divideByRow <- splitter_a(templateTags$bodyTags[bodyArrayPositions,], 1)
#
#  #for each element of the list, check for a match with this iterator and return the value of interest
#  templateTags$bodyTags$currentValue[bodyArrayPositions] <- unlist(sapply(divideByRow,
#      function(row) {
#        split <- strsplit(row$tag, split="#", fixed=TRUE)[[1]]
#        if (length(split) != 2) stop("array tag missing iterator: ", row$tag)
#
#        if (split[2] == initCollection$curIteratorName) {
#          currentValue <- eval(parse(text=paste("initCollection$", split[1], "[", initCollection$curItPos[initCollection$curIteratorDepth], "]", sep="")))
#          if (regexpr("\\[\\[\\s*[\\s\\w=><!#/]+\\s*\\]\\]", currentValue, perl=TRUE) > 0) {
#            #The replacement tag itself contains additional tags.
#            #Thus, not a simple replacement. This replacement needs to be deferred until
#            #we have iterated to the bottom of the tree and have all needed information
#            #set a deferred value to be replace later
#            currentValue <- "..deferred.."
#          }
#          if (is.null(currentValue)) stop("When replacing tag: ", row$tag, ", could not find corresponding value.")
#          return(currentValue)
#        }
#        else return(row$currentValue) #return unchanged current value if not this iterator
#      }))

  return(templateTags)
}

#' Recursive replace
#'
#' To do: fill in some details
#'
#' @param templateTags The template tags
#' @param initCollection The list of all arguments parsed from the init section
#' @param curiterator An integer that tracks of the depth of recursion through the iterators. Defaults to 1.
#' @return Does not look like it returns anything
#' @keywords internal
recurseReplace <- function(templateTags, initCollection, curiterator=1L) {
  #bodySection is the character vector representing each line of the body section
  #bodyTags is a data.frame documenting the location and type of all tags in bodySection
  #initTags is a data.frame documenting the location and type of all tags in initCollection

  if (!is.list(initCollection)) {
    stop("Argument list passed to recurseReplace is not a list")
  }

  # check that curiterator is indeed a whole number
  stopifnot(curiterator %% 1 == 0)

  thisIterator <- initCollection$iterators[curiterator]

  #set the current iterator for the collection (used by replaceTags)
  initCollection$curIteratorName <- thisIterator
  initCollection$curIteratorDepth <- curiterator

  #would it work better to use a named array here?
  #like curItVals <- c(1, 3, 5, 2) for iterators a, b, c, d
  #then names(curItVals) <- c("a", "b", "c", "d")

  for (i in initCollection[[thisIterator]]) {

    #set the current position within this iterator for use in replace tags
    #create a vector of iterator positions for use in replaceTags
    #initCollection$curItPos[curiterator] <- i

    #add the iterator name to the vector of iterator positions
    #this has the same effect as above (appending as it recurses), but allows for name-based lookup
    initCollection$curItPos[thisIterator] <- i

    #print(paste("current iterator is:", thisIterator, ", position:", as.character(i)))

    #process foreach commands
    #For now, take this out
    #bodySection <- processForEachTags(bodySection, initCollection)

    #update the current values for this iterator and this iteration
    #this applies for every iterator and iteration, not just processing
    #at the deepest level. The function only updates array and iterator
    #tags that match this iterator, thus minimizing redundant work.
    #the latest is that only init collection tags will be updated
    #then body tags are replaced at the bottom level after init collection is finalized
    templateTags <- updateCurrentValues(templateTags, initCollection)

    if (curiterator < length(initCollection$iterators)) {
      #if not at deepest level, recurse to the next level by adding 1 to the iterator

      #NOTE to self: consider adding a "foreachReplacements" collection to templateTags
      #that contains the expansions of these tags (appended per iteration)
      #this avoids having to think about reparsing the tags based on new code created by foreach

      recurseReplace(templateTags, initCollection, curiterator = curiterator+1)
    } else {
      #we have reached the bottom of the iteration tree
      #simple, array, and iterator tags should be up to date in the templateTags collection

      #first delete conditional tags from the body section, reduce subsequent processing burden
      #need to return templateTags collection from processConditionalTags (including bodyText)

      #need to use a copy of templateTags to avoid it affecting subsequent loop iterations
      finalTemplateTags <- processConditionalTags(templateTags, initCollection)

      #the body section to write is stored in the templateTags collection
      toWrite <- finalTemplateTags$bodyText

      #create a separate initCollection with the appropriate values substituted.
      finalInitCollection <- replaceInitTags(finalTemplateTags$initTags, initCollection)

      #finalize init collection values (in cases of nested tags)
      #wades through init collection for any remaining tags and replaces them
      finalInitCollection <- finalizeInitCollection(finalInitCollection)

      #update bodySection with tag values from finalized init tags
      toWrite <- replaceBodyTags(toWrite, finalTemplateTags$bodyTags, finalInitCollection)

      filename <- finalInitCollection$filename

      print (paste("writing file: ", filename))
      curdir <- getwd()

      #figure out the output directory
      outputDir <- finalInitCollection$outputDirectory

      if (!file.exists(outputDir)) {
        dir.create(outputDir, recursive=TRUE)
      }

      setwd(outputDir)

      #make sure that no line is more than 90 chars
      toWrite <- unlist(lapply(toWrite, function(line) {
        if (nchar(line) > 90) {
          strwrap(line, width=85, exdent=5)
        } else {
          line
        }
      }))

      writeLines(toWrite, con = filename, sep = "\n")

      setwd(curdir)
    }
  }
}

#' Replace Init Tags
#'
#' To do: fill in some details
#'
#' @param initTags Init tags
#' @param initCollection The list of all arguments parsed from the init section
#' @return Returns updated initCollection
#' @keywords internal
replaceInitTags <- function(initTags, initCollection) {
  targetRows <- which(initTags$tagType %in% c("simple", "iterator", "array"))
  targetTags <- initTags[targetRows, ]
  targetTags$rownumber <- 1:nrow(targetTags)

  #going to re-use this chunk in finalizeSubstitutions, so functionalize...
  #consider the looping replacement here
  for (i in 1:nrow(targetTags)) {
    row <- targetTags[i, ]
    stringToChange <- initCollection[[row$listpos]][row$element]

    if(row$start > 1) {
      preTag <- substr(stringToChange, 1, row$start - 1)
    } else {
      preTag <- ""
    }

    if (row$end < nchar(stringToChange)) {
      postTag <- substr(stringToChange, row$end+1, nchar(stringToChange))
    } else {
      postTag <- ""
    }

    initCollection[[row$listpos]][row$element] <- paste0(preTag, row$currentValue, postTag)

    subsequentRows <- which(
      targetTags$rownumber > i &
      targetTags$listpos == row$listpos &
      targetTags$element == row$element)

    if (length(subsequentRows > 0)) {

      #need to offset subsequent start/stops by the difference
      #between the tag and replacement lengths
      diffLength <- nchar(row$currentValue) - (row$end - row$start + 1)

      #update rows in targetTags that have additional tags on the same row
      #need to offset by the diffLength
      targetTags[subsequentRows,"start"] <- targetTags[subsequentRows,"start"] + diffLength
      targetTags[subsequentRows,"end"] <- targetTags[subsequentRows,"end"] + diffLength
    }
  }

  #refresh the initTags collection with the replaced values
  #need to dump the rownumber to align the data.frames
  # (templateTags doesn't have a rownumber field)
  targetTags$rownumber <- NULL
  initTags[targetRows, ] <- targetTags

  #return(initTags)
  #browser()
  return(initCollection)
}

#' Replace Body Tags
#'
#' To do: fill in some details
#'
#' @param bodySection
#' @param bodyTags
#' @param initCollection The list of all arguments parsed from the init section
#' @return Returns updated bodySection
#' @keywords internal
replaceBodyTags <- function(bodySection, bodyTags, initCollection) {
  if (length(bodySection) <= 0) stop("Empty body section")

  #need to ponder issues where a replaced tag still contains another tag

  #hmm, actually seems futile to do a replacement in the init section
  #these are already set by update values.... won't affect the body section

  # so we need to finalize the tag substitutions...
  # the idea is that we need to convert all tags to literals in the initCollection
  # once this is done, then we replace all deferred tags in the body section


  #don't update current values if initcollection value contains any tag
  #if so, replace at the last minute (check this in Init)

  #set a "deferred" status in currentValue if replacement contains tags
  targetTags <- with(bodyTags, bodyTags[tagType %in% c("simple", "iterator", "array"), ])
  targetTags$rownumber <- 1:nrow(targetTags)

  #print(targetTags)
  #stop("test")

  #could improve this by replacing identical tags at once
  #like ddply by the tag

  for (i in 1:nrow(targetTags)) {
    row <- targetTags[i, ]
    stringToChange <- bodySection[row$element]

    if (row$start > 1) {
      preTag <- substr(stringToChange, 1, row$start-1)
    } else {
      preTag <- ""
    }

    if (row$end < nchar(stringToChange)) {
      postTag <- substr(stringToChange, row$end+1, nchar(stringToChange))
    } else {
      postTag <- ""
    }

    #lookup value as needed
    if (is.na(row$currentValue)) {
      row$currentValue <- lookupValue(row$tag, row$tagType, initCollection)
    }
    #row$currentValue <- lookupValue(row$tag, row$tagType, initCollection)

    bodySection[row$element] <- paste0(preTag, row$currentValue, postTag)

    #need to offset subsequent start/stops by the difference between the tag and replacement lengths
    diffLength <- nchar(row$currentValue) - (row$end - row$start + 1)

    subsequentRows <- which(
      targetTags$rownumber > i &
      targetTags$element == row$element)

    if (length(subsequentRows > 0)) {
      #update rows in targetTags that have additional tags on the same row
      #need to offset by the diffLength
      targetTags[subsequentRows,"start"] <- targetTags[subsequentRows,"start"] + diffLength
      targetTags[subsequentRows,"end"] <- targetTags[subsequentRows,"end"] + diffLength
    }
  }

  return(bodySection)
}

#' Lookup values
#'
#' To do: fill in some details
#'
#' @param tag
#' @param tagType
#' @param initCollection The list of all arguments parsed from the init section
#' @return Current value
#' @keywords internal
lookupValue <- function(tag, tagType, initCollection) {
#redundant with finalize code... re-use
  if (missing(tag)) stop("No tag provided")
  if (missing(tagType)) stop("No tag type provided")

  if (tagType == "simple") {
    return(eval(parse(text=paste0("initCollection$", tag))))
  }
  else if (tagType == "array") {
    split <- strsplit(tag, split="#", fixed=TRUE)[[1]]
    if (length(split) != 2) stop("array tag missing iterator: ", row$tag)

    #find where in the iterator depth this iterator lies
    #iteratorPosition <- grep(paste("\\b", split[2], "\\b", sep=""), initCollection$iterators, perl=T)

    #use named array look-up
    iteratorPosition <- initCollection$curItPos[split[2]]

    #note that the padding performed by processInit should handle non-contiguous iteratorPosition values here.
    currentValue <- eval(parse(text=paste0("initCollection$", split[1], "[", iteratorPosition, "]")))

    if (is.null(currentValue)) {
      stop("When replacing tag: ", row$tag, ", could not find corresponding value.")
    }

    return(currentValue)
  }
}

#' Finalize Init Collection
#'
#' this function should handle initTags that still contain tags
#' once the initCollection is finalized, then process the deferred body tags
#' the notion is that the substitutions will be handled in an inefficient manner -- using lots
#' of regular expression parsing, not using the matched tags data.frame
#'
#' we only need to handle simple and array tags
#' iterators should always be integers
#' foreach and conditional are not relevant
#'
#' iterate over init tags until no tags are left
#' here, the init collection should already have had most of its tags substituted by
#' replaceInitTags above.
#'
#' @param initCollection The list of all arguments parsed from the init section
#' @return Finalized initCollection
#' @keywords internal
finalizeInitCollection <- function(initCollection) {
  tagsRemain <- TRUE
  numIterations <- 1
  while(tagsRemain) {
    initTags <- getInitTags(initCollection)

    if (nrow(initTags) == 0) break #if no tags found, then substitution complete

    #update: iterator tags can be nested within other tag types and not updated until here.
    initTags <- with(initTags, initTags[tagType %in% c("simple", "iterator", "array"),])
    if (nrow(initTags) == 0) break #some tags, but none of the simple or array variety, which we want to replace

    #use plyr's splitter_a function to divide dataset by row (builds a big list)
    #divideByRow <- plyr:::splitter_a(initTags, 1)
    divideByRow <- splitDFByRow(initTags)

    #for each element of the list, check for a match with this iterator and return the value of interest
    initTags$currentValue <- unlist(sapply(divideByRow,
      function(row) {
        if (row$tagType == "simple") {
          return(eval(parse(text=paste0("initCollection$", row$tag))))
        }
        else if (row$tagType == "iterator") {
          #an iterator tag was nested
          return(initCollection$curItPos[row$tag])
        }
        else if (row$tagType == "array") {
          split <- strsplit(row$tag, split="#", fixed=TRUE)[[1]]
          if (length(split) != 2) stop("array tag missing iterator: ", row$tag)

          #find where in the iterator depth this iterator lies
          #iteratorPosition <- grep(paste("\\b", split[2], "\\b", sep=""), initCollection$iterators, perl=T)

          #use named array look-up
          iteratorPosition <- initCollection$curItPos[split[2]]

          currentValue <- eval(parse(text=paste0("initCollection$", split[1], "[", iteratorPosition, "]")))

          if (is.null(currentValue)) {
            stop("When replacing tag: ", row$tag, ", could not find corresponding value.")
          }

          return(currentValue)
        }
      }
    ))

    #now we have a list of curent values for any init tags
    #and we want to update the init collection with their values... just as with above.
    initCollection <- replaceInitTags(initTags, initCollection)

    numIterations <- numIterations + 1
    if (numIterations > 20) stop("While replacing tags in init section, looped over variables 20 times without completing substitutions.\n  Check for circular definitions within init section.")
  }

  #browser()
  return(initCollection)
}

#' Evaluate Conditional
#'
#' Note that at thie point the comparator must be a number (not another variable).
#'
#' @param tag A tag
#' @param initCollection The list of all arguments parsed from the init section
#' @return A boolean value indicating whether the conditional is true
#' @keywords internal
evaluateConditional <- function(tag, initCollection) {
  #evaluate whether tag is true
  #first divide up into name, operator, and value
  regexp <- "(\\w+)\\s*([!><=]+)\\s*(\\w+)"
  conditional <- unlist(strapply(tag, regexp, c))

  if (length(conditional) < 3) {
    stop("Error in conditional tag: does not contain variable, operator, and value. Tag = ", tag)
  }

  #convert simple equals to logical equals
  if (conditional[2] == "=") conditional[2] <- "=="

  #obsolete b/c using named array
  #iteratorPosition <- grep(paste("\\b", conditional[1], "\\b", sep=""), initCollection$iterators, perl=T)

  #return a boolean value indicating whether the conditional is true
  return(eval(parse(text=paste0("initCollection$curItPos[conditional[1]]", conditional[2], conditional[3]))))
}

#' Clip String
#'
#' To do: add any details.
#'
#' @param string A string to be clipped
#' @param start The character position to start at
#' @param end  The character position to end at
#' @return A string from start to end
#' @keywords internal
clipString <- function(string, start, end) {
  #if the string is shorter than the length of the clip, then nothing remains
  if (nchar(string) <= end-start+1) return("")

  if(start > 1) preString <- substr(string, 1, start-1)
  else preString <- ""

  if(end < nchar(string)) postString <- substr(string, end+1, nchar(string))
  else postString <- ""

  return(paste0(preString, postString))
}

#' Process Conditional Tags
#'
#' To do: add details.
#'
#' @param templateTags A template tag
#' @param initCollection The list of all arguments parsed from the init section
#' @return Processed templateTags
#' @keywords internal
processConditionalTags <- function(templateTags, initCollection) {
  #require(gsubfn) #moving to import strategy
  #find all conditional tags in the body section and remove them from the templateTags and bodyText pieces...

  conditionalTagIndices <- which(templateTags$bodyTags$tagType=="conditional")

	#return templateTags unharmed if there are no conditional tags (creates error below otherwise)
	if (length(conditionalTagIndices) == 0) return(templateTags)

  openClose <- ifelse(substr(templateTags$bodyTags$tag[conditionalTagIndices], 1, 1)=="/", "close", "open")
  allOpen <- conditionalTagIndices[openClose=="open"]

  bodyTagsToDrop <- c()
  bodyLinesToDrop <- c()
  for (i in allOpen) {
    #should be able to decide whether to skip an iteration if the affected lines are already in bodyLinesToDrop
    thisTag <- templateTags$bodyTags$tag[i]

    #evaluate truth of conditional
    conditionalTrue <- evaluateConditional(thisTag, initCollection)

    #only look for closing tags after the opening and accept the first exact match
    close <- conditionalTagIndices[
      templateTags$bodyTags$tag[conditionalTagIndices] == paste0("/", thisTag) &
      templateTags$bodyTags$element[conditionalTagIndices] >= templateTags$bodyTags$element[i]][1]

    sameLine <- FALSE
    #in case of same line match, check to make sure close follows opening on that line
    #the conditions above could match when a closing tag precedes opening tag on the same line
    if (templateTags$bodyTags$element[close]==templateTags$bodyTags$element[i]) {
      sameLine <- TRUE

      close <- conditionalTagIndices[
        openClose == "close" &
        templateTags$bodyTags$tag[conditionalTagIndices] == paste0("/", thisTag) &
        templateTags$bodyTags$element[conditionalTagIndices] == templateTags$bodyTags$element[i] &
        templateTags$bodyTags$start[conditionalTagIndices] > templateTags$bodyTags$end[i]][1]

      if (!close > 0) stop("Could not find closing tag for conditional:", thisTag)
    }

    #skip this iteration if the opening and closing tags in question are already in the drop pile
    #these lines (and the lines between, if necessary) will already be dropped, so don't process
    if (templateTags$bodyTags$element[i] %in% bodyLinesToDrop &&
        templateTags$bodyTags$element[close] %in% bodyLinesToDrop) next

    #first check for tags to drop from the bodyTags collection (don't want these parsed later)
    if (conditionalTrue) {
      #only remove starting and ending tags
      bodyTagsToDrop <- c(bodyTagsToDrop, i, close)
    } else {
      #if conditional false, then remove all tags between conditional tags
      #first, dump all lines in the bodyTags section that fall between elements
      bodyTagsToDrop <- c(bodyTagsToDrop, i:close)

      #conditional is not true
      #so dump the tags and all space between
      #really, the only difference here from the calculation below is that
      #bodyLinesToDrop should encompass the space between opening and closing
      #and the clips below should dump the rest of the line when multiple tags on same line
      #no need to rewrite code for clipping out tags
      #don't clip the tag lines themselves because this is handled below (whole line goes if nchar <= 0)
      #print(bodyLinesToDrop)
      #browser()

      #only drop lines between matching open/close tags if not on the same line
      #otherwise, the clipping code below handles everything correctly
      #if on the same line, then element + 1:close - 1 will lead to something like 58:56, which is bad
      if (!sameLine) {
        bodyLinesToDrop <- c(bodyLinesToDrop,
         (templateTags$bodyTags$element[i]+1):(templateTags$bodyTags$element[close]-1))
      }
    }

    #then dump lines from the syntax section itself
    #handle same line issues, then delete whole lines between tags
    #as with replaceTags substitution, need to handle situation where tag is on line with other stuff
    #thus, need to update bodyTags collection, too to reflect new start/stop positions

    #when the conditional is true, just remove the tags and leave the syntax
    #dump the opening tag on the line

    #if the conditional is true, just use the last pos of the opening tag for the clip
    if (conditionalTrue) endPos <- templateTags$bodyTags$end[i]
    #want to clip the rest of the line
    else if (!conditionalTrue && sameLine == FALSE) endPos <- nchar(templateTags$bodyText[templateTags$bodyTags$element[i]])
    #just clip anything between open tag and first element of close tag (close tag itself handled by code below)
    else if (!conditionalTrue && sameLine == TRUE) endPos <- templateTags$bodyTags$start[close] - 1

    templateTags$bodyText[templateTags$bodyTags$element[i]] <- clipString(
      templateTags$bodyText[templateTags$bodyTags$element[i]],
      templateTags$bodyTags$start[i], endPos)

    if (nchar(trimSpace(templateTags$bodyText[templateTags$bodyTags$element[i]])) <= 0) {
      #no characters remain, so dump line
      bodyLinesToDrop <- c(bodyLinesToDrop, templateTags$bodyTags$element[i])
    } else {
      #if there is other text on this line, it may contain tags that need to be adjusted given the clip
      subsequentTags <- which(
        templateTags$bodyTags$element == templateTags$bodyTags$element[i] &
        templateTags$bodyTags$start > endPos)

      if (length(subsequentTags > 0)) {
        #calculate length of opening tag
	openLength <- endPos - templateTags$bodyTags$start[i] + 1
        templateTags$bodyTags[subsequentTags,"start"] <- templateTags$bodyTags[subsequentTags,"start"] - openLength
        templateTags$bodyTags[subsequentTags,"end"] <- templateTags$bodyTags[subsequentTags,"end"] - openLength
	#print("openlength")
	#browser()
      }
    }

    #okay, we've handled issues related to the opening tag, now handle closing tag
    #for the closing tag, just need to clip the tag itself (spacing handled above)
    templateTags$bodyText[templateTags$bodyTags$element[close]] <- clipString(
      templateTags$bodyText[templateTags$bodyTags$element[close]],
      templateTags$bodyTags$start[close],
      templateTags$bodyTags$end[close])

    if (nchar(trimSpace(templateTags$bodyText[templateTags$bodyTags$element[close]])) <= 0) {
      #no characters remain, so dump line
      bodyLinesToDrop <- c(bodyLinesToDrop, templateTags$bodyTags$element[close])
    } else {
      #only look for additional tags if nchar > 0
      #redundant code with above... must be a way to consolidate
      #if there is other text on then end line, it may contain tags that need to be adjusted given the clip
      subsequentTags <- which(
        templateTags$bodyTags$element == templateTags$bodyTags$element[close] &
        templateTags$bodyTags$start > templateTags$bodyTags$end[close])

      if (length(subsequentTags > 0)) {
        closeLength <- templateTags$bodyTags$end[close] - templateTags$bodyTags$start[close] + 1
        templateTags$bodyTags[subsequentTags,"start"] <- templateTags$bodyTags[subsequentTags,"start"] - closeLength
        templateTags$bodyTags[subsequentTags,"end"] <- templateTags$bodyTags[subsequentTags,"end"] - closeLength
	#print("closelength")
	#browser()
      }
    }
  }


  #print(bodyLinesToDrop)
  #print(bodyTagsToDrop)

  #drop all bad body lines

  #only keep unique bodyTagsToDrop (and sort for clarity in debugging)
  #hard to imagine that bodyTagsToDrop could be NULL at this point (given the return when no conditional tags above)
  #but if it were NULL, the bodyTags collection would be dumped by the NULL*-1 evaluation

  if (!is.null(bodyTagsToDrop)) {
    bodyTagsToDrop <- sort(unique(bodyTagsToDrop))
    templateTags$bodyTags <- templateTags$bodyTags[bodyTagsToDrop*-1, ]
  }

  #need to check whether bodyLinesToDrop is NULL. If it is, then we must not attempt the subset
  #(it will delete the whole character vector)
  if (!is.null(bodyLinesToDrop)) {
    #only retain unique bodyLinesToDrop (in theory handled by the "next" code above, but good to be safe)
    bodyLinesToDrop <- sort(unique(bodyLinesToDrop))
    templateTags$bodyText <- templateTags$bodyText[bodyLinesToDrop*-1]

    #need to move up the line markers in the bodyTags collection based on the lines dropped
    templateTags$bodyTags <- ddply(templateTags$bodyTags, "element", function(subDF) {
      numMoveUp <- length(which(bodyLinesToDrop < subDF$element[1]))
      subDF$element <- subDF$element - numMoveUp
      return(subDF)
    })
  }
  return(templateTags)
}


#' Process the Init Section
#'
#' To do: add details.
#'
#' @param initsection The list of all arguments parsed from the init section
#' @return arglist
#' @importFrom gsubfn strapply
#' @keywords internal
processInit <- function(initsection) {

  #combine multi-line statements by searching for semi-colon
  assignments <- grep("^\\s*.+\\s*=", initsection, perl=TRUE)

  #check for valid variable names
  valid <- grep("^\\s*[A-Za-z\\.]+[\\w\\.#]*\\s*=", initsection[assignments], perl=TRUE)

  if (length(valid) < length(assignments)) {
    badvars <- initsection[assignments[which(!1:length(assignments) %in% valid)]]
    stop(paste(c("Invalid variable definitions in init section.",
      "Variables must begin with a letter or a period.",
      "Variables may contain only the following characters: letters, numbers, underscores, periods, and a single pound sign for list variables.",
      "Problematic variable(s):", badvars), collapse="\n  "))
  }

  #preallocate vector of strings to process
  argstoprocess <- vector("character", length(assignments))

  #loop through each line containing an assignment
  for (i in 1:length(assignments)) {
    argstoprocess[i] = initsection[assignments[i]]

    #if line does not terminate in semicolon, then read subsequent lines until semicolon found
    #start file position at n+1 line
    filepos = assignments[i] + 1
    while (length(grep(";\\s*$", argstoprocess[i], perl=TRUE)) != 1) {
      #cat("multi-line: ", unlist(argstoprocess[i]), fill=T)
      argstoprocess[i] = paste(argstoprocess[i], initsection[filepos])
      filepos = filepos + 1
    }
  }

  #will return a list (one element per argstoprocess) with a three-element vector (name, iterator, value)
  #note that the regexp implicitly dumps the semicolon and any trailing spaces
  arglist <- strapply(argstoprocess, "^\\s*(\\w+[\\w\\.]*)(#[\\w\\.]+)?\\s*=\\s*(.+);\\s*$",
    function(name, iterator, value) {
      return(c(name, iterator, value))
    }, perl=TRUE)

   #copy the first element (name) of each vector into the list names
   names(arglist) <- make.names(sapply(arglist, '[', 1))

  #1. parse values into vectors according to spaces and quotes
  #2. add iterator attribute to be processed after iterators are setup below
  #3. implicitly drop name by not including element[1]
  arglist <- lapply(arglist, function(element) {
    output <- friendlyGregexpr("(\"[^\"]*\"|[^\\s]+)", element[3])$tag
    output <- gsub("\"", "", output)

    #the regexp above matches the # itself.
    #need to trim off in cases where iterator defined
    if (nchar(element[2]) > 0) {
      element[2] <- substr(element[2], 2, nchar(element[2]))
    }
    attr(output, "iterator") <- element[2]

    return(output)
  })


  if (is.null(arglist$iterators)) {
    stop("No iterators in init section. Cannot process template.")
  }

  #convert iterators from string to list
  #arglist$iterators <- unlist(strsplit(as.character(arglist$iterate_wrt), "\\s*,\\s*", perl=T))

  #process sequence text for each iterator
  for (thisIt in arglist$iterators) {
    if (is.null(arglist[[thisIt]])) {
      stop("Variable specified in iterators list, but not defined: ", thisIt)
    }

    #expand colon notation as needed
    #use do.call to combine elements of list returned by lapply
    #if there are many elements (e.g., 1 3 5), then lapply returns an element for each
    #one, but we just want a combined array. In the case of colon expansion, want to c that together
    #with any other elements... Maybe in the future when we support non-contiguous iterators.

    arglist[[thisIt]] <- do.call("c", lapply(arglist[[thisIt]], function(x) {
      if (length(grep(":", x)) > 0) {
        return(strapply(x, "(\\d+)\\s*:\\s*(\\d+)", function(start, stop) return(start:stop))[[1]])
      } else {
        return(as.numeric(x))
      }
     }))

    #sort as ascending and only keep unique values
    if (length(unique(arglist[[thisIt]])) < length(arglist[[thisIt]])) {
      stop("Problem with iterator: ", thisIt, "\n  Non-unique values specified: ",
        paste(arglist[[thisIt]], collapse=", "))
    }

    arglist[[thisIt]] <- sort(unique(arglist[[thisIt]]))
  }

  #now that iterators are defined, ensure that list tags match
  #pad vectors accordingly

  arglist <- lapply(arglist, function(element) {
    #if the iterator is defined, then this is a list tag
    #need to make sure it is properly padded
    iteratorAttr <- attr(element, "iterator")
    if (!is.null(iteratorAttr) && nchar(iteratorAttr) > 0) {
      iteratorValues <- arglist[[iteratorAttr]]

      #make sure that the length of the values vector
      #matches the length of the iterator vector
      if (length(element) != length(iteratorValues)) {
        stop("Variable locked to iterator: ", iteratorAttr,
          ", but has different length.\n  Values: ",
          paste(element, collapse=", "),
          "\n  Should be length: ", length(iteratorValues))
      }

      if (length(element) < max(iteratorValues)) {
        #pad
        updatedElement <- c()
        listElement <- 1
        #build a vector of the same length as the max of the iterator
        #only insert list values for defined indices. Otherwise pad
        for (i in 1:max(iteratorValues)) {
          if (i %in% iteratorValues) {
            updatedElement[i] <- element[listElement]
            listElement <- listElement + 1
          } else {
            updatedElement[i] <- ""
          }
        }
        element <- updatedElement
        attr(element, "iterator") <- iteratorAttr #re-add attribute
      }
    }

    return(element)
  })

  #browser()


  #default output directory to the current directory
  if (is.null(arglist$outputDirectory)) {
    warning("No output directory specified. Defaulting to the current directory.")
    arglist$outputDirectory <- getwd()
  }
  if (is.null(arglist$filename)) {
    stop("No definition provided for the output filename. The filename definition is required.")
  }

  return(arglist)
}


#' Create Mplus syntax for variable names
#'
#' This is a simple function designed to take a dataset in \code{R}
#' and translate it into a set of variable names for Mplus.
#'
#' @param data An \code{R} dataset.
#' @return A character vector of the variable names for Mplus
#' @keywords internal
#' @seealso \code{\link{prepareMplusData}}
#' @examples
#' MplusAutomation:::createVarSyntax(mtcars)
createVarSyntax <- function(data) {
  #variable created for readability
  variableNames <- paste(gsub("\\.", "_", names(data)), collapse=" ")

  #short names?
  #shortNames <- paste(substr(names(df), 1, 8), collapse=" ")

  vnames <- paste(strwrap(paste(c("NAMES = ", variableNames, ";"), collapse = ""),
    width=85, exdent=5), collapse="\n")
  vnames[length(vnames)] <- paste(vnames[length(vnames)], "\n", collapse="")

  return(vnames)
}

#' Prepare Mplus Data Matrix
#'
#' support writing of covariance or means + covariance matrix (future)
#'
#' @param covMatrix The covariance matrix
#' @param meansMatrix The means matrix
#' @param nobs Number of observations for the data
#' @return A dataset
#' @keywords internal
prepareMplusData_Mat <- function(covMatrix, meansMatrix, nobs) {

}


#' Create tab-delimited file and Mplus input syntax from R data.frame
#'
#' The \code{prepareMplusData} function converts an R data.frame object
#' into a tab-delimited file (without header) to be used in an Mplus
#' input file. The corresponding Mplus syntax, including the
#' data file definition and variable names,
#' is printed to the console or optionally to an input file.
#'
#' @param df The R data.frame to be prepared for Mplus
#' @param filename The path and filename for the tab-delimited data file
#'   for use with Mplus. Example: "C:/Mplusdata/data1.dat"
#' @param keepCols A character vector specifying the variable names
#'   within \code{df} to be output to \code{filename} or a numeric
#'   vector of the column indices to be output or a logical vector
#'   corresponding to the same.
#' @param dropCols A character vector specifying the variable names
#'   within \code{df} to be omitted from the data output to \code{filename}
#'   or a numeric vector of the column indices not to be output
#'   or a logical vector corresponding to the same.
#' @param inpfile Logical value whether the Mplus syntax should be written
#'   to the console or to an input file. Defaults to \code{FALSE}. If
#'   \code{TRUE}, the file name will be the same as \code{filename} with
#'   the extension changed to .inp.  Alternately, this can be a character
#'   string giving the file name to write the Mplus syntax to.
#' @param interactive Logical value indicating whether file names
#'   should be selected interactively. If \code{filename} is
#'   missing and \code{interative=TRUE}, then a dialogue box
#'   will pop up to select a file or a console prompt if in a
#'   non interactive context. Defaults to \code{TRUE}.
#' @param overwrite Logical value indicating whether
#'   data and input (if present) files should be overwritten.
#'   Defaults to \code{TRUE} to be consistent with prior behavior.
#'   If \code{FALSE} and the file to write the data to already exists,
#'   it will throw an error.
#' @return Invisibly returns a character vector of the Mplus input
#'   syntax. Primarily called for its side effect of creating Mplus
#'   data files and optionally input files.
#' @keywords interface
#' @author Michael Hallquist
#' @export
#' @examples
#' \dontrun{
#' library(foreign)
#'
#' study5 <- read.spss("reanalysis-study-5-mt-fall-08.sav", to.data.frame=TRUE)
#' ASData5 <- subset(study5, select=c("ppnum", paste("as", 1:33, sep="")))
#'
#' prepareMplusData(ASData5, "study5.dat")
#'
#'
#' # basic example
#' test01 <- prepareMplusData(mtcars, "test01.dat")
#'
#' # see that syntax was stored
#' test01
#'
#' # tests for keeping and dropping variables
#' prepareMplusData(mtcars, "test02.dat", keepCols = c("mpg", "hp"))
#' prepareMplusData(mtcars, "test03.dat", keepCols = c(1, 2))
#' prepareMplusData(mtcars, "test04.dat",
#'   keepCols = c(TRUE, FALSE, FALSE, TRUE, FALSE,
#'   FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
#'
#' prepareMplusData(mtcars, "test05.dat", dropCols = c("mpg", "hp"))
#' prepareMplusData(mtcars, "test06.dat", dropCols = c(1, 2))
#' prepareMplusData(mtcars, "test07.dat",
#'   dropCols = c(TRUE, FALSE, FALSE, TRUE, FALSE,
#'   FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
#'
#'
#' # interactive (test08.dat)
#' prepareMplusData(mtcars, interactive=TRUE)
#'
#' # write syntax to input file, not stdout
#' prepareMplusData(mtcars, "test09.dat", inpfile=TRUE)
#'
#' # write syntax to alternate input file, not stdout
#' prepareMplusData(mtcars, "test10.dat", inpfile="test10alt.inp")
#'
#' # should be error, no file
#' prepareMplusData(mtcars, interactive=FALSE)
#'
#' # new warnings if it is going to overwrite files
#' # (the default to be consistent with prior behavior)
#' prepareMplusData(mtcars, "test10.dat")
#'
#' # new warnings if it is going to overwrite files
#' # (the default to be consistent with prior behavior)
#' prepareMplusData(mtcars, "test11.dat", inpfile="test10alt.inp")
#'
#' # new errors if files exist and overwrite=FALSE
#' prepareMplusData(mtcars, "test10.dat",
#'   inpfile="test10alt.inp", overwrite=FALSE)
#' }

prepareMplusData <- function(df, filename, keepCols, dropCols, inpfile=FALSE,
  interactive=TRUE, overwrite=TRUE) {

  stopifnot(inherits(df, "data.frame"))

  #only allow keep OR drop.
  if(!missing(keepCols) && !missing(dropCols)) {
    stop("keepCols and dropCols passed to prepareMplusData. You must choose one or the other, but not both.")
  }

  # assert types allowed for keep and drop cols
  stopifnot(missing(keepCols) || is.character(keepCols) ||
    is.numeric(keepCols) || is.logical(keepCols))

  stopifnot(missing(dropCols) || is.character(dropCols) ||
    is.numeric(dropCols) || is.logical(dropCols))

  # if filename is missing and interactive is TRUE
  # interactively (through GUI or console)
  # request filename from user
  if (missing(filename) && interactive) {
    filename <- file.choose()
  }

  # if filename is still missing at this point
  # throw an error
  stopifnot(!missing(filename))

  #keep only columns specified by keepCols
  if (!missing(keepCols) && length(keepCols) > 0) {
    df <- df[, keepCols] # works with all types
  }
  
  #drop columns specified by dropCols
  if (!missing(dropCols) && length(dropCols) > 0) {
    if (is.character(dropCols)) {
      df <- subset(df, select = -which(colnames(df) %in% dropCols))
    } else if (is.numeric(dropCols)) {
      df <- subset(df, select = -dropCols)
    } else if (is.logical(dropCols)) {
      df <- subset(df, select = !dropCols)
    }
  }
  
  #convert factors to numbers
  df <- as.data.frame(qv <- lapply(1:ncol(df), function(col) {
            #can't use ifelse because is.factor returns only one element,
            #and ifelse enforces identical length
            if (is.factor(df[,col])) {
              # numeric storage of the levels corresponds to the order of the levels
              cat("Factor variable:", names(df)[col], "; factor levels:", paste(levels(df[,col]), collapse=", "), "converted to numbers:",
                  paste(seq_along(levels(df[,col])), collapse=", "), "\n\n")
              col_value <- as.numeric(df[,col])
            } else {
              col_value <- df[,col]
            }
            
            if (is.character(df[,col])) {
              warning("Character data requested for output using prepareMplusData.\n  Mplus does not support character data.")
            }
            
            col_value <- list(col_value)
            names(col_value) <- names(df)[col]
            return(col_value)
          }))
  
  
  if (file.exists(filename)) {
    if (overwrite) {
      warning(paste("The file", sQuote(basename(filename)),
        "currently exists and will be overwritten"))
    } else {
      stop(paste("The file", sQuote(basename(filename)),
        "currently exists. Specify a different filename or set overwrite=TRUE"))
    }
  }

  write.table(df, filename, sep = "\t", col.names = FALSE, row.names = FALSE, na=".")

  syntax <- c(
    "TITLE: Your title goes here\n",
    DATA <- paste0("DATA: FILE = \"", filename, "\";\n"),
    "VARIABLE: \n", createVarSyntax(df), "MISSING=.;\n")

  # if inpfile is a logical value and is TRUE
  # then create the file using filename
  # changing the extension to .inp
  if (is.logical(inpfile) && inpfile) {
    inpfile <- gsub("(.*)\\..*$", "\\1.inp", filename)
  }

  # if the input file is not a character
  # either by user specification or automatically
  # by replacing extension of filename with .inp
  # then just use stdout
  if (!is.character(inpfile)) {
    inpfile <- stdout()
  }

  if (is.character(inpfile) && file.exists(inpfile)) {
    if (overwrite) {
      warning(paste("The file", sQuote(basename(inpfile)),
        "currently exists and will be overwritten"))
    } else {
      stop(paste("The file", sQuote(basename(inpfile)),
        "currently exists. Specify a different filename or set overwrite=TRUE"))
    }
  }

  # write out syntax, either to stdout or to a file
  cat(syntax, file=inpfile, sep="")

  # return invisible so it can be saved/reused if desired
  return(invisible(syntax))
}
