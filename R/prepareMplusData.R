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
  variableNames <- paste(gsub("\\.", "_", colnames(data)), collapse=" ")
  
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


#' Clean data and calculate the md5 hash
#'
#' Internal utility function, primarily for \code{prepareMplusData}.
#'
#' @param df The R data.frame to be prepared for Mplus
#' @param keepCols A character vector specifying the variable names
#'   within \code{df} to be output to \code{filename} or a numeric
#'   vector of the column indices to be output or a logical vector
#'   corresponding to the same.
#' @param dropCols A character vector specifying the variable names
#'   within \code{df} to be omitted from the data output to \code{filename}
#'   or a numeric vector of the column indices not to be output
#'   or a logical vector corresponding to the same.
#' @param imputed A logical whether data are multiply imputed.  Defaults
#'   to \code{FALSE}.  If \code{TRUE}, the data should be a list,
#'   where each element of the list is a multiply imputed dataset.
#' @return A list of the data and the md5 hash.
#' @keywords internal
#' @importFrom digest digest
#' @importFrom checkmate assert_data_frame assert_logical assert_character
#' @rdname MplusAutomationUtils
#' @examples
#' \dontrun{
#'
#' ## basic example
#' MplusAutomation:::.cleanHashData(mtcars)
#'
#' ## has changes when data changes
#' MplusAutomation:::.cleanHashData(mtcars[-15,])
#'
#' ## example on a list (e.g., for multiply imputed data)
#'
#' MplusAutomation:::.cleanHashData(
#'  list(
#'    data.frame(a = 1:4),
#'    data.frame(a = c(2, 2, 3, 4))),
#'   imputed = TRUE)
#'
#' }
.cleanHashData <- function(df, keepCols=NULL, dropCols=NULL, imputed=FALSE) {
  if(!imputed) checkmate::assert_data_frame(df)
  #checkmate::assert_character(keepCols, null.ok = TRUE, all.missing = FALSE)
  #checkmate::assert_character(dropCols, null.ok = TRUE, all.missing = FALSE)
  checkmate::assert_logical(imputed, null.ok = FALSE)
  
  if (isTRUE(imputed)) {
    stopifnot(inherits(df, "list"))
  } else {
    stopifnot(inherits(df, "data.frame"))
  }
  
  ## only allow keep OR drop.
  if(!is.null(keepCols) && !is.null(dropCols)) {
    stop("keepCols and dropCols passed. You must choose one or the other, but not both.")
  }
  
  ## assert types allowed for keep and drop cols
  stopifnot(is.null(keepCols) || is.character(keepCols) ||
              is.numeric(keepCols) || is.logical(keepCols))
  
  stopifnot(is.null(dropCols) || is.character(dropCols) ||
              is.numeric(dropCols) || is.logical(dropCols))
  
  
  ## keep only columns specified by keepCols
  if (!is.null(keepCols) && length(keepCols) > 0) {
    if (imputed) {
      df <- lapply(df, function(d) d[, keepCols, drop = FALSE])
    } else {
      df <- df[, keepCols, drop = FALSE] # works with all types
    }
  }
  
  ## drop columns specified by dropCols
  if (!is.null(dropCols) && length(dropCols) > 0) {
    if (is.character(dropCols)) {
      if (imputed) {
        df <- lapply(df, function(d) {subset(d, select = -which(colnames(d) %in% dropCols))})
      } else {
        df <- subset(df, select = -which(colnames(df) %in% dropCols))
      }
    } else if (is.numeric(dropCols)) {
      if (imputed) {
        df <- lapply(df, function(d) {subset(d, select = -dropCols)})
      } else {
        df <- subset(df, select = -dropCols)
      }
    } else if (is.logical(dropCols)) {
      if (imputed) {
        df <- lapply(df, function(d) {subset(d, select = !dropCols)})
      } else {
        df <- subset(df, select = !dropCols)
      }
    }
  }
  
  f <- function(x) {
    as.vector(c(
      dim(x),
      unlist(lapply(x, class)),
      unlist(dimnames(x)),
      as.character(unlist(x[c(1, nrow(x)), ]))))
  }
  
  if (imputed) {
    hash <- lapply(seq_along(df), function(i) digest(f(df[[i]]), "md5"))
  } else {
    hash <- digest(f(df), "md5")
  }
  
  return(list(data = df, md5 = hash))
}

#' Check if a file exists with a given hash and add a hash to an existing filename
#'
#' Internal utility function, primarily for \code{prepareMplusData}.
#'
#' @param filename A character vector containing the filename
#' @param hash A character vector with the hash to use
#' @param useexisting A logical whether to use an existing file name
#'   if one is found containing the hash.  Defaults to \code{FALSE}
#'   in which case the hash is added to the user specified filename
#' @return A list of the filename (plus hash) and a logical value
#'   whether a filename with the hash already existed or not.
#' @keywords internal
#' @rdname MplusAutomationUtils
#' @examples
#' MplusAutomation:::.hashifyFile("testit.dat", "abc")
.hashifyFile <- function(filename, hash, useexisting = FALSE) {
  fileonly <- basename(filename)
  allfiles <- list.files(path = dirname(filename))
  existingfile <- grep(hash, allfiles, value=TRUE)[1]
  
  fileexists <- length(existingfile) && !isTRUE(is.na(existingfile))
  
  if (fileexists && useexisting) {
    filename <- gsub(basename(filename), basename(existingfile), filename)
  } else {
    filename <- gsub("\\.dat$", paste0("_", hash, ".dat"), filename)
  }
  list(filename = filename, fileexists = fileexists)
}


#' Convert a matrix or data frame to numeric or integer for Mplus
#'
#' Primarily an internal utility function, for \code{prepareMplusData}.
#'
#' @param df A matrix or data frame
#' @return An error if it cannot be converted or
#'   a matrix or data frame with all variables converted to
#'   numeric or integer classes
#' @importFrom data.table is.data.table
#' @importFrom checkmate assert_subset
#' @importFrom fastDummies dummy_cols
#' @keywords internal
#' @examples
#'
#' \dontrun{
#' df1 <- df2 <- df3 <- df4 <- mtcars
#'
#' df2$cyl <- factor(df2$cyl)
#' df2$am <- as.logical(df2$am)
#'
#' df3$mpg <- as.character(df3$mpg)
#'
#' df4$vs <- as.Date(df4$vs, origin = "1989-01-01")
#'
#' df5 <- as.matrix(cars)
#'
#' df6 <- matrix(c(TRUE, TRUE, FALSE, FALSE), ncol = 2)
#'
#' df7 <- as.list(mtcars)
#'
#'
#' MplusAutomation:::.convertData(df1)
#'
#' MplusAutomation:::.convertData(df2)
#'
#' MplusAutomation:::.convertData(df3)
#'
#' MplusAutomation:::.convertData(df4)
#'
#' MplusAutomation:::.convertData(df5)
#'
#' MplusAutomation:::.convertData(df6)
#'
#' MplusAutomation:::.convertData(df7)
#'
#' rm(df1, df2, df3, df4, df5, df6, df7)
#' }
.convertData <- function(df, dummyCode=NULL) {
  checkmate::assert_subset(dummyCode, names(df), empty.ok = TRUE)
  
  if (isTRUE(is.matrix(df))) {
    if (isTRUE(is.numeric(df)) || isTRUE(is.integer(df))) {
      NULL ## do nothing
    } else if (isTRUE(is.logical(df))) {
      message("Logical matrix converted to integer")
      storage.mode(df) <- "integer"
    } else {
      stop(paste(
        "\nIf data are passed as a matrix, must be of class",
        "numeric, integer, or logical, but data was of class",
        class(df[,1]),
        sep = "\n"))
    }
    
    if (isTRUE(is.null(colnames(df)[1]))) {
      message("no variable names, setting to V1, V2, etc")
      colnames(df) <- paste0("V", 1:ncol(df))
    }
  } else if (isTRUE(is.data.frame(df))) {
    if (isTRUE(is.data.table(df))) {
      df <- as.data.frame(df)
    }
    
    col_logical <- vapply(df, is.logical, FUN.VALUE = NA)
    col_numeric <- vapply(df, is.numeric, FUN.VALUE = NA) | vapply(df, is.integer, FUN.VALUE = NA)
    col_factor <- vapply(df, is.factor, FUN.VALUE = NA)
    col_character <- vapply(df, is.character, FUN.VALUE = NA)
    
    col_class <- vapply(df, class, FUN.VALUE = NA_character_)
    
    ok_cols <- col_logical | col_numeric | col_factor | col_character
    
    categorical_vars <- c(dummyCode, names(df)[col_character | col_factor])
    
    if (!all(ok_cols)) {
      stop(paste("\nCurrently only variables of class: ",
                 "numeric, integer, logical, character, or factor",
                 "are allowed but found additional class types including: ",
                 paste(unique(col_class[!ok_cols]), collapse = ", "),
                 "\nto see which variables are problematic, try:",
                 "str(yourdata)",
                 sep = "\n"))
    }
    
    if (length(categorical_vars) > 0L) {
      #First handle dummy codes
      if (!is.null(dummyCode)) {
        message("Converting the following variables to dummy codes: ", paste(dummyCode, collapse=", "))
        df <- fastDummies::dummy_cols(df, select_columns=dummyCode, remove_selected_columns = TRUE)
        categorical_vars <- setdiff(categorical_vars, dummyCode) #drop off dummy columns
      }
      
      for (thisVar in categorical_vars) {
        if (!is.factor(df[[thisVar]])) { df[[thisVar]] <- factor(df[[thisVar]]) } #convert character to factor for ease
        
        cat("\n-----\nFactor:", thisVar, "\n")
        cat("Conversion:\n")
        conv_df <- data.frame(level = levels(df[[thisVar]]),
                              number = seq_along(levels(df[[thisVar]])))
        print(conv_df, row.names=FALSE)
        df[[thisVar]] <- as.numeric(df[[thisVar]]) #convert to number
        cat("-----\n\n")
      }
    }
    
    logical_cols <- which(col_logical)
    if (isTRUE(length(logical_cols) > 0)) {
      message("Logical variables will be converted to numbers in Mplus data file")
      for (i in logical_cols) {
        message("Logical variable: ", names(df)[i], " converted to 0/1 integer")
        df[[i]] <- as.integer(df[[i]])
      }
    }
  } else {
    stop (paste(
      "\nCan only convert matrix or data frame class ",
      "data but found data of class:",
      class(df),
      sep = "\n"))
  }
  return(df)
}


#' Create tab-delimited file and Mplus input syntax from R data.frame
#'
#' The \code{prepareMplusData} function converts an R data.frame
#' (or a list of data frames), into a tab-delimited
#' file (without header) to be used in an Mplus
#' input file. The corresponding Mplus syntax, including the
#' data file definition and variable names,
#' is printed to the console or optionally to an input file.
#'
#' The \code{writeData} argument is new and can be used to reduce overhead
#' from repeatedly writing the same data from R to the disk.  When using the
#' \sQuote{always} option, \code{prepareMplusData} behaves as before, always writing
#' data from R to the disk.  When \sQuote{ifmissing}, R generates an
#' md5 hash of the data prior to writing it out to the disk.  The md5 hash is based on:
#' (1) the dimensions of the dataset, (2) the variable names,
#' (3) the class of every variable, and (4) the raw data from the first and last rows.
#' This combination ensures that under most all circumstances, if the data changes,
#' the hash will change.  The hash is appended to the specified data file name
#' (which is controlled by the logical \code{hashfilename} argument).  Next R
#' checks in the directory where the data would normally be written.  If a data file
#' exists in that directory that matches the hash generated from the data, R will
#' use that existing data file instead of writing out the data again.
#' A final option is \sQuote{never}.  If this option is used, R will not write
#' the data out even if no file matching the hash is found.
#'
#' @param df The R data.frame to be prepared for Mplus
#' @param filename The path and filename for the tab-delimited data file
#'   for use with Mplus. Example: "C:/Mplusdata/data1.dat"
#' @param inpfile Logical value whether the Mplus syntax should be written
#'   to the console or to an input file. Defaults to \code{FALSE}. If
#'   \code{TRUE}, the file name will be the same as \code{filename} with
#'   the extension changed to .inp.  Alternately, this can be a character
#'   string giving the file name to write the Mplus syntax to.
#' @param keepCols A character vector specifying the variable names
#'   within \code{df} to be output to \code{filename} or a numeric
#'   vector of the column indices to be output or a logical vector
#'   corresponding to the same.
#' @param dropCols A character vector specifying the variable names
#'   within \code{df} to be omitted from the data output to \code{filename}
#'   or a numeric vector of the column indices not to be output
#'   or a logical vector corresponding to the same.
#' @param dummyCode An optional character vector of column names indicating
#'   categorical variables in the dataset that should be converted into
#'   dummy codes (using the \code{fastDummies} package). Note that one dummy
#'   code is returned for *each level*, so no reference category is implied.
#'   Thus, it is up to you to drop one of the dummy codes in the Mplus syntax
#'   to denote the reference category and avoid multicollinearity.
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
#' @param imputed A logical whether data are multiply imputed.  Defaults
#'   to \code{FALSE}.  If \code{TRUE}, the data should be a list,
#'   where each element of the list is a multiply imputed dataset.
#' @param writeData A character vector, one of \sQuote{always},
#'   \sQuote{ifmissing}, \sQuote{never} indicating whether the data files
#'   (*.dat) should be written to disk.  Defaults to
#'   \sQuote{always} for consistency with previous behavior.
#'   See details for further information.
#' @param hashfilename A logical whether or not to add a hash of the raw data to the
#'   data file name.  Defaults to \code{FALSE} for consistency with previous
#'   behavior where this feature was not available.
#' @param quiet optional. If \code{TRUE}, show status messages in the console.
#' @return Invisibly returns a character vector of the Mplus input
#'   syntax. Primarily called for its side effect of creating Mplus
#'   data files and optionally input files.
#' @keywords interface
#' @importFrom utils write.table
#' @importFrom data.table fwrite
#' @importFrom checkmate assert_string assert_subset
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
#'
#'
#' # see that syntax was stored
#' test01
#'
#' # example when there is a factor and logical
#' tmpd <- mtcars
#' tmpd$cyl <- factor(tmpd$cyl)
#' tmpd$am <- as.logical(tmpd$am)
#' prepareMplusData(tmpd, "test_type.dat")
#' rm(tmpd)
#'
#' # by default, if re-run, data is re-written, with a note
#' test01b <- prepareMplusData(mtcars, "test01.dat")
#'
#' # if we turn on hashing in the filename the first time,
#' # we can avoid overwriting notes the second time
#' test01c <- prepareMplusData(mtcars, "test01c.dat", hashfilename=TRUE)
#'
#' # now that the filename was hashed in test01c, future calls do not re-write data
#' # as long as the hash matches
#' test01d <- prepareMplusData(mtcars, "test01c.dat",
#'   writeData = "ifmissing", hashfilename=TRUE)
#'
#' # now that the filename was hashed in test01c, future calls do not re-write data
#' # as long as the hash matches
#' test01db <- prepareMplusData(mtcars, "test01d.dat",
#'   writeData = "ifmissing", hashfilename=TRUE)
#'
#' # however, if the data change, then the file is re-written
#' test01e <- prepareMplusData(iris, "test01c.dat",
#'   writeData = "ifmissing", hashfilename=TRUE)
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
#'
#'
#' # can write multiply imputed data too
#' # here are three "imputed" datasets
#' idat <- list(
#'   data.frame(mpg = mtcars$mpg, hp = c(100, mtcars$hp[-1])),
#'   data.frame(mpg = mtcars$mpg, hp = c(110, mtcars$hp[-1])),
#'   data.frame(mpg = mtcars$mpg, hp = c(120, mtcars$hp[-1])))
#'
#' # if we turn on hashing in the filename the first time,
#' # we can avoid overwriting notes the second time
#' testimp1 <- prepareMplusData(idat, "testi1.dat",
#'   writeData = "ifmissing", hashfilename=TRUE,
#'   imputed = TRUE)
#'
#' # now that the filename was hashed, future calls do not re-write data
#' # as long as all the hashes match
#' testimp2 <- prepareMplusData(idat, "testi2.dat",
#'   writeData = "ifmissing", hashfilename=TRUE,
#'   imputed = TRUE)
#'
#' # in fact, the number of imputations can decrease
#' # and they still will not be re-written
#' testimp3 <- prepareMplusData(idat[-3], "testi3.dat",
#'   writeData = "ifmissing", hashfilename=TRUE,
#'   imputed = TRUE)
#'
#' # however, if the data changes, then all are re-written
#' # note that it warns for the two files that already exist
#' # as these two are overwritten
#'
#' idat2 <- list(
#'   data.frame(mpg = mtcars$mpg, hp = c(100, mtcars$hp[-1])),
#'   data.frame(mpg = mtcars$mpg, hp = c(109, mtcars$hp[-1])),
#'   data.frame(mpg = mtcars$mpg, hp = c(120, mtcars$hp[-1])))
#' testimp4 <- prepareMplusData(idat2, "testi4.dat",
#'   writeData = "ifmissing", hashfilename=TRUE,
#'   imputed = TRUE)
#'
#'
#' }
prepareMplusData <- function(df, filename=NULL, inpfile=FALSE, keepCols=NULL, dropCols=NULL, dummyCode=NULL,
                             interactive=TRUE, overwrite=TRUE, imputed=FALSE,
                             writeData=c("always", "ifmissing", "never"), hashfilename=FALSE, quiet = TRUE) {
  
  checkmate::assert_string(filename, null.ok = TRUE)
  checkmate::assert_subset(writeData, choices = c("always", "ifmissing", "never"),  empty.ok = FALSE)
  
  writeData <- match.arg(writeData)
  
  ## message and then exit function if never write data
  if (identical(writeData, "never")) {
    message("No action taken as writeData = 'never'")
    return(invisible(""))
  }
  
  if (!hashfilename && identical(writeData, "ifmissing")) {
    writeData <- "always"
    message("When hashfilename = FALSE, writeData cannot be 'ifmissing', setting to 'always'")
  }
  
  cleand <- .cleanHashData(df = df, keepCols = keepCols,
                           dropCols = dropCols, imputed = imputed)
  
  df <- cleand$data
  md5 <- cleand$md5
  rm(cleand)
  
  # If filename is missing and interactive is TRUE,
  # interactively (through GUI or console) request filename from user
  if (is.null(filename) && isTRUE(interactive)) {
    filename <- file.choose()
  }
  
  ## if filename is still missing at this point, throw an error
  stopifnot(!is.null(filename))
  
  origfilename <- filename
  
  ## impfilename <- gsub("\\.dat$", "_implist.dat", filename)
  impfilename <- filename
  
  if (isTRUE(imputed) && isTRUE(hashfilename)) {
    tmp <- lapply(1:length(md5), function(i) {
      .hashifyFile(filename, md5[[i]],
                   useexisting = identical(writeData, "ifmissing"))
    })
    filename <- unlist(lapply(tmp, function(x) x$filename))
    
    allfilesexist <- all(vapply(tmp, function(x) x$fileexists, FUN.VALUE = NA))
  } else if (isTRUE(imputed) && isFALSE(hashfilename)) {
    filename.base <- gsub("\\.dat", "", filename)
    filename <- unlist(lapply(1:length(df), function(i) {
      paste0(filename.base, "_imp_", i, ".dat")
    }))
  } else {
    tmp <- .hashifyFile(filename, md5,
                        useexisting = identical(writeData, "ifmissing"))
    allfilesexist <- tmp$fileexists
    if (isTRUE(hashfilename)) {
      filename <- tmp$filename
    }
  }
  
  if (isTRUE(imputed)) {
    if(isFALSE(quiet)){ 
      message("writing implist to ", impfilename)
      cat(filename, file = impfilename, sep = "\n")
    }
  }
  
  if (identical(writeData, "ifmissing") && isTRUE(allfilesexist)) {
    if(isFALSE(quiet)){ message(sprintf("File(s) with md5 hash matching data found, using \n%s",
                    paste(filename, collapse = "\n")))}
  } else {
    ## even if writeData = 'ifmissing' if the data are missing, need to write out
    writeData <- "always"
  }
  
  if (identical(writeData, "always")) {
    ## convert factors to numbers
    if (isTRUE(imputed)) {
      df <- lapply(1:length(df), function(i) {
        if (i == 1) {
          .convertData(df[[i]], dummyCode = dummyCode)
        } else {
          suppressMessages(.convertData(df[[i]], dummyCode = dummyCode))
        }
      })
    } else {
      df <- .convertData(df, dummyCode = dummyCode)
    }
    
    if (any(vapply(filename, file.exists, FUN.VALUE = NA))) {
      if (isTRUE(overwrite)) {
        message(paste("The file(s)\n", sQuote(
          paste(vapply(filename[vapply(filename, file.exists, FUN.VALUE = NA)], basename,
                       FUN.VALUE = NA_character_), collapse = ";\n")),
          "\ncurrently exist(s) and will be overwritten"))
      } else {
        stop(paste("The file(s)\n", sQuote(
          paste(vapply(filename[vapply(filename, file.exists, FUN.VALUE = NA)], basename,
                       FUN.VALUE = NA_character_), collapse = ";\n")),
          "\ncurrently exist(s). Specify a different filename or set overwrite=TRUE"))
      }
    }
    
    if (isTRUE(imputed)) {
      junk <- lapply(1:length(df), function(i) {
        fwrite(df[[i]], filename[[i]], sep = "\t",
               col.names = FALSE, row.names = FALSE, na=".")
      })
    } else {
      fwrite(df, filename, sep = "\t",
             col.names = FALSE, row.names = FALSE, na=".")
    }
  }
  
  if (isTRUE(imputed)) {
    syntax <- c(
      "TITLE: Your title goes here\n",
      DATA <- paste0("DATA: FILE = \"", impfilename, "\";\n", "TYPE = IMPUTATION;\n"),
      "VARIABLE: \n", createVarSyntax(df[[1]]), "MISSING=.;\n")
    
  } else {
    syntax <- c(
      "TITLE: Your title goes here\n",
      DATA <- paste0("DATA: FILE = \"", filename, "\";\n"),
      "VARIABLE: \n", createVarSyntax(df), "MISSING=.;\n")
  }
  
  # if inpfile is a logical value and is TRUE
  # then create the file using filename
  # changing the extension to .inp
  if (is.logical(inpfile) && inpfile) {
    inpfile <- gsub("(.*)\\..*$", "\\1.inp", origfilename)
  }
  
  # if the input file is not a character
  # either by user specification or automatically
  # by replacing extension of filename with .inp
  # then just use stdout
  if (isFALSE(is.character(inpfile))) {
    inpfile <- stdout()
  }
  
  if (isTRUE(is.character(inpfile)) && isTRUE(file.exists(inpfile))) {
    if (overwrite) {
      message(paste("The file", sQuote(basename(inpfile)),
                    "currently exists and will be overwritten"))
    } else {
      stop(paste("The file", sQuote(basename(inpfile)),
                 "currently exists. Specify a different filename or set overwrite=TRUE"))
    }
  }
  
  # write out syntax, either to stdout or to a file
  cat(syntax, file=inpfile, sep="")
  
  # return syntax invisibly for later use/reuse
  return(invisible(syntax))
}
