#' Automatically detect variables from an Mplus model object
#'
#' This is a function to automatically detect the variables used in an
#' Mplus model object.
#'
#'
#' @param object An Mplus model object from \code{mplusObject}.#'
#' @param quiet optional. If \code{TRUE}, show status messages in the console.
#' @return A vector of variables from the R dataset to use.
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @export
#' @importFrom stats na.omit
#' @seealso \code{\link{mplusModeler}}, \code{\link{mplusObject}}
#' @keywords interface
#' @examples
#'
#' example1 <- mplusObject(MODEL = "mpg ON wt;",
#'   rdata = mtcars, autov = FALSE)
#' example1$usevariables
#' MplusAutomation:::detectVariables(example1)
#'
#' example2 <- mplusObject(MODEL = "mpg ON wt;",
#'   rdata = mtcars, autov = TRUE)
#' example2$usevariables
#' example3 <- update(example2,
#'   MODEL = ~ . + "mpg ON qsec; wt WITH qsec;",
#'   autov = TRUE)
#' example3$usevariables
#' rm(example1, example2, example3)
detectVariables <- function(object, quiet = TRUE) {
  if (isFALSE(is.null(object$MONTECARLO))) {
    stop("detectVariables() does not work with MONTECARLO models")
  }

  if (isFALSE(is.null(object$rdata))) {
    if (isTRUE(object$imputed)) {
      v <- colnames(object$rdata[[1]])
    } else {
      v <- colnames(object$rdata)
    }

    tmpVARIABLE <- unlist(c(
      tryCatch(unlist(strsplit(object$VARIABLE, split = ";")), error = function(e) ""),
      tryCatch(unlist(strsplit(object$DEFINE, split = ";")), error = function(e) ""),
      tryCatch(unlist(strsplit(object[["MODEL"]], split = ";")), error = function(e) "")))
    tmpVARIABLE <- na.omit(tmpVARIABLE)
    tmpVARIABLE <- tmpVARIABLE[nzchar(tmpVARIABLE)]
    
    if(length(tmpVARIABLE) == 0){
      if(isFALSE(quiet)){
        message("No variables extracted from VARIABLE, DEFINE, or MODEL sections. Using all variables in rdata.")
      }
      return(v)
    }
    tmpVARIABLE <- gsub("\\s", "", unique(unlist(lapply(tmpVARIABLE, function(x) {
      x <- gsub("\n|\t|\r", "", gsub("^(.*)(=| ARE | are | IS | is )(.*)$", "\\3", x))
      xalt <- unique(unlist(strsplit(x, split = "\\s")))

      y <- separateHyphens(x)
      if (isTRUE(is.list(y))) {
        lapply(y, function(z) {
          if (isTRUE(all(sapply(z, function(var) any(grepl(var, x = v, ignore.case=TRUE)))))) {
            i1 <- which(grepl(z[[1]], x = v, ignore.case = TRUE))
            if (isTRUE(length(i1) > 1)) {
              test <- tolower(z[[1]]) == tolower(v)
              if (isTRUE(any(test))) {
                i1 <- which(test)[1]
              }
            }
            i2 <- which(grepl(z[[2]], x = v, ignore.case = TRUE))
            if (length(i2) > 1) {
              test <- tolower(z[[2]]) == tolower(v)
              if (any(test)) {
                i2 <- which(test)[1]
              }
            }
            if (isTRUE(length(i1) && length(i2))) {
              c(v[i1:i2], xalt)
            }
          } else  {
            c(unlist(z), xalt)
          }
        })
      } else {
        xalt
      }
    }))))

    if(isFALSE(quiet)) message("R variables selected automatically as any variable name that\noccurs in the MODEL, VARIABLE, or DEFINE section.")
    usevariables <- unique(v[sapply(v, function(var) any(grepl(var, x = tmpVARIABLE, ignore.case = TRUE)))])

    if (isFALSE(grepl("usevariables", object$VARIABLE, ignore.case=TRUE))) {

      if(isFALSE(quiet)){
        message(sprintf("If any issues, suggest explicitly specifying USEVARIABLES.\nA starting point may be:\nUSEVARIABLES = %s;",
                        paste(usevariables, collapse = " ")))
      }
    }
  }

  return(usevariables)
}

#' Create an Mplus model object
#'
#' This is a function to create an Mplus model object in \code{R}.
#' The object holds all the sections of an Mplus input file, plus some
#' extra \code{R} ones. Once created, the model can be run using other
#' functions such as \code{mplusModeler} or updated using methods defined
#' for the \code{update} function.
#'
#' Mplus model objects  allow a base model to be defined,
#' and then flexibly update the data, change the precise model, etc. If a section
#' does not vary between models, you can leave it the same. For example, suppose
#' you are fitting a number of models, but in all cases, wish to use maximum likelihood
#' estimator, \dQuote{ANALYSIS: ESTIMATOR = ML;} and would like standardized output,
#' \dQuote{OUTPUT: STDYX;}. Rather than retype those in every model, they can be defined
#' in one Mplus model object, and then that can simply be updated with different models,
#' leaving the analysis and output sections untouched. This also means that if a reviewer
#' comes back and asks for all analyses to be re-run say using the robust maximum likelihood
#' estimator, all you have to do is change it in the model object once, and re run all your code.
#'
#' @param TITLE A character string of the title for Mplus.
#' @param DATA A charater string of the data section for Mplus (note, do not define
#'   the filename as this is generated automatically)
#' @param VARIABLE A character string of the variable section for Mplus (note, do not
#'   define the variable names from the dataset as this is generated automatically)
#' @param DEFINE A character string of the define section for Mplus (optional)
#' @param MONTECARLO A character string of the montecarlo section for Mplus (optional).
#'   If used, \code{autov} is defaults to \code{FALSE} instead of the usual default,
#'   \code{TRUE}, but may still be overwritten, if desired.
#' @param MODELPOPULATION A character string of the MODEL POPULATION section for Mplus (optional).
#' @param MODELMISSING A character string of the MODEL MISSING section for Mplus (optional).
#' @param ANALYSIS A character string of the analysis section for Mplus (optional)
#' @param MODEL A character string of the model section for Mplus (optional, although
#'   typically you want to define a model)
#' @param MODELINDIRECT A character string of the MODEL INDIRECT section for Mplus (optional).
#' @param MODELCONSTRAINT A character string of the MODEL CONSTRAINT section for Mplus (optional).
#' @param MODELTEST A character string of the MODEL TEST section for Mplus (optional).
#' @param MODELPRIORS A character string of the MODEL PRIORS section for Mplus (optional).
#' @param OUTPUT A character string of the output section for Mplus (optional)
#' @param SAVEDATA A character string of the savedata section for Mplus (optional)
#' @param PLOT A character string of the plot section for Mplus (optional)
#' @param usevariables A character vector of the variables from the
#'   \code{R} dataset to use in the model.
#' @param rdata An \code{R} dataset to be used for the model.
#' @param autov A logical (defaults to \code{TRUE}) argument indicating
#'   whether R should attempt to guess the correct variables to use from
#'   the R dataset, if \code{usevariables} is left \code{NULL}.
#' @param imputed A logical whether the data are multiply imputed (a list).
#'   Defaults to \code{FALSE}.
#' @param quiet optional. If \code{TRUE}, show status messages in the console.
# @template mplusmodeler_args
#' @param ... Arguments passed on to \code{\link{mplusModeler}} if
#' \code{run > 0}.
#' @return A list of class \code{mplusObject} with elements
#' \item{TITLE}{The title in Mplus (if defined)}
#' \item{DATA}{The data section in Mplus (if defined)}
#' \item{VARIABLE}{The variable section in Mplus (if defined)}
#' \item{DEFINE}{The define section in Mplus (if defined)}
#' \item{MONTECARLO}{The montecarlo section in Mplus (if defined)}
#' \item{MODELPOPULATION}{The modelpopulation section in Mplus (if defined)}
#' \item{MODELMISSING}{The modelmissing section in Mplus (if defined)}
#' \item{ANALYSIS}{The analysis section in Mplus (if defined)}
#' \item{MODEL}{The model section in Mplus (if defined)}
#' \item{MODELINDIRECT}{The modelindirect section in Mplus (if defined)}
#' \item{MODELCONSTRAINT}{The modelconstraint section in Mplus (if defined)}
#' \item{MODELTEST}{The modeltest section in Mplus (if defined)}
#' \item{MODELPRIORS}{The modelpriors section in Mplus (if defined)}
#' \item{OUTPUT}{The output section in Mplus (if defined)}
#' \item{SAVEDATA}{The savedata section in Mplus (if defined)}
#' \item{PLOT}{The plot section in Mplus (if defined)}
#' \item{results}{NULL by default, but can be later updated to include the results from the model run.}
#' \item{usevariables}{A character vector of the variables from the \code{R} data set to be used.}
#' \item{rdata}{The \code{R} data set to use for the model.}
#' \item{imputed}{A logical whether the data are multiply imputed.}
#' \item{autov}{A logical whether the data should have the usevariables detected automatically or not}
#'
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @export
#' @seealso \code{\link{mplusModeler}}
#' @keywords interface
#' @examples
#'
#' example1 <- mplusObject(MODEL = "mpg ON wt;",
#'   usevariables = c("mpg", "hp"), rdata = mtcars)
#' str(example1)
#' rm(example1)
#'
#' # R figures out the variables automagically, with a message
#' example2 <- mplusObject(MODEL = "mpg ON wt;",
#'   rdata = mtcars, autov = TRUE)
#' str(example2)
#' rm(example2)
#'
#' # R can also try to figure out a list of variables when
#' # variable names are hyphenated first-last variable, all variables
#' # between the first and last one will be included
#' example3 <- mplusObject(MODEL = "mpg ON wt-vs;",
#'   rdata = mtcars, autov = TRUE)
#' str(example3)
#' rm(example3)
#'
#' # R warns if the first 8 characters of a (used) variable name are not unique
#' # as they will be indistinguishable in the Mplus output
#' example4 <- mplusObject(MODEL = "basename_01 ON basename_02;",
#'   rdata = data.frame(basename_01 = 1:5, basename_02 = 5:1),
#'   autov = TRUE)
#' rm(example4)
mplusObject <- function(TITLE = NULL, DATA = NULL, VARIABLE = NULL, DEFINE = NULL,
  MONTECARLO = NULL, MODELPOPULATION = NULL, MODELMISSING = NULL, ANALYSIS = NULL,
  MODEL = NULL, MODELINDIRECT = NULL, MODELCONSTRAINT = NULL, MODELTEST = NULL, MODELPRIORS = NULL,
  OUTPUT = NULL, SAVEDATA = NULL, PLOT = NULL,
  usevariables = NULL, rdata = NULL, autov = TRUE, imputed = FALSE,
  quiet = TRUE,
  ...){
  charOrNull <- function(x) {is.character(x) || is.null(x)}
  stopifnot(charOrNull(TITLE))
  stopifnot(charOrNull(DATA))
  stopifnot(charOrNull(VARIABLE))
  stopifnot(charOrNull(DEFINE))
  stopifnot(charOrNull(MONTECARLO))
  stopifnot(charOrNull(MODELPOPULATION))
  stopifnot(charOrNull(MODELMISSING))
  stopifnot(charOrNull(ANALYSIS))

  stopifnot(charOrNull(MODEL))
  stopifnot(charOrNull(MODELINDIRECT))
  stopifnot(charOrNull(MODELCONSTRAINT))
  stopifnot(charOrNull(MODELTEST))
  stopifnot(charOrNull(MODELPRIORS))

  stopifnot(charOrNull(OUTPUT))
  stopifnot(charOrNull(SAVEDATA))
  stopifnot(charOrNull(PLOT))

  object <- list(
    TITLE = TITLE,
    DATA = DATA,
    VARIABLE = VARIABLE,
    DEFINE = DEFINE,
    MONTECARLO = MONTECARLO,
    MODELPOPULATION = MODELPOPULATION,
    MODELMISSING = MODELMISSING,
    ANALYSIS = ANALYSIS,
    MODEL = MODEL,
    MODELINDIRECT = MODELINDIRECT,
    MODELCONSTRAINT = MODELCONSTRAINT,
    MODELTEST = MODELTEST,
    MODELPRIORS = MODELPRIORS,
    OUTPUT = OUTPUT,
    SAVEDATA = SAVEDATA,
    PLOT = PLOT,
    results = NULL,
    usevariables = usevariables,
    rdata = rdata,
    imputed = imputed,
    quiet = quiet
    )

  class(object) <- c("mplusObject", "list")

  if (isFALSE(is.null(MONTECARLO)) && isTRUE(missing(autov))) {
    object$autov <- autov <- FALSE
  }
  
  if (isTRUE(autov) && isTRUE(is.null(usevariables)) &&
      isFALSE(is.null(rdata))) {
      object$usevariables  <- detectVariables(object, quiet = quiet)
  }

  i <- duplicated(substr(object$usevariables, start = 1, stop = 8))
  if (isTRUE(any(i))) {
    message(sprintf("The following variables are not unique in the first 8 characters:\n %s",
      paste(object$usevariables[i], collapse = ", ")))
  }
  
  dots <- list(...)
  
  if(isFALSE(is.null(dots[["run"]]))) {
    if(isTRUE(dots[["run"]] > 0L)){
      
      mplusmodeler_args <- names(dots)[names(dots) %in% c("dataout", "modelout", "run", "check", "varwarnings", 
                                                          "Mplus_command", "writeData", "hashfilename", "killOnFail", "quiet")]
      args_mplusmodeler <- c(list(name = "mplusModeler",
                                  object = object),
                             dots[mplusmodeler_args])
      cl_mplusmodeler <- do.call(call, args_mplusmodeler)
      return(eval.parent(cl_mplusmodeler))
    }
  } else {
    if(isFALSE(is.null(dots[["modelout"]]))) object[["modelout"]] <- dots[["modelout"]]
    if(isFALSE(is.null(dots[["dataout"]]))) object[["dataout"]] <- dots[["dataout"]]
    return(object)
  }
}


#' Update an Mplus model object
#'
#' This is a method for updating an Mplus model object.
#' It takes an Mplus model object as the first argument, and
#' then optionally any sections to update. There are two ways
#' to update a section using a formula interface. \code{~ "new stuff"} will
#' replace a given section with the new text.  Alternately, you can add
#' additional text using \code{~ + "additional stuff"}. Combined these let you
#' replace or add to a section.
#' @param object An object of class mplusObject
#' @param quiet optional. If \code{TRUE}, show status messages in the console.
#' @param \dots Additional arguments to pass on
#' @return An (updated) Mplus model object
#' @export
#' @method update mplusObject
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @keywords interface
#' @examples
#' example1 <- mplusObject(MODEL = "mpg ON wt;",
#'   usevariables = c("mpg", "hp"), rdata = mtcars)
#' x <- ~ "ESTIMATOR = ML;"
#' str(update(example1, rdata = iris))
#' str(update(example1, ANALYSIS = x))
#' str(update(example1, MODEL = ~ "wt ON hp;"))
#' str(update(example1, MODEL = ~ . + "wt ON hp;"))
#' str(update(example1, ANALYSIS = x, MODEL = ~ . + "wt ON hp;"))
#'
#' ## check that use variables can be updated & over ridden
#' str(update(example1, usevariables = c("mpg", "hp", "cyl")))
#'
#' # test to make sure . in Mplus code does not cause problems
#' str(update(example1, ANALYSIS = x, MODEL = ~ . + "wt ON hp*.5;"))
#' rm(example1, x)
update.mplusObject <- function(object, quiet = TRUE, ...) {

  dots <- list(...)
  if (isTRUE(length(dots) == 0)) return(object)
  sectionNames <- names(dots)

  mplusList <- c("TITLE", "DATA", "VARIABLE", "DEFINE",
    "MONTECARLO", "MODELPOPULATION", "MODELMISSING", "ANALYSIS",
    "MODEL", "MODELINDIRECT", "MODELCONSTRAINT", "MODELTEST", "MODELPRIORS",
    "OUTPUT", "SAVEDATA", "PLOT")
  rList <- c("results", "usevariables", "rdata")

  mplusIndex <- sectionNames[which(sectionNames %in% mplusList)]
  rIndex <- sectionNames[which(sectionNames %in% rList)]

  if (isTRUE(length(mplusIndex) > 0)) {
    mplusSections <- lapply(mplusIndex, function(n) {
      tmp <- dots[[n]]
      tmp <- as.character(tmp[[2]])
      if (isTRUE(any(grepl("^\\.$", tmp)))) {
        old <- paste0(object[[n]], "\n")
      } else {
        old <- ""
      }
      new <- tmp[length(tmp)]
      combined <- paste(old, new, collapse = "\n")
    })

    object[mplusIndex] <- mplusSections
  }

  if (isTRUE(length(rIndex) > 0)) {
    object[rIndex] <- dots[rIndex]
  }

  if (isTRUE(object$autov) && isFALSE("usevariables" %in% rIndex)) {
    object$usevariables <- detectVariables(object)
  }

  return(object)
}

#' Create the Mplus input text for an mplusObject
#'
#' This function takes an object of class \code{mplusObject} and creates
#' the Mplus input text corresponding to it, including data link and
#' variable names.
#'
#' @param object An object of class mplusObject
#' @param filename The name of the data file as a character vector
#' @param check A logical indicating whether or not to run \code{parseMplus}
#'   on the created input file. Checks for errors like lines that are too long,
#'   or for missing semi-colons and gives notes.
#' @param add A logical passed on to \code{parseMplus} whether to add semi
#'   colons to line ends. Defaults to \code{FALSE}.
#' @param imputed A logical whether the data are multiply imputed.
#'   Defaults to \code{FALSE}.
#' @return A character string containing all the text for the Mplus
#'   input file.
#' @keywords interface
#' @export
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @seealso \code{\link{prepareMplusData}}, \code{\link{mplusModeler}}
#' @examples
#' # example mplusObject
#' example1 <- mplusObject(MODEL = "mpg ON wt;",
#'   usevariables = c("mpg", "hp"), rdata = mtcars)
#'
#' # create the Mplus input text
#' cat(createSyntax(example1, "example1.dat"), file=stdout(), fill=TRUE)
#'
#' # update the object, then create input text
#' cat(createSyntax(update(example1,
#'   TITLE = ~ "This is my title;",
#'   MODEL = ~ . + "\nmpg ON hp;",
#'   usevariables = c("mpg", "hp", "wt")), "example1.dat"),
#'   file=stdout(),
#'   fill=TRUE)
#' rm(example1)
createSyntax <- function(object, filename, check=TRUE, add=FALSE, imputed=FALSE) {
  stopifnot(isTRUE(inherits(object, "mplusObject")))

  mplusList <- data.frame(
    Names = c("TITLE", "DATA", "VARIABLE", "DEFINE",
              "MONTECARLO", "MODELPOPULATION", "MODELMISSING", "ANALYSIS",
              "MODEL", "MODELINDIRECT", "MODELCONSTRAINT", "MODELTEST", "MODELPRIORS",
              "OUTPUT", "SAVEDATA", "PLOT"),
    Labels = c("TITLE", "DATA", "VARIABLE", "DEFINE",
              "MONTECARLO", "MODEL POPULATION", "MODEL MISSING", "ANALYSIS",
              "MODEL", "MODEL INDIRECT", "MODEL CONSTRAINT", "MODEL TEST", "MODEL PRIORS",
              "OUTPUT", "SAVEDATA", "PLOT"),
    stringsAsFactors = FALSE)

  simulation <- isFALSE(is.null(object$MONTECARLO))

  if (isFALSE(simulation) && isFALSE(missing(filename))) {
    dFile <- paste0("FILE = \"", filename, "\";\n")
    if (isTRUE(imputed)) {
      dFile <- paste0(dFile, "TYPE = IMPUTATION;\n")
    }

    object$DATA <- paste(dFile, object$DATA, collapse = "\n")
  }

  if (isFALSE(is.null(object$rdata)) && isFALSE(is.null(object$usevariables))) {
    if (isTRUE(object$imputed)) {
      vNames <- createVarSyntax(object$rdata[[1]][, object$usevariables, drop = FALSE])
    } else {
      vNames <- createVarSyntax(object$rdata[, object$usevariables, drop = FALSE])
    }
    object$VARIABLE <- paste(vNames, "MISSING=.;\n", object$VARIABLE, collapse = "\n")
  }

  index <- sapply(object[mplusList$Names], function(x) {
    !(is.null(x) || !nzchar(gsub("\\s*", "", x, perl=TRUE)))
  })

  sections <- mplusList$Names[index]
  body <- unlist(lapply(sections, function(n) {
    c(paste0(mplusList$Labels[match(n, mplusList$Names)], ":"), object[[n]])
  }))
  body <- paste(body, collapse = "\n")

  if (isTRUE(check)) {
    body <- parseMplus(body, add = add)
  }

  return(body)
}

#' Create, run, and read Mplus models.
#'
#' This is a convenience wrapper to automate many of the
#' usual steps required to run an Mplus model. It relies in part
#' on functions from the MplusAutomation package.
#'
#' Combined with functions from the MplusAutomation package,
#' this function is designed to make it easy to fit Mplus models
#' from R and to ease many of the usual frustrations with Mplus.
#' For example, Mplus has very specific formats it accepts data in,
#' but also very little data management facilities. Using \R data
#' management is easy. This function is designed to make using data
#' from \R in Mplus models easy.
#' It is also common to want to fit many different models that are
#' slight variants. This can be tedius in Mplus, but using \R you can
#' create one basic set of input, store it in a vector, and then just
#' modify that (e.g., using regular expressions) and pass it to Mplus.
#' You can even use loops or the \code{*apply} constructs to fit the same
#' sort of model with little variants.
#'
#' The \code{writeData} argument is new and can be used to reduce overhead
#' from repeatedly writing the same data from R to the disk.  When using the
#' \sQuote{always} option, \code{mplusModeler} behaves as before, always writing
#' data from R to the disk.  This remains the default for the \code{prepareMplusData}
#' function to avoid confusion or breaking old code.  However, for \code{mplusModeler},
#' the default has been set to \sQuote{ifmissing}.  In this case, R generates an
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
#' @param object An object of class mplusObject
#' @template mplusmodeler_args
#' @seealso \code{\link{runModels}} and \code{\link{readModels}}
#' @import boot
#' @export
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @examples
#' \dontrun{
#' # minimal example of a model using builtin data, allowing R
#' # to automatically guess the correct variables to use
#' test <- mplusObject(MODEL = "mpg ON wt hp;
#'   wt WITH hp;", rdata = mtcars)
#'
#'  # estimate the model in Mplus and read results back into R
#'  res <- mplusModeler(test, modelout = "model1.inp", run = 1L)
#'
#'  # when forcing writeData = "always" data gets overwritten (with a warning)
#'  resb <- mplusModeler(test, modelout = "model1.inp", run = 1L,
#'                       writeData = "always")
#'
#'  # using writeData = "ifmissing", the default, no data re-written
#'  resc <- mplusModeler(test, modelout = "model1.inp", run = 1L)
#'
#'  # using writeData = "ifmissing", the default, data ARE written
#'  # if data changes
#'  test <- mplusObject(MODEL = "mpg ON wt hp;
#'    wt WITH hp;", rdata = mtcars[-10, ])
#'  resd <- mplusModeler(test, modelout = "model1.inp", run = 1L)
#'
#'  # show summary
#'  summary(resd)
#'
#'  # show coefficients
#'  coef(resd)
#'
#'  # what if you wanted confidence intervals
#'  # and standardized values?
#'  # first update to tell Mplus you want them, re-run and print
#'  test <- update(test, OUTPUT = ~ "CINTERVAL; STDYX;")
#'  resd <- mplusModeler(test, modelout = "model1.inp", run = 1L)
#'
#' coef(resd)
#' confint(resd)
#'
#' # now standardized
#' coef(resd, type = "stdyx")
#' confint(resd, type = "stdyx")
#'
#' # put together in one data frame if desired
#' merge(
#'   coef(resd, type = "stdyx"),
#'   confint(resd, type = "stdyx"),
#'   by = "Label")
#'
#'  # remove files
#'  unlink(resc$results$input$data$file)
#'  unlink(resd$results$input$data$file)
#'  unlink("model1.inp")
#'  unlink("model1.out")
#'
#' # simple example of a model using builtin data
#' # demonstrates use with a few more sections
#' test2 <- mplusObject(
#'   TITLE = "test the MplusAutomation Package and mplusModeler wrapper;",
#'   MODEL = "
#'     mpg ON wt hp;
#'     wt WITH hp;",
#'   usevariables = c("mpg", "wt", "hp"),
#'   rdata = mtcars)
#'
#'  res2 <- mplusModeler(test2, modelout = "model2.inp", run = 1L)
#'
#'  # remove files
#'  unlink(res2$results$input$data$file)
#'  unlink("model2.inp")
#'  unlink("model2.out")
#'
#'  # similar example using a robust estimator for standard errors
#'  # and showing how an existing model can be easily updated and reused
#'  test3 <- update(test2, ANALYSIS = ~ "ESTIMATOR = MLR;")
#'
#'  res3 <- mplusModeler(test3, modelout = "model3.inp", run = 1L)
#'  unlink(res3$results$input$data$file)
#'  unlink("model3.inp")
#'  unlink("model3.out")
#'
#'  # now use the built in bootstrapping methods
#'  # note that these work, even when Mplus will not bootstrap
#'  # also note how categorical variables and weights are declared
#'  # in particular, the usevariables for Mplus must be specified
#'  # because mroe variables are included in the data than are in the
#'  # model. Note the R usevariables includes all variables for both
#'  # model and weights. The same is true for clustering.
#'  test4 <- mplusObject(
#'    TITLE = "test bootstrapping;",
#'    VARIABLE = "
#'      CATEGORICAL = cyl;
#'      WEIGHT = wt;
#'      USEVARIABLES = cyl mpg;",
#'    ANALYSIS = "ESTIMATOR = MLR;",
#'    MODEL = "
#'      cyl ON mpg;",
#'    usevariables = c("mpg", "wt", "cyl"),
#'    rdata = mtcars)
#'
#'  res4 <- mplusModeler(test4, "mtcars.dat", modelout = "model4.inp", run = 10L,
#'    hashfilename = FALSE)
#'  # see the results
#'  res4$results$boot
#'
#'  # remove files
#'  unlink("mtcars.dat")
#'  unlink("model4.inp")
#'  unlink("model4.out")
#'
#' # Monte Carlo Simulation Example
#' montecarlo <- mplusObject(
#'  TITLE = "Monte Carlo Example;",
#'  MONTECARLO = "
#'   NAMES ARE i1-i5;
#'   NOBSERVATIONS = 100;
#'   NREPS = 100;
#'   SEED = 1234;",
#'  MODELPOPULATION = "
#'   f BY i1-i5*1;
#'   f@1;
#'   i1-i5*1;",
#'  ANALYSIS = "
#'   ESTIMATOR = BAYES;
#'   PROC = 2;
#'   fbiter = 100;",
#'  MODEL = "
#'   f BY i1-i5*.8 (l1-l5);
#'   f@1;
#'   i1-i5*1;",
#'  MODELPRIORS = "
#'    l1-l5 ~ N(.5 .1);",
#'  OUTPUT = "TECH9;")
#'
#' fitMonteCarlo <- mplusModeler(montecarlo,
#'   modelout = "montecarlo.inp",
#'   run = 1L,
#'   writeData = "always",
#'   hashfilename = FALSE)
#'
#' unlink("montecarlo.inp")
#' unlink("montecarlo.out")
#'
#'
#' # Example including ID variable and extracting factor scores
#' dat <- mtcars
#' dat$UID <- 1:nrow(mtcars)
#'
#' testIDs <- mplusObject(
#'   TITLE = "test the mplusModeler wrapper with IDs;",
#'   VARIABLE = "IDVARIABLE = UID;",
#'   MODEL = "
#'     F BY mpg wt hp;",
#'   SAVEDATA = "
#'     FILE IS testid_fscores.dat;
#'     SAVE IS fscores;
#'     FORMAT IS free;",
#'   usevariables = c("UID", "mpg", "wt", "hp"),
#'   rdata = dat)
#'
#'  resIDs <- mplusModeler(testIDs, modelout = "testid.inp", run = 1L)
#'
#' # view the saved data from Mplus, including factor scores
#' # the indicator variables, and the ID variable we specified
#' head(resIDs$results$savedata)
#'
#' # merge the factor scores with the rest of the original data
#' # merge together by the ID column
#' dat <- merge(dat, resIDs$results$savedata[, c("F", "UID")],
#'   by = "UID")
#'
#' # correlate merged factor scores against some other new variable
#' with(dat, cor(F, qsec))
#'
#'
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
#' testobjimp <- mplusObject(MODEL = "[mpg];", rdata = idat, imputed = TRUE)
#'
#' testimp <- mplusModeler(
#'   testobjimp,
#'   modelout = "testimp.inp",
#'   writeData = "ifmissing", hashfilename=FALSE)
#'
#' testimp <- mplusModeler(
#'   testobjimp,
#'   modelout = "testimp.inp",
#'   writeData = "ifmissing", hashfilename=TRUE)
#'
#' testimp <- mplusModeler(
#'   testobjimp,
#'   modelout = "testimp.inp",
#'   writeData = "ifmissing", hashfilename=TRUE,
#'   run = TRUE)
#'
#' testobjimp2 <- mplusObject(MODEL = "[hp];", rdata = idat, imputed = TRUE)
#' testimp2 <- mplusModeler(
#'   testobjimp2,
#'   modelout = "testimp2.inp",
#'   writeData = "ifmissing", hashfilename=TRUE,
#'   run = TRUE)
#'
#'  # remove files
#'  unlink(resIDs$results$input$data$file)
#'  unlink("testid.inp")
#'  unlink("testid.out")
#'  unlink("testid_fscores.dat")
#'  unlink("Mplus Run Models.log")
#' }
mplusModeler <- function(object, dataout, modelout, run = 0L,
                         check = FALSE, varwarnings = TRUE, Mplus_command = detectMplus(),
                         writeData = c("ifmissing", "always", "never"),
                         hashfilename = TRUE, killOnFail = TRUE,
                         quiet = TRUE,
                         ...) {
  stopifnot(isTRUE((run %% 1) == 0 && length(run) == 1))
  oldSHELL <- Sys.getenv("SHELL")
  Sys.setenv(SHELL = Sys.getenv("COMSPEC"))
  on.exit(Sys.setenv(SHELL = oldSHELL))

  writeData <- match.arg(writeData)

  simulation <- isFALSE(is.null(object$MONTECARLO))
  # Check arguments
  if(!is.null(object[["check"]])){
    if(!check == object[["check"]]){
      
    }
  } 
  if (isTRUE(missing(modelout)) && isTRUE(missing(dataout))) {
    if(is.null(object[["modelout"]]) & is.null(object[["dataout"]])){
      stop("You must specify either modelout or dataout")
    } else {
      if(!is.null(object[["modelout"]])) {
        modelout <- object[["modelout"]]
        if(is.null(object[["dataout"]])){
          dataout <- gsub("(^.*)(\\.inp$)", "\\1.dat", modelout)
        }
      }
      if(!is.null(object[["dataout"]])) {
        dataout <- object[["dataout"]]
        if(is.null(object[["modelout"]])){
          modelout <- gsub("(.*)(\\..+$)", "\\1.inp", dataout)
        }
      }
    }
  } else if (isTRUE(missing(dataout)) && isFALSE(simulation)) {
    dataout <- gsub("(^.*)(\\.inp$)", "\\1.dat", modelout)
  } else if (isTRUE(missing(modelout))) {
    modelout <- gsub("(.*)(\\..+$)", "\\1.inp", dataout)
  }

  if (isTRUE(simulation)) {
    if (isTRUE(run > 1)) {
      run <- 1L
      message("run cannot be greater than 1 when using montecarlo simulation, setting run = 1")
    }
    dataout <- dataout2 <- NULL
  } else if (isFALSE(simulation)) {
    ## if (isTRUE(object$imputed)) {
    ##   if (identical(writeData, "ifmissing")) {
    ##     writeData <- "always"
    ##     message("When imputed = TRUE, writeData cannot be 'ifmissing', setting to 'always'")
    ##   }
    ##   if (isTRUE(hashfilename)) {
    ##     hashfilename <- FALSE
    ##     message("When imputed = TRUE, hashfilename cannot be TRUE, setting to FALSE")
    ##   }
    ## }
    if (isTRUE(run > 1)) {
      if (identical(writeData, "ifmissing")) {
        writeData <- "always"
        message("When run > 1, writeData cannot be 'ifmissing', setting to 'always'")
      }
      if (isTRUE(hashfilename)) {
        hashfilename <- FALSE
        message("When run > 1, hashfilename cannot be TRUE, setting to FALSE")
      }
    }
    if (isFALSE(hashfilename) && identical(writeData, "ifmissing")) {
      writeData <- "always"
      message("When hashfilename = FALSE, writeData cannot be 'ifmissing', setting to 'always'")
    }

    if (isTRUE(hashfilename) && isFALSE(object$imputed)) {
      md5 <- .cleanHashData(
        df = object$rdata,
        keepCols = object$usevariables,
        imputed = object$imputed)$md5
      tmp <- .hashifyFile(dataout, md5,
                          useexisting = identical(writeData, "ifmissing"))
      dataout2 <- tmp$filename
    } else {
      dataout2 <- dataout
    }
  }

  .run <- function(data, i, boot = TRUE, imputed = FALSE, ...) {
    if (isFALSE(simulation)) {
      if (isTRUE(imputed)) {
        if (isTRUE(boot)) stop("Cannot use imputed data and bootstrap")
        prepareMplusData(df = data,
                         keepCols = object$usevariables,
                         filename = dataout,
                         inpfile = tempfile(tmpdir = tempdir(check = TRUE)),
                         imputed = imputed,
                         writeData = writeData,
                         hashfilename = hashfilename,
                         quiet = quiet,
                         ...)
      } else {
        prepareMplusData(df = data[i, , drop = FALSE],
                         keepCols = object$usevariables,
                         filename = dataout, inpfile = tempfile(tmpdir = tempdir(check = TRUE)),
                         writeData = ifelse(boot, "always", writeData),
                         hashfilename = ifelse(boot, FALSE, hashfilename),
                         quiet = quiet,
                         ...)
      }
    }

    runModels(target = modelout, Mplus_command = Mplus_command,
              killOnFail = killOnFail, logFile=NULL, quiet = quiet)
    outfile <- gsub("(^.*)(\\.inp$)", "\\1.out", modelout)
    results <- readModels(target = outfile, quiet = quiet)
    if (isFALSE(boot)) {
      if (isFALSE(varwarnings)) rmVarWarnings(outfile)
      return(invisible(results))
    } else {
      with(results, unlist(lapply(
        parameters[!grepl("^ci\\..+", names(parameters))],
        function(x) {
          x <- x[, c("est", "se")]
          x[] <- lapply(x, as.numeric)
          as.vector(t(na.omit(x)))
        }
      )))
    }
  }

  body <- createSyntax(object, dataout2, check=check, imputed = object$imputed)
  writeLines(body, con = modelout, sep = "\n")
  if(isFALSE(quiet)) message("Wrote model to: ", modelout)

  if (isFALSE(simulation)) {
    if (isTRUE(hashfilename) && identical(writeData, "ifmissing")) {
      if (isFALSE(object$imputed)) {
        if (isTRUE(tmp$fileexists)) {
          NULL
        } else {
          if(isFALSE(quiet)) message("Wrote data to: ", dataout2)
        }
      }
    } else {
      if(isFALSE(quiet)) message("Wrote data to: ", dataout2)
    }
  }

  results <- bootres <- NULL
  finalres <- list(model = results, boot = bootres)

  if (isFALSE(simulation)) {
    if (isTRUE(run > 1) && isFALSE(object$imputed)) {
      bootres <- boot(object$rdata, .run, R = run, sim = "ordinary")
      finalres$boot <- bootres
      class(finalres) <- c("boot.mplus.model", "list")
    }
  }

  if (isTRUE(run>0)) {
    results <- .run(data = object$rdata, boot = FALSE, imputed = object$imputed, ...)
    finalres$model <- results
  } else if (isFALSE(simulation)) {
    prepareMplusData(df = object$rdata,
                     keepCols = object$usevariables,
                     filename = dataout,
                     inpfile = tempfile(),
                     imputed = object$imputed,
                     writeData = writeData,
                     hashfilename = hashfilename,
                     ...)
    return(object)
  }
  if (isTRUE(run == 1)) {
    object$results <- finalres$model
  } else {
    object$results <- finalres
  }
  return(object)
}


#' Create Mplus code for various residual covariance structures.
#'
#' This function makes it easy to write the Mplus syntax for various
#' residual covariance structure.
#'
#'
#' The \strong{homogenous} residual covariance structure estimates one parameter:
#' the residual variance, \eqn{\sigma^{2}_{e}}{s^2}. The residual variance
#' is assumed to be identical for all variables and all covariances are
#' assumed to be zero. The structure is represented in this table.
#' \tabular{llllll}{
#'     \tab t1 \tab t2 \tab t3 \tab \ldots \tab tn \cr
#'   t1 \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \tab \tab \ldots \tab \cr
#'   t2 \tab 0 \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \tab \ldots \tab \cr
#'   t3 \tab 0 \tab 0 \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \ldots \tab \cr
#'   \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \cr
#'   tn \tab 0 \tab 0 \tab 0 \tab \ldots \tab \eqn{\sigma^{2}_{e}}{s^2} \cr
#' }
#'
#' The \strong{heterogenous} residual covariance structure estimates
#' \bold{n} parameters, where \bold{n} is the number of variables.
#' A unique residual variance is estimated for every variable. All
#' covariances are assumed to be zero. The structure is represented in this table.
#' \tabular{llllll}{
#'     \tab t1 \tab t2 \tab t3 \tab \ldots \tab tn \cr
#'   t1 \tab \eqn{\sigma^{2}_{e1}}{s1^2} \tab \tab \tab \ldots \tab \cr
#'   t2 \tab 0 \tab \eqn{\sigma^{2}_{e2}}{s2^2} \tab \tab \ldots \tab \cr
#'   t3 \tab 0 \tab 0 \tab \eqn{\sigma^{2}_{e3}}{s3^2} \tab \ldots \tab \cr
#'   \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \cr
#'   tn \tab 0 \tab 0 \tab 0 \tab \ldots \tab \eqn{\sigma^{2}_{en}}{sn^2} \cr
#' }
#'
#' The \strong{compound symmetric} residual covariance structure estimates
#' two parameters: one for the residual variance , \eqn{\sigma^{2}_{e}}{s^2},
#' and one for the covariance. The residual variance
#' is assumed to be identical for all variables and all covariances are
#' assumed to be identical. The structure is represented in this table.
#' \tabular{llllll}{
#'     \tab t1 \tab t2 \tab t3 \tab \ldots \tab tn \cr
#'   t1 \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \tab \tab \ldots \tab \cr
#'   t2 \tab \eqn{\rho}{rho} \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \tab \ldots \tab \cr
#'   t3 \tab \eqn{\rho}{rho} \tab \eqn{\rho}{rho} \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \ldots \tab \cr
#'   \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \cr
#'   tn \tab \eqn{\rho}{rho} \tab \eqn{\rho}{rho} \tab \eqn{\rho}{rho} \tab \ldots \tab \eqn{\sigma^{2}_{e}}{s^2} \cr
#' }
#'
#' The \strong{toeplitz} residual covariance structure estimates
#' \bold{n} parameters, one for every band of the matrix.
#' The residual variance , \eqn{\sigma^{2}_{e}}{s^2}, is
#' assumed to be identical for all variables. The covariances one step removed
#' are all assumed identical. Likewise for all further bands.
#' The structure is represented in this table.
#' \tabular{llllll}{
#'     \tab t1 \tab t2 \tab t3 \tab \ldots \tab tn \cr
#'   t1 \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \tab \tab \ldots \tab \cr
#'   t2 \tab \eqn{\rho}{rho} \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \tab \ldots \tab \cr
#'   t3 \tab \eqn{\rho_{2}}{rho2} \tab \eqn{\rho}{rho} \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \ldots \tab \cr
#'   \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \cr
#'   tn \tab \eqn{\rho_{n}}{rhon} \tab \eqn{\rho_{n - 1}}{rho(n - 1)} \tab \eqn{\rho_{n - 2}}{rho(n - 2)} \tab \ldots \tab \eqn{\sigma^{2}_{e}}{s^2} \cr
#' }
#'
#' The \strong{autoregressive} residual covariance structure has two parameters:
#' the residual variance, \eqn{\sigma^{2}_{e}}{s^2} and
#' the correlation between adjacent time points, \eqn{\rho}{rho}. The variances
#' are constrained to be equal for all time points. A single correlation
#' parameter is estimated. The \eqn{\rho}{rho} is the correlation between adjacent
#' time points such as 1 and 2 or 2 and 3. More distant relationships are assumed
#' to have smaller correlations, decreasing exponentially. Thus between 1 and 3,
#' the estimate is \eqn{\rho^2}{rho^2}. The structure is represented in this table.
#' \tabular{llllll}{
#'     \tab t1 \tab t2 \tab t3 \tab \ldots \tab tn \cr
#'   t1 \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \tab \tab \ldots \tab \cr
#'   t2 \tab \eqn{\rho}{rho} \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \tab \ldots \tab \cr
#'   t3 \tab \eqn{\rho^2}{rho^2} \tab \eqn{\rho}{rho} \tab \eqn{\sigma^{2}_{e}}{s^2} \tab \ldots \tab \cr
#'   \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \cr
#'   tn \tab \eqn{\rho^{n-1}}{rho^(n-1)} \tab \eqn{\rho^{n-2}}{rho^(n-2)} \tab \eqn{\rho^{n-3}}{rho^(n-3)} \tab \ldots \tab \eqn{\sigma^{2}_{e}}{s^2} \cr
#' }
#' Because structural equation models generally model covariance structures,
#' the autoregressive residual structure must be parameterized in terms of
#' covariances. This is done in two parts. First, the function returns
#' syntax to estimate all the pairwise covariances, labelling the parameters
#' \eqn{\rho}{rho}, \eqn{\rho^2}{rho^2}, etc. so that they are constrained to be
#' equal. Next, it returns the syntax for the necessary model constraints to
#' constrain the different covariances, to decrease exponentially in their
#' correlations. This is done via:
#' \deqn{\rho^2 = (\frac{\rho}{\sigma^2_{e}})^{2}\sigma^2_{e}}{rho^2 = (rho/s^2)^2 * s^2}
#' and likewise for all later time points.
#'
#' The \strong{unstructured} residual covariance structure estimates
#' \eqn{\frac{n(n + 1)}{2}}{(n(n + 1))/2} parameters. It is unstructured
#' in that every variance and covariance is freely estimated with no
#' constraints. However, in most cases, this results in an overparameterized
#' model and is unestimable. The structure is represented in this table.
#'
#' \tabular{llllll}{
#'     \tab t1 \tab t2 \tab t3 \tab \ldots \tab tn \cr
#'   t1 \tab \eqn{\sigma^{2}_{e1}}{s1^2} \tab \tab \tab \ldots \tab \cr
#'   t2 \tab \eqn{\rho_{1}}{rho1} \tab \eqn{\sigma^{2}_{e2}}{s2^2} \tab \tab \ldots \tab \cr
#'   t3 \tab \eqn{\rho_{2}}{rho2} \tab \eqn{\rho_{3}}{rho3} \tab \eqn{\sigma^{2}_{e3}}{s3^2} \tab \ldots \tab \cr
#'   \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \tab \ldots \cr
#'   tn \tab \eqn{\rho_{5}}{rho5} \tab \eqn{\rho_{6}}{rho6} \tab \eqn{\rho_{7}}{rho7} \tab \ldots \tab \eqn{\sigma^{2}_{en}}{sn^2} \cr
#' }
#'
#' @param x input character vector of variable names, ordered by time
#' @param type A character string indicating the type of residual covariance
#'   structure to be used. Defaults to \sQuote{homogenous}. Current options include
#'   \sQuote{homogenous}, \sQuote{heterogenous}, \sQuote{cs} for compound symmetric,
#'   \sQuote{toeplitz} for banded toeplitz, \sQuote{ar} for autoregressive, and
#'   \sQuote{un} for unstructured.
#' @param r a character vector of the base label to name covariance parameters.
#'   Defaults to \sQuote{rho}.
#' @param e a character vector of the error variance of the variable.
#'   Used to create constraints on the covariance parameters. Defaults to \sQuote{e}.
#' @param collapse whether to collapse the covariance code using \sQuote{PWITH}. Note that
#'   at the time of writing, Mplus does not allow more than 80 characters per row.
#'   Defaults to \code{FALSE}.
#' @return A named character vector of class \sQuote{MplusRstructure} with four elements:
#'   \item{all}{A character string collapsing all other sections.}
#'   \item{Variances}{A character string containing all of the variances.}
#'   \item{Covariances}{A character string containing all of the
#'     covariances, properly labelled to allow constraints and the
#'     autoregressive residual covariance structure.}
#'   \item{Constraints}{A character string containing the \sQuote{MODEL CONSTRAINT}
#'     section and code needed to parameterize the residual covariance structure
#'     as autoregressive.}
#' @keywords interface
#' @export
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @examples
#' # all five structures collapsing
#' mplusRcov(letters[1:4], "homogenous", "rho", "e", TRUE)
#' mplusRcov(letters[1:4], "heterogenous", "rho", "e", TRUE)
#' mplusRcov(letters[1:4], "cs", "rho", "e", TRUE)
#' mplusRcov(letters[1:4], "toeplitz", "rho", "e", TRUE)
#' mplusRcov(letters[1:4], "ar", "rho", "e", TRUE)
#' mplusRcov(letters[1:4], "un", "rho", "e", TRUE)
#'
#' # all five structures without collapsing
#' # useful for long names or many variables
#' # where a line may cross 80 characters
#' mplusRcov(letters[1:4], "homogenous", "rho", "e", FALSE)
#' mplusRcov(letters[1:4], "heterogenous", "rho", "e", FALSE)
#' mplusRcov(letters[1:4], "cs", "rho", "e", FALSE)
#' mplusRcov(letters[1:4], "toeplitz", "rho", "e", FALSE)
#' mplusRcov(letters[1:4], "ar", "rho", "e", FALSE)
#' mplusRcov(letters[1:4], "un", "rho", "e", FALSE)
mplusRcov <- function(x, type = c("homogenous", "heterogenous", "cs", "toeplitz", "ar", "un"),
  r = "rho", e = "e", collapse=FALSE) {

  type <- match.arg(type)

  indCov <- function(x, r, e, collapse) {
    n <- length(x)
    if (isTRUE(collapse)) {
      res <- lapply(1:(n - 1), function(i) {
        paste0(x[i], " WITH ", paste(paste0(x, "@0")[(i+1):n], collapse = " "), ";")
      })
    } else {
      res <- lapply(1:(n - 1), function(i) {
        paste(paste0(x[i], " WITH ", paste0(x, "@0")[(i+1):n], ";"), collapse = "\n")
      })
    }

    res <- do.call("paste", list(unlist(res), collapse = "\n"))

    list(Covariances = res, Constraints = "")
  }

  toeplitzCov <- function(x, r, e, collapse, type = c("toeplitz", "cs", "ar")) {

    type <- match.arg(type)
    n <- length(x)

    k <- n - 1
    index <- lapply(1:k, function(j) {
      sapply(1:(n - j), function(i) c(i, i + j))
    })

    rho <- switch(type,
      toeplitz = paste0(r, c("", seq_along(index)[-1])),
      cs = rep(r, length(index)),
      ar = paste0(r, c("", seq_along(index)[-1]))
    )

    if (isTRUE(collapse)) {
      res <- lapply(seq_along(index), function(i) {
        start <- paste(x[index[[i]][1, ]], collapse = " ")
        end <- paste(x[index[[i]][2, ]], collapse = " ")
        paste0(paste(c(start, end), collapse = " PWITH "), paste0(" (", rho[i], ");"))
      })
    } else {
      res <- lapply(seq_along(index), function(i) {
        sapply(1:ncol(index[[i]]), function(j) {
          paste0(x[index[[i]][1, j]], " WITH ", x[index[[i]][2, j]], paste0(" (", rho[i], ");"))
        })
      })
    }
    res <- do.call("paste", list(unlist(res), collapse = "\n"))

    constraint <- switch(type,
      toeplitz = "",
      cs = "",
      ar = {
        cons <- lapply(seq_along(rho)[-1], function(i) {
          paste0("  ", rho[i], " = ((", r, "/", e, ")^", i, ") * ", e, ";")
        })
        cons <- do.call("paste", list(unlist(cons), collapse = "\n"))
        paste(c("MODEL CONSTRAINT: \n", cons, "\n"), collapse = "")
      }
    )

    list(Covariances = res, Constraints = constraint)
  }

  V <- switch(type,
    homogenous = paste0(paste(x, collapse = " "), " (", e, ");"),
    heterogenous = paste0(paste(x, collapse = " "), ";"),
    cs = paste0(paste(x, collapse = " "), " (", e, ");"),
    toeplitz = paste0(paste(x, collapse = " "), " (", e, ");"),
    ar = paste0(paste(x, collapse = " "), " (", e, ");"),
    un = paste0(paste(x, collapse = " "), ";")
  )

  Rcov <- switch(type,
    homogenous = indCov(x = x, r = r, e = e, collapse = collapse),
    heterogenous = indCov(x = x, r = r, e = e, collapse = collapse),
    cs = toeplitzCov(x = x, r = r, e = e, collapse = collapse, type = type),
    toeplitz = toeplitzCov(x = x, r = r, e = e, collapse = collapse, type = type),
    ar = toeplitzCov(x = x, r = r, e = e, collapse = collapse, type = type),
    un = lapply(indCov(x = x, r = r, e = e, collapse = collapse), function(x) {
      gsub("@0", "", x)})
  )

  Rstruc <- c(Variances = V, Rcov)
  allres <- do.call(paste, list(Rstruc, collapse = "\n"))
  Rstruc <- c(all = allres, Rstruc)

  class(Rstruc) <- "MplusRstructure"

  return(Rstruc)
}


#' Extract parameters from a data frame of Mplus estimates
#'
#' This is a simple convenience function designed to facilitate
#' looking at specific parameter types by easily return a subset
#' of a data frame with those types only. It is designed to follow up
#' the results returned from the \code{\link{readModels}} function.
#'
#' @param x A data frame (specifically the type returned by \code{readModels}) containing
#'   parameters. Should be specific such as unstandardized and the data frame must have a
#'   column called \sQuote{paramHeader}.
#' @param params A character string indicating the types of parameters to be returned.
#'   Options currently include \sQuote{regression}, \sQuote{loading}, \sQuote{undirected},
#'   \sQuote{expectation}, \sQuote{variability}, and \sQuote{new} for new/additional parameters.
#'   Regressions include regression of one variable
#'   \code{ON} another. \sQuote{loading} include indicator variables (which are assumed caused by the
#'   underlying latent variable) and variables in latent growth models (\code{BY} or \code{|}).
#'   Undirected paths currently only include covariances, indicated by the \code{WITH}
#'   syntax in Mplus. Expectation paths are the unconditional or conditional expectations of
#'   variables.  In other words those parameters related to the first moments. For independent
#'   variables, these are the means, \eqn{E(X)} and the conditional means or intercepts,
#'   \eqn{E(X | f(\theta))}{E(X | f(theta))} where \eqn{f(\theta)}{f(theta)} is the model,
#'   some function of the parameters, \eqn{\theta}{theta}. Finally \sQuote{variability}
#'   refers to both variances and residual variances, corresponding to the second moments.
#'   As with the expectations, variances are unconditional for variables that are not
#'   predicted or conditioned on any other variable in the model whereas residual variances
#'   are conditional on the model. Note that \R uses fuzzy matching so that each of these
#'   can be called via shorthand, \sQuote{r}, \sQuote{l}, \sQuote{u}, \sQuote{e}, and \sQuote{v}.
#' @return A subset data frame with the parameters of interest.
#' @seealso \code{\link{readModels}}
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @keywords utils
#' @export
#' @examples
#' \dontrun{
#'   test <- mplusObject(
#'     TITLE = "test the MplusAutomation Package and my Wrapper;",
#'     MODEL = "
#'       mpg ON wt hp;
#'       wt WITH hp;",
#'     usevariables = c("mpg", "wt", "hp"),
#'     rdata = mtcars)
#'
#'   res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)
#'
#'   # store just the unstandardized parameters in 'd'
#'   d <- res$results$parameters$unstandardized
#'   # extract just regression parameters
#'   paramExtract(d, "regression")
#'   # extract other types of parameters using shorthand
#'   paramExtract(d, "u")
#'   paramExtract(d, "e")
#'   paramExtract(d, "v")
#' }
paramExtract <- function(x, params = c("regression", "loading", "undirected", "expectation", "variability", "new")) {
  #readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex3.9.out")
  params <- match.arg(params)

  keys <- switch(params,
    regression = c("ON"),
    loading = c("BY", "\\|"),
    undirected = c("WITH"),
    expectation = c("Means", "Intercepts", "Thresholds"),
    variability = c("Variances", "Residual.Variances"),
    new = "New.Additional.Parameters")
  index <- sapply(keys, function(pattern) {
    grepl(paste0(".*", pattern, "$"), x[, "paramHeader"])
  })
  if (isTRUE(is.matrix(index))) {
    index <- rowSums(index) > 0
  } else {
    index <- sum(index) > 0
  }
  index <- which(index)

  # catch cases where there is nothing to extract
  if (isFALSE(length(index) > 0)) return(NULL)

  output <- x[index, , drop=FALSE]
  attr(output, "type") <- params

  return(output)
}


#' Check Mplus code for missing semicolons or too long lines.
#'
#' The function parses a character string containing Mplus code
#' and checks that every non blank line ends in either a colon or
#' a semicolon. In addition, it checks that every line is less than 90
#' characters, because Mplus ignores everything after 90 characters on a line
#' which can be a source of enigmatic errors.
#'
#' The function is fairly basic at the moment. It works by simply
#' removing blank space (spaces, tabs, etc.) and then if a line does not
#' terminate in a colon or semicolon, it returns a note and the line
#' number.  Optionally, it can add semicolons to any lines missing them
#' and return the input with added semicolons. To check for lines that are too long,
#' all trailing (but not before) white space is removed, and then the number of
#' characters is checked.
#'
#' @param x a character string containing Mplus code.
#' @param add logical indicating whether or not to add semicolons to
#'   lines that do not have them. Defaults to \code{FALSE}.
#' @return a character vector containing the input text and
#'   optionally added semicolons.
#' @seealso \code{\link{mplusModeler}}
#' @keywords utils
#' @export
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @examples
#'
#' # sample input
#' test <- "
#' MODEL:
#'   mpg ON wt hp;
#'   wt WITH hp
#' "
#' # check and return
#' cat(parseMplus(test), file=stdout(), fill=TRUE)
#' # add missing semicolons and return
#' cat(parseMplus(test, TRUE), file=stdout(), fill=TRUE)
#' # line that is too long for Mplus
#' test <- "
#' MODEL:
#'   mpg cyl disp hp drat wt qsec vs am gear PWITH cyl disp hp drat wt qsec vs am gear carb;
#' "
#' cat(parseMplus(test), file=stdout())
parseMplus <- function(x, add = FALSE) {
  cc <- textConnection(x)
  init <- readLines(cc) #need to close the connection explicitly
  close(cc)

  nospace <- gsub("[[:space:]]", "", init)
  empty <- nchar(nospace) < 1
  end <- grepl(".*[:;]$", nospace)
  semiok <- empty | end
  if (isFALSE(all(semiok))) {
    index <- which(!semiok)
    message(paste(c("The following lines are not empty and do not end in a : or ;.",
            paste(index, init[index], sep = ": ")), collapse = "\n"))
    if (isTRUE(add)) {
      init[index] <- paste0(init[index], ";")
      message("added semicolons ';' to all of the above lines")
    } else {
      message("Rerun with parseMplus(add = TRUE) to add semicolons to all lines")
    }
  }

  notrailspace <- gsub("[[:space:]]+$", "", init)
  lengthok <- nchar(notrailspace) <= 90
  if (isFALSE(all(lengthok))) {
    index <- which(!lengthok)
    message(paste(c("The following lines are longer than 90 characters",
            paste(index, init[index], sep = ": ")), collapse = "\n"))
    message("Mplus will ignore everything after the 90th character on a line.\n",
        "Consider breaking the line(s)")
  }

  if(isTRUE(all(semiok & lengthok))) message("All ok")
  return(paste(init, collapse = "\n"))
}

#' Remove variable name length warnings from Mplus output file
#'
#' This function is designed to remove warnings in Mplus output files
#' about variable names being greater than 8 characters. It replaces them
#' with a note that the warnings were removed and a count of how many warnings
#' were removed.
#'
#' This is an internal function and not meant to be directly called by the
#' user under most circumstances. It is called by \code{\link{mplusModeler}}
#' when the \code{varwarnings = FALSE} argument is used.
#'
#' @param file A file name (including path if necessary) to an Mplus output
#'   file.  Note that you must have read and write privileges on the file
#'   for this function to work properly.
#' @return Usually NULL. Called for the side effect of removing warnings in
#'   Mplus output files. If \code{\link{file.access}} testing for write permissions
#'   returns \code{FALSE}, a character string note that \code{rmVarWarnings}
#'   could not run.
#' @seealso \code{\link{mplusModeler}}
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @keywords internal
#' @examples
#' # to do
rmVarWarnings <- function(file) {
  dat <- readLines(file)

  init <- grep("Variable name contains more than 8 characters.", dat)

  stopifnot(isTRUE(all(
    grepl("^\\*", dat[init - 1]),
    grepl("^[[:space:]]*Only", dat[init + 1]),
    grepl("^[[:space:]]*Variable", dat[init + 2]))))

  index <- sort(rep(init, each = 4) + (-1:2L))

  dat[index[1]] <- sprintf("%d Mplus warnings about variable name length removed",
    length(init))

  dat <- dat[-index[-1]]

  if (isTRUE(file.access(file, mode = 2)==0)) {
    unlink(file)
    writeLines(dat, con = file)
  } else {
    return("Could not access file")
  }
}
