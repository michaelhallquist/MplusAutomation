#' Run Mplus Models Using Graphical Interface
#'
#' This function is provides a graphical user interface to the \code{runModels} function.
#' It uses Tcl/Tk to display a window in which the user can specify parameters for \code{runModels},
#' including the directory for runs, recursing through subdirectories, displaying output on the console,
#' and replacing existing outfiles.
#'
#' This function exists as a GUI wrapper for \code{runModels} and does not provide any distinct functionality.
#'
#' @param directory optional. The starting directory that will display in the dialog window. Defaults to the
#'    current working directory.
#' @param recursive optional. Whether the recursive checkbox should be checked when the window opens.
#'   \dQuote{0} for \code{FALSE}, \dQuote{1} for \code{TRUE}.
#' @param showOutput optional. Whether the show output checkbox should be checked when the window opens.
#'   \dQuote{0} for \code{FALSE}, \dQuote{1} for \code{TRUE}.
#' @param replaceOutfile optional. Whether the replace outfile checkbox should be checked when the window opens.
#'   \dQuote{0} for \code{FALSE}, \dQuote{1} for \code{TRUE}.
#' @param checkDate optional. Whether the check modified date checkbox should be checked when the window opens.
#'   \dQuote{0} for \code{FALSE}, \dQuote{1} for \code{TRUE}.
#' @param logFile optional. Whether the log file checkbox should be checked when the window opens.
#'   \dQuote{0} for \code{FALSE}, \dQuote{1} for \code{TRUE}.
#' @return None. Function is used to display user interface for running models.
#' @author Michael Hallquist
#' @seealso \code{\link{runModels}}
#' @keywords interface
#' @export
#' @examples
#' # interactive, none
runModels_Interactive <- function(directory=getwd(), recursive="0",
    showOutput="1", replaceOutfile="1", checkDate="0", logFile="1")
{
  if (!suppressWarnings(require(tcltk))) {
    stop("The tcltk package is absent. Interactive folder selection cannot function.")
  }

  #button handler functions
  onOK <- function() {
    #tkmessageBox(message="clicked ok")
    recursiveChecked <- as.logical(as.numeric(tclvalue(recursiveChecked)))
    showOutputChecked <- as.logical(as.numeric(tclvalue(showOutputChecked)))
    replaceOutfileChecked <- as.logical(as.numeric(tclvalue(replaceOutfileChecked)))
    checkDateChecked <- as.logical(as.numeric(tclvalue(checkDateChecked)))
    directory <- tclvalue(directoryVariable)
    logFileChecked <- as.logical(as.numeric(tclvalue(logFileChecked)))
    logFile_TCL <- tclvalue(logFile_TCL)

    #check date may only be checked if replace outfile is on.
    if (!replaceOutfileChecked) checkDateChecked <- FALSE

    if (replaceOutfileChecked && checkDateChecked) replaceOutfileStr <- "modifiedDate"
    else if (replaceOutfileChecked && !checkDateChecked) replaceOutfileStr <- "always"
    else if (!replaceOutfileChecked) replaceOutfileStr <- "never"

    #if log file unchecked, pass NULL
    if (!logFileChecked) logFile_TCL <- NULL

#    cat(paste("directory:", tclvalue(directoryVariable), "\nrecurseChecked: ", recursiveChecked,
#            "\nshowOutputChecked:", showOutputChecked,
#            "\nreplaceOutfileChecked:", replaceOutfileChecked, "\ncheckDateChecked:", checkDateChecked,
#            "\nlogFileChecked:", logFileChecked, "\nlogFilename:", as.character(logFile_TCL), "\n"
#            ))

    tkgrab.release(top)
    tkdestroy(top)

    runModels(directory=directory, recursive=recursiveChecked,
        showOutput=showOutputChecked, replaceOutfile=replaceOutfileStr, logFile=logFile_TCL)
  }
  onCancel <- function(){
    #tkgrab.release(top)
    tkdestroy(top)
    return()
  }
  onBrowse <- function(){
    #choose.dir is a prettier way to select directory
    #tclvalue(directoryVariable) <- tclvalue(tkchooseDirectory())
    if (.Platform$OS.type == "unix") runDir <- tclvalue(tkchooseDirectory())
    else runDir <- tclvalue(tclVar(choose.dir(tclvalue(directoryVariable), "Choose the Mplus Run Directory")))

    if (!runDir == "NA") {
      tclvalue(directoryVariable) <- runDir
      #if log file is still at its default (getwd() + "Mplus Run Models.log"), switch to run directory
      if (userSetLogFile == FALSE) tclvalue(logFile_TCL) <- file.path(runDir, "Mplus Run Models.log")
    }

  }
  onLogBrowse <- function(){
    splitPath <- splitFilePath(tclvalue(logFile_TCL))
    #logDir <- tclvalue(tkgetSaveFile(defaultextension="log", initialdir=chartr("/", "\\", splitPath$directory), initialfile=splitPath$filename))
    logDir <- tclvalue(tkgetSaveFile(defaultextension="log", initialdir=splitPath$directory, initialfile=splitPath$filename))
    if (!logDir == "") {
      tclvalue(logFile_TCL) <- logDir
      userSetLogFile <<- TRUE #needs to be assigned in parent env
    }
  }
  onReplace <- function() {
    curVal <- as.character(tclvalue(replaceOutfileChecked))
    #cat(paste("replace checked is:", curVal))
    if (curVal == "1") {
      tkconfigure(checkDateCB, state="!disabled")
      tkconfigure(checkDateLabel, state="!disabled")
    }
    else if (curVal == "0") {
      tkconfigure(checkDateCB, state="disabled")
      tkconfigure(checkDateLabel, state="disabled")
    }
    #; tclvalue(replaceChecked) <- "0" }
  }
  onLogcheck <- function() {
    curVal <- as.character(tclvalue(logFileChecked))
    #cat(paste("replace checked is:", curVal))
    if (curVal == "1") {
      tkconfigure(logField, state="!disabled")
      tkconfigure(BrowseLog.but, state="!disabled")
      tkconfigure(logFilenameLabel, state="!disabled")
      #tkgrid.configure(logFrame, row="5")
    }
    else if (curVal == "0") {
      tkconfigure(logField, state="disabled")
      tkconfigure(BrowseLog.but, state="disabled")
      tkconfigure(logFilenameLabel, state="disabled")
      #tkgrid.forget(logFrame)
    }
    #; tclvalue(replaceChecked) <- "0" }
  }

  top <- tktoplevel(borderwidth=10)
  tkgrid(ttklabel(top, text="Run Mplus Models", font="-size 14 -family Arial -weight bold"), sticky="w")

  tkwm.title(top, "Run Mplus Models")
  directoryVariable <- tclVar(directory)
  locationFrame <- ttkframe(top)
  locationField <- ttkentry(locationFrame, width="80", textvariable=directoryVariable)
  Browse.but <- ttkbutton(locationFrame, text="Browse", command=onBrowse)

  tkgrid(ttklabel(locationFrame,text="Target directory:"), locationField, Browse.but)
  tkgrid(locationFrame, sticky="w")

  optionsFrame <- ttklabelframe(top, text="Options", borderwidth=10, relief="groove")
  #optionsFrame <- ttkframe(top, borderwidth=20, relief="groove")
  #tkgrid(ttklabel(optionsFrame,text="Options", font="-weight bold -size 12"), sticky="w")
  recursiveChecked <- tclVar(recursive)
  recurseCB <- ttkcheckbutton(optionsFrame, variable=recursiveChecked)
  tkgrid(ttklabel(optionsFrame,text="Run models in nested subdirectories"), recurseCB, sticky="w")

  replaceOutfileChecked <- tclVar(replaceOutfile)
  replaceCB <- ttkcheckbutton(optionsFrame, variable=replaceOutfileChecked, command=onReplace)
  tkgrid(ttklabel(optionsFrame,text="Re-run models that have existing output files"), replaceCB, sticky="w")

  checkDateChecked <- tclVar(checkDate)
  if (as.logical(as.numeric(replaceOutfile)) == TRUE) initialState <- "!disabled"
  else initialState <- "disabled"
  checkDateLabel <- ttklabel(optionsFrame, text="    -> Only re-run if input file is newer than existing output (check date)   ", state=initialState)
  checkDateCB <- ttkcheckbutton(optionsFrame, variable=checkDateChecked, state=initialState)
  tkgrid(checkDateLabel, checkDateCB, sticky="w")

  showOutputChecked <- tclVar(showOutput)
  showOutputCB <- ttkcheckbutton(optionsFrame, variable=showOutputChecked)
  tkgrid(ttklabel(optionsFrame,text="Show Mplus output on console"), showOutputCB, sticky="w")

  #include log file checkbox in options frame
  logFileChecked <- tclVar(logFile)
  logFileCB <- ttkcheckbutton(optionsFrame, variable=logFileChecked, command=onLogcheck)
  tkgrid(ttklabel(optionsFrame, text="Log Mplus Run Details to File"), logFileCB, sticky="w")

  #add the options frame to the dialog
  tkgrid(optionsFrame, sticky="w")

  #setup the filename field for the log file
  logFile_TCL <- tclVar(file.path(directory, "Mplus Run Models.log"))
  userSetLogFile <- FALSE
  logFrame <- ttkframe(optionsFrame) #borderwidth="5m")
  if (as.logical(as.numeric(logFile)) == TRUE) initialState <- "!disabled"
  else initialState <- "disabled"
  logField <- ttkentry(logFrame, width="50", textvariable=logFile_TCL, state=initialState)
  logFilenameLabel <- ttklabel(logFrame,text="    -> Log filename:", state=initialState)
  BrowseLog.but <- ttkbutton(logFrame, text="Browse", command=onLogBrowse, state=initialState)

  #add log frame to grid
  tkgrid(logFilenameLabel, logField, BrowseLog.but)
  tkgrid(logFrame, sticky="w", columnspan="2")


  #add button frame to grid
  buttonsFrame <- ttkframe(top)
  OK.but <- ttkbutton(buttonsFrame,text="OK",command=onOK)
  Cancel.but <- ttkbutton(buttonsFrame, text="Cancel", command=onCancel)
  tkgrid(OK.but, ttklabel(buttonsFrame,text="   "), Cancel.but, sticky="w")
  tkgrid(buttonsFrame, sticky="w")

  #tkgrid.size(top)

  tkwm.resizable(top, 0, 0)
  tkbind(top, "<Return>", onOK)
  tkbind(top, "<Escape>", onCancel)
  tkwm.deiconify(top)
  #tkgrab.set(top)
  tkfocus(top)
  tkwait.window(top)


}

#' Run Mplus Models
#'
#' This function runs a group of Mplus models (.inp files) located within a
#' single directory or nested within subdirectories.
#'
#' @param directory the directory containing Mplus input files (.inp) to run.
#'   Defaults to the current working directory. Example: \dQuote{C:/Users/Michael/Mplus Runs}
#' @param recursive optional. If \code{TRUE}, run all models nested in subdirectories
#'   within \code{directory}. Defaults to \code{FALSE}.
#' @param filefilter a Perl regular expression (PCRE-compatible) specifying particular input
#'   files to be run within \code{directory}. See \code{regex} or \url{http://www.pcre.org/pcre.txt}
#'   for details about regular expression syntax.
#' @param showOutput optional. If \code{TRUE}, show estimation output (TECH8)
#'   in the R console. Note that if run within Rgui, output will display within R,
#'   but if run via Rterm, a separate window will appear during estimation.
#' @param replaceOutfile optional. Currently supports three settings: \dQuote{always}, which
#'   runs all models, regardless of whether an output file for the model exists; \dQuote{never},
#'   which does not run any model that has an existing output file; and \dQuote{modifiedDate}, which
#'   only runs a model if the modified date for the input file is more recent than
#'   the output file modified date (implying there have been updates to the model).
#' @param logFile optional. If non-null, specifies a file (and optionally, directory)
#'   that records the settings passed into the function and the models run (or skipped)
#'   during the run.
#' @param Mplus_command optional. N.B.: No need to pass this parameter for most users (has intelligent
#'   defaults). Allows the user to specify the name/path of the Mplus executable to be used for
#'   running models. This covers situations where Mplus is not in the system's path,
#'   or where one wants to test different versions of the Mplus program.
#' @param killOnFail optional. Windows only for now. If \code{TRUE}, kill all processes named mplus.exe when 
#'   \code{runModels} does not terminate normally. Defaults to \code{TRUE}.
#'
#' @return None. Function is used for its side effects (running models).
#' @author Michael Hallquist
#' @seealso \code{\link{runModels_Interactive}}
#' @keywords interface
#' @export
#' @examples
#' \dontrun{
#'   runModels("C:/Users/Michael/Mplus Runs", recursive=TRUE, showOutput=TRUE,
#'     replaceOutfile="modifiedDate", logFile="MH_RunLog.txt",
#'     Mplus_command="C:\\Users\\Michael\\Mplus Install\\Mplus51.exe")
#' }
runModels <- function(directory=getwd(), recursive=FALSE, filefilter = NULL, showOutput=FALSE,
	replaceOutfile="always", logFile="Mplus Run Models.log", Mplus_command="Mplus", killOnFail=TRUE) {

  #retain working directory and reset at end of run
  #need to set here to ensure that logTarget initialization below is within target directory, not getwd()
  curdir <- getwd()
  directory <- sub("(\\\\|/)?$", "", directory, perl=TRUE) #remove trailing slash, which generates file.exists error on windows: https://bugs.r-project.org/bugzilla/show_bug.cgi?id=14721
  if (!file.exists(directory)) { stop("runModels cannot change to directory: ", directory)}
  setwd(directory)
  normalComplete <- FALSE

  #if log file requested, then open file connection for writing
  if (!is.null(logFile)) {
    logTarget <- file(description = logFile, open = "wt", blocking = TRUE)
    writeLines(c(paste("------Begin Mplus Model Run: ", format(Sys.time(), "%d%b%Y %H:%M:%S"), "------", sep=""),
            paste("Target directory: ", directory, sep=""),
            "Run options:",
            paste("\tRecursive (run models in subdirectories):", as.character(recursive)),
            paste("\tShow output on console:", as.character(showOutput)),
            paste("\tReplace existing outfile:", replaceOutfile),
            "------"
        ), con=logTarget)
    #need to flush after each writeLines to keep the text file current.
    flush(logTarget)
  }

  isLogOpen <- function() {
    #if null is passed as the log file, then it is by definition not open (non-existent)
    if (is.null(logFile)) return(FALSE)

    connections <- data.frame(showConnections(all = FALSE))
    if (length(grep(splitFilePath(logFile)$filename, connections$description, ignore.case=TRUE)) > 0) return(TRUE)
    else return(FALSE)
  }

  #if the function gets interrupted (e.g., the user presses escape), kill the Mplus process (doesn't happen automatically).
  exitRun <- function() {
    deleteOnKill <- TRUE #whether to delete unfinished output
    
    if (normalComplete == FALSE && isLogOpen()) {
      writeLines("Run terminated abnormally", logTarget)
      flush(logTarget)
    }

    #create a data frame consisting of the process names and pids
    #uses str split on the output of wmic to extract name and pid columns
    #depends on windows tools
    if (.Platform$OS.type == "windows" && normalComplete == FALSE && killOnFail == TRUE) {
      processList <- ldply(strsplit(shell("wmic process get caption, processid", intern=TRUE), split="\\s+", perl=TRUE),
          function(element) {
            return(data.frame(procname=element[1], pid=element[2], stringsAsFactors = FALSE))
          })

      if(length(grep("mplus.exe", processList$procname, ignore.case=TRUE)) > 0) {
        if(isLogOpen()) {
          writeLines("Killing wayward Mplus processes", logTarget)
          flush(logTarget)
        }
        shell("taskkill /f /im mplus.exe")

        #if the process is currently running and we kill it, then the output and gph files will be incomplete.
        #in general, it would be good to delete these.
        if(deleteOnKill == TRUE) {
          noExtension <- substr(absFilename, length(absFilename) - 4, length(absFilename))
          outDelete <- paste(noExtension, ".out", sep="")
          gphDelete <- paste(noExtension, ".gph", sep="")
          if (file.exists(outDelete)) {
            unlink(outDelete)
            if(isLogOpen()) {
              writeLines(paste("Deleting unfinished output file:", outDelete), logTarget)
              flush(logTarget)
            }
          }
          if (file.exists(gphDelete)) {
            unlink(gphDelete)
            if(isLogOpen()) {
              writeLines(paste("Deleting unfinished graph file:", gphDelete), logTarget)
              flush(logTarget)
            }
          }
        }
      }
    }

    #close logfile
    if (isLogOpen()) { close(logTarget) }

    #reset working directory
    setwd(curdir)
  } #end exitRun definition

  on.exit(exitRun())

  #list files in the current directory
  filelist <- list.files(recursive=recursive, pattern=filefilter)

  #select only .inp files using grep
  inpfiles <- filelist[grep(".*\\.inp$", filelist, ignore.case=TRUE)]
  outfiles <- filelist[grep(".*\\.out$", filelist, ignore.case=TRUE)]

  if(length(inpfiles) < 1) stop("No Mplus input files detected in the target directory: ", directory)

  dropOutExtensions <- sapply(outfiles, function(x) {
        if (nchar(x) >= 4) return(tolower(substr(x, 1, (nchar(x)-4))))
      })

  for (i in 1:length(inpfiles)) {

    if (!replaceOutfile == "always") {
      #if there is a match in the outfiles for this input file, then decide whether to skip
      if (tolower(substr(inpfiles[i], 1, (nchar(inpfiles[i]) - 4))) %in% dropOutExtensions) {

        if (replaceOutfile == "modifiedDate") {
          #if check date is true, then the output file must exist and it must be
          #older than the input file to re-run
          inpmtime <- file.info(inpfiles[i])$mtime

          #need to locate the exact outfile match
          matchPos <- grep(tolower(substr(inpfiles[i], 1, (nchar(inpfiles[i]) - 4))), dropOutExtensions)
          if (length(matchPos) < 1) warning("Could not locate matching outfile")
          outmtime <- file.info(outfiles[matchPos[1]])$mtime

          if (inpmtime <= outmtime) {
            if (isLogOpen()) {
              writeLines(paste("Skipping model because output file is newer than input file:", inpfiles[i]), logTarget)
              flush(logTarget)
            }
            next
          }
        }
        else if (replaceOutfile == "never"){
          if (isLogOpen()) {
            writeLines(paste("Skipping model because output file already exists:", inpfiles[i]), logTarget)
            flush(logTarget)
          }
          next
        }
      }
    }

    #split input file into one element per directory (e.g., "dir1/dir2/mytest.inp" becomes a 3-element vector
    #code adapted from R.utils filePath command
    inputSplit <- splitFilePath(inpfiles[i])
    if (is.na(inputSplit$directory)) dirtocd <- directory
    else dirtocd <- file.path(directory, inputSplit$directory)

    #the absolute path to the file combines the passed-in directory with the subdirectories
    #identified in the case of recursive=T

    absFilename <- file.path(directory, inpfiles[i])

    #UPDATE 21Oct2011: Since mplus has been released for linux, don't default to wine.
    if (.Platform$OS.type == "unix" && Mplus_command == "Mplus") {
      if (Sys.info()["sysname"] == "Darwin") Mplus_command <- "/Applications/Mplus/mplus"
      else Mplus_command <- "mplus" #linux is case sensitive
    }

    #navigate to working directory in DOS using cd command so that Mplus finds the appropriate files (support rel paths)
    #switched over to use relative filename because of problems in Mplus via Wine misinterpreting absolute paths due to forward slashes.
    #25Jul2012: Quote Mplus_command in case it's in a path with spaces.
    command <- paste("cd \"", dirtocd, "\" && \"", Mplus_command, "\" \"", inputSplit$filename, "\"", sep="")

    #allow for divergence if the package is being run in Linux (Mplus via wine)
    if (.Platform$OS.type == "windows") {
      #Given that Mplus is a Windows program, should generally automatically remap / as \ for Windows
      #remap forward slashes to backslashes
      command <- chartr("/", "\\", command)

      #code swiped from shell because shell didn't support suppressing output
      shellcommand <- Sys.getenv("COMSPEC")
      flag <- "/c"

      #assemble full command from the shell call, flag, and command
      command <- paste(shellcommand, flag, command)
    }
    else if (.Platform$OS.type == "unix") {
      #allow for unix-specific customization here
    }

    if (isLogOpen()) {
      writeLines(paste("Currently running model:", inputSplit$filename), logTarget)
      flush(logTarget)
    }

    #run the model
    cat("\nRunning model:", inputSplit$filename, "\n")
    cat("System command:", command, "\n")

    #unix system command does not have show.output.on.console or invisible parameters
    if (.Platform$OS.type == "windows")	{
      system(command, show.output.on.console = showOutput, invisible=(!showOutput), wait=TRUE)
    } else {
      if(showOutput) stdout.value = ""
      else stdout.value = NULL
      #need to switch to each directory, then run Mplus within using just the filename
      oldwd <- getwd()
      setwd(dirtocd)
      exitCode <- system2(Mplus_command, args=c(shQuote(inputSplit$filename)), stdout=stdout.value, wait=TRUE)
      if (exitCode > 0L) {
        warning("Mplus returned error code: ", exitCode, ", for model: ", inputSplit$filename, "\n")
      }
      setwd(oldwd)
    }
  }

  if (isLogOpen()) {
    writeLines(c("", paste("------End Mplus Model Run: ", format(Sys.time(), "%d%b%Y %H:%M:%S"), "------", sep="")), logTarget)
    flush(logTarget)
  }
  normalComplete <- TRUE #if we made it here, then all completed normally

  #exitRun will fire here if all successful.
}
