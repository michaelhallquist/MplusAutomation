#' Create an mplusModel object for a given model
#' @param syntax a character vector of Mplus input syntax for this model
#' @param data a data.frame to be used for estimating the model
#' @param inp_file the location of .inp file for this model
#' @param read If TRUE and the .out file already exists, read the contents of the .out file using `readModels`
#' @param Mplus_command The location of the Mplus executable to run. If NULL, use `detectMplus()`
#' @return a `mplusModel_r6` object containing information about the model
#' @export
mplusModel <- function(syntax=NULL, data=NULL, inp_file=NULL, read=TRUE, Mplus_command = NULL) {
  # simple wrapper around class constructor
  mobj <- mplusModel_r6$new(syntax, data, inp_file, read, Mplus_command)
  
  return(mobj)
}

#' An R6 class for a single Mplus model
#' @details
#'   Wrapped by `mplusModel`
#'   Note that this R6 class deliberately uses unlockBinding to populate private fields after the object is instantiated.
#'   This allows the model outputs to be added to the relevant sections of the object while keeping the fields private.
#' @keywords internal
mplusModel_r6 <- R6::R6Class(
  classname = "mplusModel_r6",
  lock_objects=FALSE,
  private=list(
    pvt_output_loaded = FALSE,
    pvt_syntax = NULL,           # syntax for this model, parsed into a list
    pvt_data = NULL,             # data.frame containing data for model estimation
    pvt_inp_file = NULL,         # name of .inp file
    pvt_out_file = NULL,         # name of .out file
    pvt_dat_file = NULL,         # name of .dat file
    pvt_model_dir = NULL,        # location of model files
    pvt_Mplus_command = NULL,    # location of Mplus binary
    pvt_variables = NULL,        # names of columns in data to be written to the .dat file
    pvt_is_montecarlo = FALSE,   # whether this is a monte carlo model (in which case the data section is irrelevant)
    
    # private method to populate fields using the result of readModels
    populate_output = function(o) {
      private$pvt_output_loaded <- TRUE
      for (ff in c("parameters", "input", "warnings", "summaries", "savedata")) {
      #for (ff in names(o)) {
        unlockBinding(ff, self)
        self[[ff]] <- o[[ff]]
        lockBinding(ff, self)
      }
    },
    
    # private method to clear all fields after the object is invalidated (e.g., by changing the inp_file or data)
    clear_output = function() {
      private$pvt_output_loaded <- FALSE
      for (ff in c("parameters", "input", "warnings", "summaries", "savedata")) {
        #for (ff in names(o)) {
        unlockBinding(ff, self)
        self[[ff]] <- NULL
        lockBinding(ff, self)
      }
    },
    
    detect_variables = function() {
      # both syntax and data must be present to attempt detection
      if (is.null(private$pvt_syntax) || is.null(private$pvt_data)) return(invisible(NULL))
      
      # TODO: make variable detection better... detectVariables misses IDVARIABLE and CLUSTER, for example
      # and we need to convert the pvt_syntax sections to strings to match detectVariables

      # this is preferred for now: use user-specified names
      if (!is.null(private$pvt_syntax$variable$names)) {
        private$pvt_variables <- strsplit(private$pvt_syntax$variable$names, "\\s+")[[1L]]
      } else {
        # mimic mplusObject to use detectVariables
        obj <- list()
        obj$MODEL <- private$pvt_syntax$model
        obj$DEFINE <- private$pvt_syntax$define
        obj$VARIABLE <- private$pvt_syntax$variable
        obj$rdata <- private$pvt_data

        private$pvt_variables <- detectVariables(obj)
      }      
    }
  ),
  active = list(
    #' @field model_dir the directory for Mplus files corresponding to this model
    model_dir = function(value) {
      if (missing(value)) {
        private$pvt_model_dir
      } else {
        if (!dir.exists(value)) dir.create(value, recursive = TRUE)
        private$pvt_model_dir <- value
      }
    },
    
    #' @field inp_file the location of the Mplus .inp file for this model
    inp_file = function(value) {
      if (missing(value)) file.path(private$pvt_model_dir, private$pvt_inp_file)
      else stop("Cannot set read-only field")
    },
    
    #' @field out_file the location of the Mplus .out file for this model
    out_file = function(value) {
      if (missing(value)) file.path(private$pvt_model_dir, private$pvt_out_file)
      else stop("Cannot set read-only field")
    },
    
    #' @field dat_file the location of the Mplus .dat (data) file for this model
    dat_file = function(value)  {
      if (missing(value)) file.path(private$pvt_model_dir, private$pvt_dat_file)
      else stop("Cannot set read-only field")
    },
    
    #' @field data the dataset used for estimating this model
    data = function(value) {
      if (missing(value)) {
        private$pvt_data
      } else {
        had_data <- !is.null(private$pvt_data) # is the user updating data for an existing model?
        if (is.null(value)) {
          private$pvt_data <- NULL # unset data
        } else if (checkmate::test_data_table(value)) {
          private$pvt_data <- as.data.frame(value)
        } else if (checkmate::test_data_frame(value)) {
          private$pvt_data <- value
        } else {
          stop("data must be a data.frame object")
        }
        
        if (had_data) {
          # invalidate loaded output if data changes
          message("Data has changed. Unloading model results from object.")
          private$clear_output()  
        }
        
        # if we are updating the dataset, make sure that all expected variables are present
        if (!is.null(value) && !is.null(private$pvt_variables)) {
          missing_vars <- setdiff(private$pvt_variables, names(value))
          if (length(missing_vars) > 0L)
            warning("The following variables are mentioned in the model syntax, but missing in the data: ",
                    paste(missing_vars, collapse=", "))
        }
      }
    },
    
    #' @field Mplus_command the location of the Mplus program
    Mplus_command = function(value) {
      if (missing(value)) {
        private$pvt_Mplus_command
      } else {
        if (is.null(value)) {
          private$pvt_Mplus_command <- MplusAutomation::detectMplus() # default
        } else if (!checkmate::test_string(value)) {
          warning("Mplus_command must be a character string pointing to the location of Mplus")
        } else {
          private$pvt_Mplus_command <- value
          if (!checkmate::test_file_exists(value)) warning("Mplus_command does not point to a valid location. This could cause problems!")
        }
      }
    },
    
    #' @field syntax the Mplus syntax for this model as a character vector
    syntax = function(value) {
      if (missing(value)) {
        mplusInpToString(private$pvt_syntax)
        #unname(unlist(private$pvt_syntax)) # unlist to return as character
      } else {
        if (!checkmate::test_character(value)) {
          stop("Syntax must be a character vector")
        } else {
          private$pvt_syntax <- parseMplusSyntax(value, dropSectionNames = TRUE)

          # detect variables in syntax
          private$detect_variables()

          if (!is.null(private$pvt_data) && !is.null(private$pvt_variables)) {
            missing_vars <- setdiff(private$pvt_variables, names(private$pvt_data))
            if (length(missing_vars) > 0L)
              warning("The following variables are mentioned in the model syntax, but missing in the data: ",
                      paste(missing_vars, collapse=", "))
          }

          # determine whether this is a montecarlo model
          private$pvt_is_montecarlo <- any(grepl("^\\s*montecarlo\\s*:", value, ignore.case = TRUE, perl=TRUE))
        }
      }
    }
  ),
  public=list(
    
    ### READ-ONLY FIELDS (set by populate_output)
    #' @field input Mplus input syntax parsed into a list by major section
    input = NULL,
    
    #' @field warnings Syntax and estimation warnings as a list
    warnings = NULL,
    
    #' @field parameters a list containing the parameter estimates for the model
    parameters = NULL,
    
    #' @field summaries a list containing the model summary information and statistics
    summaries = NULL,
    
    #' @field savedata a list containing the data output by the SAVEDATA command
    savedata = NULL,
    
    #' @description generate an mplusModel_r6 object
    #' @param syntax a character vector of Mplus input syntax for this model
    #' @param data a data.frame to be used for estimating the model
    #' @param inp_file the location of .inp file for this model
    #' @param read If TRUE and the .out file already exists, read the contents of the .out file using `readModels`
    #' @param Mplus_command N.B.: No need to pass this parameter for most users (has intelligent defaults). Allows the user to 
    #'   specify the name/path of the Mplus executable to be used for running models. This covers situations where Mplus
    #'   is not in the system's path, or where one wants to test different versions of the Mplus program.
    initialize = function(syntax=NULL, data=NULL, inp_file=NULL, read=TRUE, Mplus_command = NULL) {
      checkmate::assert_flag(read)
      
      # look for extant inp file and read the corresponding output if requested
      if (!is.null(inp_file)) {
        # if (!file.exists(inp_file)) stop("inp_file does does not exist: ", inp_file)
        checkmate::assert_string(inp_file)
        s <- splitFilePath(inp_file, normalize=TRUE)
        private$pvt_model_dir <- ifelse(is.na(s$directory), getwd(), s$directory)
        private$pvt_inp_file <- s$filename
        private$pvt_out_file <- sub("\\.inp?$", ".out", s$filename)
        
        if (file.exists(self$out_file) && isTRUE(read)) self$read() # load the .out file if requested
        
        # if a syntax string is not passed in, set the syntax string to be the contents of the extant input file
        if (is.null(syntax) && file.exists(self$inp_file)) {
          syntax <- readLines(self$inp_file)
        }
        
        # if data is not provided, but the .out file is provided, attempt to read the data
        if (is.null(data) && private$pvt_output_loaded && !is.null(self$input$data$file)) {
          dfile <- self$input$data$file
          private$pvt_dat_file <- self$input$data$file
          
          # if file cannot be loaded as is because of a relative path problem, look in the model directory
          if (!file.exists(dfile)) {
            dfile <- file.path(private$pvt_model_dir, dfile)
          }
          data <- tryCatch(data.table::fread(dfile, header = FALSE, na.strings=c("*", "."), strip.white=TRUE, data.table = FALSE),
                           error=function(e) { warning("Could not load data file: ", dfile); return(NULL) })
          
          # set the names of the data if read succeeds
          if (!is.null(data)) names(data) <- strsplit(expandCmd(self$input$variable$names), "\\s+")[[1]]
        }
      }
      
      # populate model syntax
      if (!checkmate::test_character(syntax)) stop("syntax argument must not be NULL")
      
      # populate model data
      self$data <- data
      
      # force syntax to be a character vector (convert \n to elements)
      self$syntax <- unlist(strsplit(syntax, "\\n"))
      
      # set default data file name
      if (isFALSE(private$pvt_is_montecarlo) && is.null(private$pvt_dat_file) && !is.null(private$pvt_inp_file)) {
        private$pvt_dat_file <- sub("\\.inp?$", ".dat", private$pvt_inp_file)
      }
      
      # set Mplus command
      self$Mplus_command <- Mplus_command
      
      ### args from submitModels
      # function(target=getwd(), recursive=FALSE, filefilter = NULL,
      #                          replaceOutfile="modifiedDate", Mplus_command = NULL, quiet = FALSE,
      #                          scheduler="slurm", sched_args=NULL, env_variables=NULL, export_all=FALSE,
      #                          cores_per_model = 1L, memgb_per_model = 8L, time_per_model="1:00:00",
      #                          time_per_command = NULL, pre=NULL, post=NULL, batch_outdir=NULL, job_script_prefix=NULL, 
      #                          debug = FALSE, fail_on_error = TRUE
    },
    
    #' @description read the results of an Mplus model from the .out file using `readModels`
    #' @param force if `TRUE`, re-read the .out file even if was already previously loaded in to this object
    read = function(force=FALSE) {
      checkmate::assert_flag(force)
      if ((force || !private$pvt_output_loaded) && file.exists(self$out_file)) {
        o <- MplusAutomation::readModels(self$out_file)
        private$populate_output(o)
      }
    },
    
    #' @description write the .inp and .dat files for this model to the intended location
    #' @param overwrite if `TRUE`, overwrite existing data. Default: `TRUE`.
    #' @param quiet if `TRUE`, do not produce messages about the outcome of this command (e.g., a message about overwriting existing data)
    #' @param ... additional arguments passed to `prepareMplusData`
    write_dat = function(overwrite = TRUE, quiet = FALSE, ...) {
      checkmate::assert_flag(overwrite)
      checkmate::assert_flag(quiet)
      
      # montecarlo models do not have data files
      if (private$pvt_is_montecarlo) return(invisible(self))
      
      if (is.null(self$data)) stop("Cannot write data to file because this object has no data.")
      
      if (file.exists(self$dat_file) && isFALSE(overwrite)) {
        if (!quiet) message("Not overwriting existing data file: ", self$dat_file)
        return(invisible(self))
      }

      inp_syntax <- prepareMplusData(df = self$data, filename = self$dat_file, keepCols=private$pvt_variables, quiet = TRUE, use_relative_path = TRUE, ...)
      
      # ensure that the variable names in the syntax match what we detected
      private$pvt_syntax$variable$names <- attr(inp_syntax, "variable_names")
      private$pvt_syntax$variable$missing <- attr(inp_syntax, "missing")
      private$pvt_syntax$data$file <- attr(inp_syntax, "data_file") # update FILE = in DATA section to match absolute/relative (NB, this is overridden in write_inp... need to unify)

      if (!quiet) message("Writing data to file: ", self$dat_file)
      return(invisible(self))
    },
    
    #' @description write the .inp and .dat files for this model to the intended location
    #' @param overwrite if `TRUE`, overwrite existing data. Default: `TRUE`.
    #' @param inp_file The location of the input file to write. If NULL (default), use the `$inp_file` of this object.
    #' @param quiet if `TRUE`, do not produce messages about the outcome of this command (e.g., a message about overwriting existing data)
    write_inp = function(overwrite = TRUE, inp_file = NULL, quiet = FALSE) {
      checkmate::assert_flag(overwrite)
      checkmate::assert_string(inp_file, null.ok = TRUE)
      checkmate::assert_flag(quiet)
      write <- TRUE
      
      if (is.null(inp_file)) inp_file <- self$inp_file
      
      # always ensure that the data file in the syntax matches the internal object location
      # and apply wrapping at 75 characters to avoid > 90 errors
      # we also need to add quotations to get Mplus to handle multi-line read properly
      # currently forcing this to refer to the relative path (since the goal is always to have .dat files match location)
      if (!is.null(private$pvt_syntax$data$file)) private$pvt_syntax$data$file <- paste0("\"", trimws(gsub('(.{1,75})(\\s|$|/|\\\\)', '\\1\\2\n', basename(self$dat_file))), "\"")
      
      # if the inp file exists, compare its contents against the user's syntax
      if (file.exists(self$inp_file)) {
        new_md5 <- digest::digest(self$syntax, algo="md5", serialize=FALSE)
        
        # for some reason, the file = TRUE approach fails under different variants
        # ext_md5 <- digest::digest(self$inp_file, algo="md5", file=TRUE, serialize=FALSE, ascii=TRUE)
        ext_md5 <- digest::digest(readLines(self$inp_file), algo="md5", file=FALSE, serialize=FALSE)
        
        if (new_md5 != ext_md5 && isFALSE(overwrite)) {
          write <- FALSE
          if (!quiet) message("Not overwriting existing .inp file: ", self$inp_file)
        }
      }
      
      if (write) {
        if (!quiet) message("Writing Mplus syntax to file: ", self$inp_file)
        writeLines(self$syntax, con = self$inp_file)
      }
      return(invisible(self))
    },
    
    #' @description submit this model for estimation on an HPC using `submitModels`
    #' @param replaceOutfile Currently supports three settings: “always”, which runs all models, regardless of whether an output file for the model exists; 
    #'   “never”, which does not run any model that has an existing output file; and “modifiedDate”, which only runs a model if the modified date for the input
    #'   file is more recent than the output file modified date (implying there have been updates to the model).
    #' @param ... additional arguments passed to `submitModels`
    submit = function(replaceOutfile = "modifiedDate", ...) {
      self$write_dat()
      self$write_inp()
      submitModels(target = self$inp_file, replaceOutfile = replaceOutfile, Mplus_command = self$Mplus_command, ...)
    },
    
    #' @description run this model locally using `runModels`
    #' @param replaceOutfile Currently supports three settings: “always”, which runs all models, regardless of whether an output file for the model exists; 
    #'   “never”, which does not run any model that has an existing output file; and “modifiedDate”, which only runs a model if the modified date for the input
    #'   file is more recent than the output file modified date (implying there have been updates to the model).
    #' @param ... additional arguments passed to `runModels`
    run = function(replaceOutfile = "modifiedDate", ...) {
      # TODO: only write inp and dat files if things have changed compared to disk
      self$write_dat()
      self$write_inp()
      runModels(target = self$inp_file, replaceOutfile = replaceOutfile, Mplus_command = self$Mplus_command, ...)
      self$read(force = TRUE) # read/re-read after estimation

    }
  )
)

# small utility function to join strings in a regexp loop
joinRegexExpand <- function(cmd, argExpand, matches, iterator, matchLength = "match.length") {
  if (iterator == 1 && matches[iterator] > 1) {
    pre <- substr(cmd, 1, matches[iterator] - 1)
  } else {
    pre <- ""
  }
  
  # if this is not the final match, then get sub-string between the end of this match and the beginning of the next
  # otherwise, match to the end of the command
  post.end <- ifelse(iterator < length(matches), matches[iterator + 1] - 1, nchar(cmd))
  post <- substr(cmd, matches[iterator] + attr(matches, matchLength)[iterator], post.end)
  
  cmd.expand <- paste(pre, argExpand, post, sep = "")
  return(cmd.expand)
}


# expand Mplus hyphen syntax (will also expand constraints with hyphens)
expandCmd <- function(cmd) {
  # use negative lookahead and negative lookbehind to eliminate possibility of hyphen being used as a negative starting value (e.g., x*-1)
  # also avoid match of anything that includes a decimal point, such as a floating-point starting value -10.5*x1
  
  # need to do a better job of this so that u1-u20* is supported... I don't think the regexp below is general enough
  
  # hyphens <- gregexpr("(?!<(\\*|\\.))\\w+(?!(\\*|\\.))\\s*-\\s*(?!<(\\*|\\.))\\w+(?!(\\*|\\.))", cmd, perl=TRUE)[[1]]
  
  # support trailing @XXX. Still still fail on Trait1-Trait3*XXX
  hyphens <- gregexpr("(?!<(\\*|\\.))\\w+(?!(\\*|\\.))\\s*-\\s*(?!<(\\*|\\.))\\w+(?!(\\*|\\.))(@[\\d\\.\\-]+)?", cmd, perl = TRUE)[[1]]
  
  #hyphens <- unlist(gregexpr('-', cmd))
  #hyphens <- gregexpr("(\\w)(\\d+)-(\\w)(\\d+)", cmd, perl=TRUE)[[1]]
  
  # Promising, but this is still failing in the case of x3*1 -4.25*x4
  # On either side of a hyphen, require alpha character followed by alphanumeric
  # This enforces that neither side of the hyphen can be a number
  # Alternatively, match digits on either side alone
  # hyphens <- gregexpr("([A-z]+\\w*\\s*-\\s*[A-z]+\\w*(@[\\d\\.-]+)?|\\d+\\s*-\\s*\\d+)", cmd, perl=TRUE)[[1]]
  
  if (hyphens[1L] > 0) {
    cmd.expand <- c()
    ep <- 1
    
    for (v in 1:length(hyphens)) {
      # match one keyword before and after hyphen
      argsplit <- strsplit(substr(cmd, hyphens[v], hyphens[v] + attr(hyphens, "match.length")[v] - 1), "\\s*-\\s*", perl = TRUE)[[1]]
      
      v_pre <- argsplit[1]
      v_post <- argsplit[2]
      
      v_post.suffix <- sub("^([^@\\*]+)([@\\*][\\d\\-.]+)?$", "\\2", v_post, perl = TRUE) # will be empty string if not present
      v_post <- sub("[@\\*][\\d\\-.]+$", "", v_post, perl = TRUE) # trim @ suffix
      
      # If v_pre and v_post contain leading alpha characters, verify that these prefixes match.
      # Otherwise, there is nothing to expand, as in the case of MODEL CONSTRAINT: e1e2=e1-e2_n.
      v_pre.alpha <- sub("\\d+$", "", v_pre, perl = TRUE)
      v_post.alpha <- sub("\\d+$", "", v_post, perl = TRUE)
      
      # only enforce prefix match if we have leading alpha characters (i.e., not simple numeric 1 - 3 syntax)
      if (length(v_pre.alpha) > 0L && length(v_post.alpha) > 0L) {
        # if alpha prefixes do match, assume that the hyphen is not for expansion (e.g., in subtraction case)
        if (v_pre.alpha != v_post.alpha) {
          return(cmd)
        }
      }
      
      # the basic positive lookbehind blows up with pure numeric constraints (1 - 3) because no alpha char precedes digit
      # can use an non-capturing alternation grouping to allow for digits only or the final digits after alphas (as in v_post.num)
      v_pre.num <- as.integer(sub("\\w*(?<=[A-Za-z_])(\\d+)$", "\\1", v_pre, perl = TRUE)) # use positive lookbehind to avoid greedy \w+ match -- capture all digits
      
      v_post.match <- regexpr("^(?:\\w*(?<=[A-Za-z_])(\\d+)|(\\d+))$", v_post, perl = TRUE)
      stopifnot(v_post.match[1L] > 0)
      
      # match mat be under capture[1] or capture[2] because of alternation above
      whichCapture <- which(attr(v_post.match, "capture.start") > 0)
      
      v_post.num <- as.integer(substr(v_post, attr(v_post.match, "capture.start")[whichCapture], attr(v_post.match, "capture.start")[whichCapture] + attr(v_post.match, "capture.length")[whichCapture] - 1))
      v_post.prefix <- substr(v_post, 1, attr(v_post.match, "capture.start")[whichCapture] - 1) # just trusting that pre and post match
      
      if (is.na(v_pre.num) || is.na(v_post.num)) stop("Cannot expand variables: ", v_pre, ", ", v_post)
      v_expand <- paste(v_post.prefix, v_pre.num:v_post.num, v_post.suffix, sep = "", collapse = " ")
      
      # for first hyphen, there may be non-hyphenated syntax preceding the initial match
      cmd.expand[ep] <- joinRegexExpand(cmd, v_expand, hyphens, v)
      
      # This won't really work because the cmd.expand element may contain other variables
      # that are at the beginning or end, prior to hyphen stuff
      # This is superseded by logic above where @ is included in hyphen match, then trapped as suffix
      # I don't think it will work yet for this Mplus syntax: y1-y10*5 -- the 5 wouldn't propagate
      # handle the case of @ fixed values or * starting values used in a list
      # example: Trait1-Trait3@1
      ## if (grepl("@|\\*", cmd.expand[ep], perl=TRUE)) {
      ##   exp_split <- strsplit(cmd.expand[ep], "\\s+", perl=TRUE)[[1]]
      ##   suffixes <- sub("^([^@\\*]+)([@*][\\d\\.-]+)?$", "\\2", exp_split, perl=TRUE)
      ##   variables <- sub("^([^@\\*]+)([@*][\\d\\.-]+)?$", "\\1", exp_split, perl=TRUE)
      ##   suffixes <- suffixes[suffixes != ""]
      ##   if (length(unique(suffixes)) > 1L) {
      ##     browser()
      
      ##     #stop("Don't know how to interpret syntax: ", cmd)
      ##   } else {
      ##     variables <- paste0(variables, suffixes[1])
      ##     cmd.expand[ep] <- paste(variables, collapse=" ")
      ##   }
      ## }
      
      ep <- ep + 1
    }
    return(paste(cmd.expand, collapse = ""))
  } else {
    return(cmd) # no hyphens to expand
  }
}
