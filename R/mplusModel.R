# sections returned by readModels (used for dynamic active bindings)
.mplus_sections <- c(
  "input", "warnings", "errors", "data_summary", "sampstat",
  "covariance_coverage", "summaries", "invariance_testing",
  "parameters", "class_counts", "indirect", "mod_indices",
  "residuals", "savedata", "savedata_info", "bparameters",
  "tech1", "tech3", "tech4", "tech7", "tech8", "tech9",
  "tech10", "tech12", "tech15", "fac_score_stats",
  "lcCondMeans", "r3step", "gh5", "h5results", "output"
)

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

#' @title mplusModel R6 class
#' @description An R6 class for a single Mplus model
#' @details
#'   Wrapped by `mplusModel`
#' @name mplusModel_r6
#' @keywords internal
mplusModel_r6 <- R6::R6Class(
  classname = "mplusModel_r6",
  lock_objects=FALSE,
  private = c(
    list(
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
      
      # private method to populate mplus output fields from readModels result
      populate_output = function(o) {
        private$pvt_output_loaded <- TRUE
        for (sec in .mplus_sections) private[[paste0("pvt_", sec)]] <- o[[sec]]
      },
      
      # private method to clear mplus output fields (if output is invalidated)
      clear_output = function() {
        private$pvt_output_loaded <- FALSE
        for (sec in .mplus_sections) private[[paste0("pvt_", sec)]] <- NULL
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
    
    # all output sections from readModels
    setNames(vector("list", length(.mplus_sections)), paste0("pvt_", .mplus_sections))
  ),
  active = c(
    list(
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
        else stop("Cannot set read-only field", call. = FALSE)
      },
      
      #' @field out_file the location of the Mplus .out file for this model
      out_file = function(value) {
        if (missing(value)) file.path(private$pvt_model_dir, private$pvt_out_file)
        else stop("Cannot set read-only field", call. = FALSE)
      },
      
      #' @field dat_file the location of the Mplus .dat (data) file for this model
      dat_file = function(value)  {
        if (missing(value)) file.path(private$pvt_model_dir, private$pvt_dat_file)
        else stop("Cannot set read-only field", call. = FALSE)
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
    
    # Dynamic read-only active bindings for all .mplus_sections
    # Pass through the argument sec as a function attribute because R6 resets the function environment
    # to point to the R6 object itself (so self and private are visible)
    
    #' @section Active bindings (from `readModels()`):
    #' @field input Read-only accessor for the `input` section returned by `readModels()`.
    #' @field warnings Read-only accessor for the `warnings` section returned by `readModels()`.
    #' @field errors Read-only accessor for the `errors` section returned by `readModels()`.
    #' @field data_summary Read-only accessor for the `data_summary` section returned by `readModels()`.
    #' @field sampstat Read-only accessor for the `sampstat` section returned by `readModels()`.
    #' @field covariance_coverage Read-only accessor for the `covariance_coverage` section returned by `readModels()`.
    #' @field summaries Read-only accessor for the `summaries` section returned by `readModels()`.
    #' @field invariance_testing Read-only accessor for the `invariance_testing` section returned by `readModels()`.
    #' @field parameters Read-only accessor for the `parameters` section returned by `readModels()`.
    #' @field class_counts Read-only accessor for the `class_counts` section returned by `readModels()`.
    #' @field indirect Read-only accessor for the `indirect` section returned by `readModels()`.
    #' @field mod_indices Read-only accessor for the `mod_indices` section returned by `readModels()`.
    #' @field residuals Read-only accessor for the `residuals` section returned by `readModels()`.
    #' @field savedata Read-only accessor for the `savedata` section returned by `readModels()`.
    #' @field savedata_info Read-only accessor for the `savedata_info` section returned by `readModels()`.
    #' @field bparameters Read-only accessor for the `bparameters` section returned by `readModels()`.
    #' @field tech1 Read-only accessor for the `tech1` section returned by `readModels()`.
    #' @field tech3 Read-only accessor for the `tech3` section returned by `readModels()`.
    #' @field tech4 Read-only accessor for the `tech4` section returned by `readModels()`.
    #' @field tech7 Read-only accessor for the `tech7` section returned by `readModels()`.
    #' @field tech8 Read-only accessor for the `tech8` section returned by `readModels()`.
    #' @field tech9 Read-only accessor for the `tech9` section returned by `readModels()`.
    #' @field tech10 Read-only accessor for the `tech10` section returned by `readModels()`.
    #' @field tech12 Read-only accessor for the `tech12` section returned by `readModels()`.
    #' @field tech15 Read-only accessor for the `tech15` section returned by `readModels()`.
    #' @field fac_score_stats Read-only accessor for the `fac_score_stats` section returned by `readModels()`.
    #' @field lcCondMeans Read-only accessor for the `lcCondMeans` section returned by `readModels()`.
    #' @field r3step Read-only accessor for the `r3step` section returned by `readModels()`.
    #' @field gh5 Read-only accessor for the `gh5` section returned by `readModels()`.
    #' @field h5results Read-only accessor for the `h5results` section returned by `readModels()`.
    #' @field output Read-only accessor for the `output` section returned by `readModels()`.
    {
      .make_ro_binding <- function(section) {
        f <- function(value) {
          s <- attr(sys.function(), "section")
          if (missing(value)) {
            private[[paste0("pvt_", s)]]
          } else {
            stop(sprintf("`%s` is read-only.", s), call. = FALSE)
          }
        }
        attr(f, "section") <- section
        f
      }
      setNames(lapply(.mplus_sections, .make_ro_binding), .mplus_sections)
    }
  ),
  public=list(
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
          if (!file.exists(dfile)) dfile <- file.path(private$pvt_model_dir, dfile)
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
    
    #' @description run this model locally using `runModels`. The method detects
    #'   changes in the data and input syntax and only rewrites the corresponding
    #'   files when updates are detected.
    #' @param replaceOutfile Currently supports three settings: “always”, which runs all models, regardless of whether an output file for the model exists; 
    #'   “never”, which does not run any model that has an existing output file; and “modifiedDate”, which only runs a model if the modified date for the input
    #'   file is more recent than the output file modified date (implying there have been updates to the model).
    #' @param ... additional arguments passed to `runModels`
    run = function(replaceOutfile = "modifiedDate", ...) {
      # Only write input and data files when the current contents differ from those on disk

      # check whether data have changed compared to existing .dat file
      if (!private$pvt_is_montecarlo && !is.null(self$data)) {
        write_dat <- TRUE
        cols <- private$pvt_variables
        if (is.null(cols)) cols <- names(self$data)
        if (file.exists(self$dat_file)) {
          new_hash <- digest::digest(self$data[, cols, drop = FALSE], algo = "md5")
          existing <- tryCatch(
            data.table::fread(self$dat_file, col.names = cols, data.table = FALSE),
            error = function(e) NULL
          )
          if (!is.null(existing)) {
            ext_hash <- digest::digest(existing, algo = "md5")
            if (isTRUE(new_hash == ext_hash)) write_dat <- FALSE
          }
        }
        if (write_dat) self$write_dat(quiet = TRUE)
      }

      # check whether the input file has changed
      write_inp <- TRUE
      if (file.exists(self$inp_file)) {
        new_md5 <- digest::digest(self$syntax, algo = "md5", serialize = FALSE)
        ext_md5 <- digest::digest(readLines(self$inp_file), algo = "md5", serialize = FALSE)
        if (isTRUE(new_md5 == ext_md5)) write_inp <- FALSE
      }
      if (write_inp) self$write_inp(quiet = TRUE)

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

#' Expand Mplus-style hyphenated variable ranges
#'
#' Expands Mplus shorthand expressions that specify sequences of variables
#' using hyphenated ranges (e.g., \code{y1-y3}) into the full list of
#' variables (e.g., \code{y1 y2 y3}). This function also propagates suffixes
#' from the right-hand token (e.g., \code{@c}, \code{*c}, or bare \code{*})
#' to every expanded variable (e.g., \code{y1-y3@1} → \code{y1@1 y2@1 y3@1}).
#'
#' By default, the function does **not** expand pure numeric ranges (e.g.,
#' \code{1-3}) to avoid confusion with arithmetic subtraction. If
#' \code{expand_numeric = TRUE}, such ranges will be expanded when they
#' appear in list-like contexts (whitespace/comma/semicolon/parentheses
#' boundaries) and the line does not contain an equals sign (to avoid
#' accidental expansion of arithmetic like \code{d = 1 - 3}).
#'
#' Hyphens in \code{MODEL CONSTRAINT} expressions such as
#' \code{a = b1-b2} are explicitly protected and left untouched.
#'
#' @param cmd A single character string containing Mplus syntax to expand.
#' @param expand_numeric Logical. If \code{TRUE}, expand pure numeric ranges
#'   (e.g., \code{1-3} → \code{1 2 3}) in list-like contexts. Default: \code{FALSE}.
#'
#' @return A character string with hyphenated ranges expanded to explicit
#'   variable lists.
#'
#' @examples
#' expandCmd("y1-y3 y5-y6")
#' # "y1 y2 y3 y5 y6"
#'
#' expandCmd("BY y1-y3@0.5;")
#' # "BY y1@0.5 y2@0.5 y3@0.5;"
#'
#' expandCmd("z10-z12*2")
#' # "z10*2 z11*2 z12*2"
#'
#' expandCmd("MODEL CONSTRAINT: a = b1-b2;")
#' # "MODEL CONSTRAINT: a = b1-b2;" (unchanged)
#'
#' expandCmd("1 - 3", expand_numeric = TRUE)
#' # "1 2 3"
#'
#' expandCmd("d = 1 - 3;", expand_numeric = TRUE)
#' # "d = 1 - 3;" (unchanged because of '=')
#'
#' @keywords internal
expandCmd <- function(cmd, expand_numeric = FALSE) {
  x <- cmd
  
  # --- Protect constraint-style identifier subtraction (preserve spacing) ---
  # Example: "a = b1-b2" becomes "a = b1‹MINUS›b2"
  # The space after '=' is captured and restored unchanged.
  x <- gsub(
    "(=\\s*)([A-Za-z_][A-Za-z_0-9]*\\d+)\\s*-\\s*([A-Za-z_][A-Za-z_0-9]*\\d+)",
    "\\1\\2‹MINUS›\\3",
    x, perl = TRUE
  )
  
  # --- Expand identifier ranges (prefixN - prefixM) ---
  # Boundaries: whitespace, comma, semicolon, parentheses, or string edges.
  bL <- "(?:(?<=^)|(?<=[\\s,;\\(]))"
  bR <- "(?:(?=$)|(?=[\\s,;\\)]))"
  
  id_pattern <- paste0(
    bL,
    "(?<full>",
    "(?<alpha1>[A-Za-z_][A-Za-z_0-9]*?)(?<i1>\\d+)",
    "\\s*-\\s*",
    "(?<alpha2>[A-Za-z_][A-Za-z_0-9]*?)(?<i2>\\d+)",
    "(?<suf>(?:@[-+]?\\d+(?:\\.\\d+)?|\\*[-+]?\\d*(?:\\.\\d+)?)?)",
    ")",
    bR
  )
  
  m <- gregexpr(id_pattern, x, perl = TRUE)
  if (m[[1]][1L] != -1L) {
    starts <- as.integer(m[[1]])
    caps   <- attr(m[[1]], "capture.start")
    clen   <- attr(m[[1]], "capture.length")
    nm     <- attr(m[[1]], "capture.names")
    
    cap <- function(k, name) {
      i <- match(name, nm)
      if (is.na(i) || caps[k, i] == -1L) "" else
        substr(x, caps[k, i], caps[k, i] + clen[k, i] - 1L)
    }
    
    # Replace matches from right to left so string indices stay valid.
    for (k in rev(seq_along(starts))) {
      a1  <- cap(k, "alpha1"); a2 <- cap(k, "alpha2")
      i1  <- as.integer(cap(k, "i1")); i2 <- as.integer(cap(k, "i2"))
      suf <- cap(k, "suf")
      
      # Skip if prefixes differ or indices invalid.
      if (!nzchar(a1) || a1 != a2 || is.na(i1) || is.na(i2) || i2 < i1) next
      
      # Build expansion sequence.
      seq_ids <- paste0(a1, i1:i2)
      if (nzchar(suf)) seq_ids <- paste0(seq_ids, suf)
      repl <- paste(seq_ids, collapse = " ")
      
      # Replace full match with expanded list.
      full_start <- caps[k, match("full", nm)]
      full_len   <- clen[k, match("full", nm)]
      x <- paste0(
        substr(x, 1L, full_start - 1L),
        repl,
        substr(x, full_start + full_len, nchar(x))
      )
    }
  }
  
  # --- Optional: expand pure numeric ranges (1 - 3 → 1 2 3) ---
  # Only applied if expand_numeric = TRUE and no '=' is present in the line
  # (to avoid arithmetic like "d = 1 - 3;").
  if (isTRUE(expand_numeric) && !grepl("=", x, fixed = TRUE)) {
    num_pattern <- paste0(bL, "(?<full>(?<a>\\d+)\\s*-\\s*(?<b>\\d+))", bR)
    m2 <- gregexpr(num_pattern, x, perl = TRUE)
    if (m2[[1]][1L] != -1L) {
      starts <- as.integer(m2[[1]])
      caps   <- attr(m2[[1]], "capture.start")
      clen   <- attr(m2[[1]], "capture.length")
      nm     <- attr(m2[[1]], "capture.names")
      
      cap2 <- function(k, name) {
        i <- match(name, nm)
        if (is.na(i) || caps[k, i] == -1L) "" else
          substr(x, caps[k, i], caps[k, i] + clen[k, i] - 1L)
      }
      
      for (k in rev(seq_along(starts))) {
        a <- as.integer(cap2(k, "a")); b <- as.integer(cap2(k, "b"))
        if (is.na(a) || is.na(b) || b < a) next
        repl <- paste(a:b, collapse = " ")
        full_start <- caps[k, match("full", nm)]
        full_len   <- clen[k, match("full", nm)]
        x <- paste0(
          substr(x, 1L, full_start - 1L),
          repl,
          substr(x, full_start + full_len, nchar(x))
        )
      }
    }
  }
  
  # --- Unprotect sentinels ---
  x <- gsub("‹MINUS›", "-", x, fixed = TRUE)
  
  x
}