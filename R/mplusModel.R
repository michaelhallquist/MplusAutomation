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

.mplus_input_sections <- c(
  "title", "data", "variable", "define", "montecarlo",
  "model.population", "model.missing", "analysis", "model",
  "model.indirect", "model.constraint", "model.test", "model.priors",
  "output", "savedata", "plot"
)

.normalize_mplus_update_name <- function(name) {
  key <- gsub("[[:space:]_.]+", "", tolower(name), perl = TRUE)
  switch(
    key,
    title = "title",
    data = "data",
    variable = "variable",
    define = "define",
    montecarlo = "montecarlo",
    modelpopulation = "model.population",
    modelmissing = "model.missing",
    analysis = "analysis",
    model = "model",
    modelindirect = "model.indirect",
    modelconstraint = "model.constraint",
    modeltest = "model.test",
    modelpriors = "model.priors",
    output = "output",
    savedata = "savedata",
    plot = "plot",
    NA_character_
  )
}

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
      pvt_variables_manual = FALSE,# whether variables are manually set (not auto-detected)
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
        if (isTRUE(private$pvt_variables_manual)) return(invisible(NULL))
        if (is.null(private$pvt_syntax)) return(invisible(NULL))

        data_names <- if (!is.null(private$pvt_data)) names(private$pvt_data) else NULL
        vars <- extractMplusVariables(
          parsed_syntax = private$pvt_syntax,
          data_names = data_names
        )

        if (length(vars) == 0L && !is.null(data_names)) vars <- data_names
        private$pvt_variables <- if (length(vars) > 0L) vars else NULL
      },
      
      sanitize_dat_file = function(path) {
        if (is.null(path)) return(NULL)
        path <- trimws(path) # remove leading and trailing white space
        if (!nzchar(path)) return(NULL)
        path <- gsub('^"|"$', "", path) # strip leading and trailing double quotes
        # unwrap line-wrapped path continuations from DATA: FILE syntax
        gsub("([/\\\\])\\s+", "\\1", path, perl = TRUE)
      },
      
      is_absolute_path = function(path) {
        if (is.null(path)) return(FALSE)
        path <- trimws(path)
        if (!nzchar(path)) return(FALSE)
        startsWith(path, "/") || startsWith(path, "\\") || grepl("^[A-Za-z]:", path)
      },
      
      set_dat_file = function(path) {
        private$pvt_dat_file <- private$sanitize_dat_file(path)
      },
      
      resolve_dat_file = function() {
        spec <- private$sanitize_dat_file(private$pvt_dat_file)
        if (is.null(spec)) return(NULL)
        # return absolute path if dat file is absolute
        if (private$is_absolute_path(spec)) return(private$normalize_path_safe(spec))
        expanded <- path.expand(spec)
        if (private$is_absolute_path(expanded)) return(private$normalize_path_safe(expanded))
        if (is.null(private$pvt_model_dir)) return(spec)
        private$normalize_path_safe(file.path(private$pvt_model_dir, spec))
      },
      
      normalize_path_safe = function(path) {
        if (is.null(path)) return(NULL)
        out <- tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE),
                       error = function(e) path)
        if (!nzchar(out)) return(path)
        out <- gsub("(?<!^)//+", "/", out, perl = TRUE)
        out
      },
      
      dat_path_is_in_model_dir = function() {
        if (is.null(private$pvt_model_dir)) return(FALSE)
        resolved <- private$resolve_dat_file()
        if (is.null(resolved)) return(FALSE)
        model_norm <- private$normalize_path_safe(private$pvt_model_dir)
        resolved_norm <- private$normalize_path_safe(resolved)
        if (is.null(model_norm) || is.null(resolved_norm)) return(FALSE)
        prefix <- paste0(model_norm, "/")
        identical(resolved_norm, model_norm) || startsWith(resolved_norm, prefix)
      },
      
      dat_path_in_model_dir_root = function() {
        if (is.null(private$pvt_model_dir)) return(FALSE)
        resolved <- private$resolve_dat_file()
        if (is.null(resolved)) return(FALSE)
        model_norm <- private$normalize_path_safe(private$pvt_model_dir)
        resolved_norm <- private$normalize_path_safe(resolved)
        if (is.null(model_norm) || is.null(resolved_norm)) return(FALSE)
        dirname(resolved_norm) == model_norm
      },
      
      dat_syntax_path = function() {
        spec <- private$sanitize_dat_file(private$pvt_dat_file)
        if (is.null(spec)) return(NULL)
        if (private$dat_path_in_model_dir_root()) {
          resolved <- private$resolve_dat_file()
          return(basename(resolved))
        }
        spec
      },
      
      format_data_file_for_syntax = function(path) {
        if (is.null(path)) return(NULL)
        wrapped <- trimws(gsub("(.{1,75})(\\s|$|/|\\\\)", "\\1\\2\n", path))
        paste0("\"", wrapped, "\"")
      },
      
      section_to_text = function(section_name) {
        sec <- private$pvt_syntax[[section_name]]
        if (is.null(sec)) return("")
        
        if (is.list(sec)) {
          out <- character(0)
          for (nm in names(sec)) {
            val <- sec[[nm]]
            if (length(val) == 0L) next
            out <- c(out, paste0(toupper(sub("\\.", " ", nm, fixed = TRUE)), " = ", paste(val, collapse = " "), ";"))
          }
          return(paste(out, collapse = "\n"))
        }
        
        paste(as.character(sec), collapse = "\n")
      },
      
      resolve_update_text = function(spec, old_text) {
        if (inherits(spec, "formula")) {
          if (length(spec) < 2L) stop("Invalid formula specification in update().")
          rhs <- spec[[2L]]
          rhs_parts <- as.character(rhs)
          append_mode <- any(grepl("^\\.$", rhs_parts))
          new_text <- rhs_parts[length(rhs_parts)]
          if (append_mode && nzchar(trimws(old_text))) {
            return(paste(old_text, new_text, sep = "\n"))
          }
          return(new_text)
        }
        
        if (is.character(spec)) return(paste(spec, collapse = "\n"))
        stop("Update values must be either formulas or character vectors.")
      },
      
      set_section_text = function(section_name, text) {
        txt <- if (is.null(text)) "" else paste(text, collapse = "\n")
        txt <- gsub("\r", "", txt, fixed = TRUE)
        lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
        lines <- lines[nzchar(trimws(lines))]
        
        if (length(lines) == 0L) {
          private$pvt_syntax[[section_name]] <- NULL
          if (identical(section_name, "data")) private$set_dat_file(NULL)
          if (identical(section_name, "montecarlo")) private$pvt_is_montecarlo <- FALSE
          return(invisible(NULL))
        }
        
        if (section_name %in% c("data", "variable", "analysis", "montecarlo", "savedata")) {
          parsed <- divideIntoFields(lines)
          private$pvt_syntax[[section_name]] <- parsed
          if (identical(section_name, "data")) {
            if (!is.null(parsed$file)) private$set_dat_file(parsed$file)
            else private$set_dat_file(NULL)
          }
        } else if (identical(section_name, "title")) {
          private$pvt_syntax[[section_name]] <- paste(trimws(lines), collapse = " ")
        } else {
          private$pvt_syntax[[section_name]] <- lines
        }
        
        if (identical(section_name, "montecarlo")) {
          private$pvt_is_montecarlo <- TRUE
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
        if (missing(value)) private$resolve_dat_file()
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

          # data updates can change case-insensitive matching of detected syntax variables
          private$detect_variables()
          
          # if we are updating the dataset, make sure that all expected variables are present
          if (!is.null(value) && !is.null(private$pvt_variables)) {
            missing_vars <- setdiff(private$pvt_variables, names(value))
            if (length(missing_vars) > 0L)
              warning("The following variables are mentioned in the model syntax, but missing in the data: ",
                      paste(missing_vars, collapse=", "))
          }
        }
      },
      
      #' @field variables variables to write to the .dat file. Set NULL to restore automatic detection.
      variables = function(value) {
        if (missing(value)) {
          private$pvt_variables
        } else {
          if (is.null(value)) {
            private$pvt_variables_manual <- FALSE
            private$detect_variables()
          } else if (!checkmate::test_character(value, min.len = 1L)) {
            stop("variables must be a non-empty character vector or NULL.")
          } else {
            private$pvt_variables <- unique(as.character(value))
            private$pvt_variables_manual <- TRUE
          }
          
          if (!is.null(private$pvt_data) && !is.null(private$pvt_variables)) {
            missing_vars <- setdiff(private$pvt_variables, names(private$pvt_data))
            if (length(missing_vars) > 0L) {
              warning(
                "The following variables are in `variables`, but missing in the data: ",
                paste(missing_vars, collapse = ", ")
              )
            }
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
            if (!is.null(private$pvt_syntax$data$file)) {
              private$set_dat_file(private$pvt_syntax$data$file)
            }
            
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
          private$set_dat_file(self$input$data$file)

          # If the data file cannot be loaded as-is, attempt to locate it in the directory of the input/output files.
          # This handles cases where the data file was specified with an absolute path on another machine but
          # the .dat, .inp and .out files all reside in the same folder.
          if (!file.exists(dfile)) {
            # first, try using the provided path relative to the model directory
            rel_dfile <- file.path(private$pvt_model_dir, dfile)
            if (file.exists(rel_dfile)) {
              dfile <- rel_dfile
            } else {
              # next, try just the basename of the data file in the model directory
              base_dfile <- file.path(private$pvt_model_dir, basename(dfile))
              if (file.exists(base_dfile)) {
                dfile <- base_dfile
                private$set_dat_file(basename(dfile))
              }
            }
          }

          data <- tryCatch(
            data.table::fread(dfile, header = FALSE, na.strings = c("*", "."), strip.white = TRUE, data.table = FALSE),
            error = function(e) {
              warning("Could not load data file: ", dfile)
              return(NULL)
            }
          )

          # set the names of the data if read succeeds
          if (!is.null(data)) names(data) <- strsplit(expandCmd(self$input$variable$names), "\\s+")[[1]]
        }
      }
      
      # populate model syntax
      if (!checkmate::test_character(syntax)) stop("syntax argument must not be NULL")
      
      # populate model data
      self$data <- data
      
      # force syntax to be a character vector (convert \n to elements)
      dat_spec_override <- private$pvt_dat_file
      self$syntax <- unlist(strsplit(syntax, "\\n"))
      if (!is.null(dat_spec_override)) {
        parsed_spec <- private$sanitize_dat_file(private$pvt_syntax$data$file)
        override_spec <- private$sanitize_dat_file(dat_spec_override)
        if (!is.null(override_spec) && !identical(parsed_spec, override_spec)) {
          private$set_dat_file(override_spec)
        }
      }
      
      # set default data file name
      if (isFALSE(private$pvt_is_montecarlo) && is.null(private$pvt_dat_file) && !is.null(private$pvt_inp_file)) {
        private$set_dat_file(sub("\\.inp?$", ".dat", private$pvt_inp_file))
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
      
      dat_path <- self$dat_file
      if (is.null(dat_path)) {
        stop("Cannot determine data file path for this model.")
      }
      if (file.exists(dat_path) && isFALSE(overwrite)) {
        if (!quiet) message("Not overwriting existing data file: ", self$dat_file)
        return(invisible(self))
      }
      
      if (!dir.exists(dirname(dat_path))) dir.create(dirname(dat_path), recursive = TRUE, showWarnings = FALSE)

      # determine if data file is in the same directory of a subdirectory of the input file
      use_relative <- private$dat_path_in_model_dir_root()
      inp_syntax <- prepareMplusData(
        df = self$data, filename = dat_path,
        keepCols = private$pvt_variables,
        quiet = TRUE, use_relative_path = use_relative, ...
      )
      
      # ensure that the variable names in the syntax match what we detected
      private$pvt_syntax$variable$names <- attr(inp_syntax, "variable_names")
      private$pvt_syntax$variable$missing <- attr(inp_syntax, "missing")
      attr_data_file <- private$sanitize_dat_file(attr(inp_syntax, "data_file"))
      if (!is.null(attr_data_file)) {
        current_spec <- private$sanitize_dat_file(private$pvt_dat_file)
        if (!identical(attr_data_file, current_spec)) {
          if (private$is_absolute_path(attr_data_file) && private$is_absolute_path(current_spec)) {
            private$set_dat_file(attr_data_file)
          } else if (!grepl("[/\\\\]", attr_data_file) && private$dat_path_is_in_model_dir()) {
            private$set_dat_file(attr_data_file)
          }
        }
      }
      
      data_spec <- private$dat_syntax_path()
      if (!is.null(data_spec)) {
        private$pvt_syntax$data$file <- private$format_data_file_for_syntax(data_spec)
      }
      
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
      data_spec <- private$dat_syntax_path()
      if (!is.null(data_spec)) {
        private$pvt_syntax$data$file <- private$format_data_file_for_syntax(data_spec)
      }
      
      # if the inp file exists, compare its contents against the user's syntax
      if (file.exists(inp_file)) {
        new_md5 <- digest::digest(self$syntax, algo="md5", serialize=FALSE)
        
        # for some reason, the file = TRUE approach fails under different variants
        # ext_md5 <- digest::digest(self$inp_file, algo="md5", file=TRUE, serialize=FALSE, ascii=TRUE)
        ext_md5 <- digest::digest(readLines(inp_file), algo="md5", file=FALSE, serialize=FALSE)
        
        if (new_md5 != ext_md5 && isFALSE(overwrite)) {
          write <- FALSE
          if (!quiet) message("Not overwriting existing .inp file: ", inp_file)
        }
      }
      
      if (write) {
        if (!quiet) message("Writing Mplus syntax to file: ", inp_file)
        writeLines(self$syntax, con = inp_file)
      }
      return(invisible(self))
    },
    
    #' @description submit this model for estimation on an HPC using `submitModels`
    #' @param replaceOutfile Currently supports three settings: "always", which runs all models, regardless of whether an output file for the model exists; 
    #'   "never", which does not run any model that has an existing output file; and "modifiedDate", which only runs a model if the modified date for the input
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
    #' @param replaceOutfile Currently supports three settings: "always", which runs all models, regardless of whether an output file for the model exists; 
    #'   "never", which does not run any model that has an existing output file; and "modifiedDate", which only runs a model if the modified date for the input
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
        new_md5 <- digest::digest(self$syntax, algo = "md5", serialize = TRUE)
        ext_md5 <- digest::digest(readLines(self$inp_file), algo = "md5", serialize = TRUE)
        if (isTRUE(new_md5 == ext_md5)) write_inp <- FALSE
      }
      if (write_inp) self$write_inp(quiet = TRUE)

      runModels(target = self$inp_file, replaceOutfile = replaceOutfile, Mplus_command = self$Mplus_command, ...)
      self$read(force = TRUE) # read/re-read after estimation

    },
    
    #' @description Update model sections using `update()`-style formula semantics.
    #' @param ... Named updates. For Mplus input sections, use formulas:
    #'   `~ "new text"` replaces, `~ . + "additional text"` appends.
    #' @param in_place If `TRUE` (default), mutate this object. If `FALSE`, return an updated clone.
    #' @param quiet If `TRUE`, suppress status messages.
    update = function(..., in_place = TRUE, quiet = TRUE) {
      checkmate::assert_flag(in_place)
      checkmate::assert_flag(quiet)
      
      if (isFALSE(in_place)) {
        obj <- self$clone(deep = TRUE)
        obj$update(..., in_place = TRUE, quiet = quiet)
        return(obj)
      }
      
      dots <- list(...)
      if (length(dots) == 0L) return(invisible(self))
      if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
        stop("All update arguments must be named.")
      }
      
      syntax_changed <- FALSE
      for (nm in names(dots)) {
        key_lc <- tolower(nm)
        val <- dots[[nm]]
        
        if (identical(key_lc, "rdata") ||
            (identical(key_lc, "data") &&
             (is.null(val) || checkmate::test_data_frame(val) || checkmate::test_data_table(val)))) {
          self$data <- val
          next
        }
        
        if (key_lc %in% c("usevariables", "variables")) {
          self$variables <- val
          next
        }
        
        section_name <- .normalize_mplus_update_name(nm)
        if (is.na(section_name) || !section_name %in% .mplus_input_sections) {
          stop("Unknown update field: ", nm)
        }
        
        old_text <- private$section_to_text(section_name)
        new_text <- private$resolve_update_text(val, old_text)
        if (!identical(new_text, old_text)) {
          private$set_section_text(section_name, new_text)
          syntax_changed <- TRUE
        }
      }
      
      if (isTRUE(syntax_changed)) {
        # normalize sections through parser/stringifier path
        self$syntax <- mplusInpToString(private$pvt_syntax)
        if (isTRUE(private$pvt_output_loaded)) {
          private$clear_output()
          if (!quiet) message("Syntax has changed. Unloading model results from object.")
        }
      }
      
      invisible(self)
    }
  )
)

#' @method update mplusModel_r6
#' @export
update.mplusModel_r6 <- function(object, ..., .in_place = FALSE, quiet = TRUE) {
  checkmate::assert_flag(.in_place)
  checkmate::assert_flag(quiet)
  out <- if (isTRUE(.in_place)) object else object$clone(deep = TRUE)
  out$update(..., in_place = TRUE, quiet = quiet)
  out
}

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
#' to every expanded variable (e.g., `y1-y3@1` -> `y1@1 y2@1 y3@1`).
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
#'   (e.g., `1-3` -> `1 2 3`) in list-like contexts. Default: \code{FALSE}.
#'
#' @return A character string with hyphenated ranges expanded to explicit
#'   variable lists.
#'
#' @examples
#' \dontrun{
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
#' }
#' @keywords internal
expandCmd <- function(cmd, expand_numeric = FALSE) {
  x <- cmd
  
  # --- Protect constraint-style identifier subtraction (preserve spacing) ---
  # Example: "a = b1-b2" becomes "a = b1<MINUS>b2"
  # The space after '=' is captured and restored unchanged.
  x <- gsub(
    "(=\\s*)([A-Za-z_][A-Za-z_0-9]*\\d+)\\s*-\\s*([A-Za-z_][A-Za-z_0-9]*\\d+)",
    "\\1\\2<MINUS>\\3",
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
  
  # --- Optional: expand pure numeric ranges (1 - 3 -> 1 2 3) ---
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
  x <- gsub("<MINUS>", "-", x, fixed = TRUE)
  
  x
}
