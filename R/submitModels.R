#' helper function to validate format of walltime inputs for HPC submission
#'
#' @param str string containing a duration that may include a days specification
#' @importFrom checkmate assert_string
#' @details this always converts to an hms format, and if days are present, it
#'   converts to dhms. Supported date formats match slurm sbatch:
#'   https://slurm.schedmd.com/sbatch.html
#' @keywords internal
validate_dhms <- function(str) {
  checkmate::assert_string(str)
  if (grepl("^\\d+:\\d+?$", str, perl = T)) { # m:s input
    return(paste0("00:", str)) # add 0 hours prefix
  } else if (grepl("^(\\d+-)?\\d+:\\d+:\\d+?$", str, perl = T)) { # h:m:s or d-h:m:s input
    return(str)
  } else if (grepl("^(\\d+-)?\\d+:\\d+?$", str, perl = T)) { # d-h:m input
    return(paste0(str, ":00")) # add 0 seconds
  } else if (grepl("^\\d+-\\d+$", str, perl = T)) { # days-hours input
    return(paste0(str, ":00:00")) # add 0 minutes, 0 seconds
  } else if (grepl("^\\d+$", str, perl = T)) {
    return(paste0("00:", str, ":00")) # minutes only -> hours:minutes:seconds
  } else {
    stop("Invalid duration string: ", str)
  }
}

#' helper function to convert a dhms string to the number of hours for combining similar job times
#' @param str string in dhms format
#' @return the number of hours represented by the dhms string
#' @keywords internal
dhms_to_hours <- function(str) {
  checkmate::assert_string(str)
  str <- validate_dhms(str) # force checks
  if (grepl("^\\d+-", str, perl = TRUE)) {
    split_hyphen <- strsplit(str, "-", fixed = TRUE)[[1]]
    stopifnot(length(split_hyphen) == 2L)
    days <- as.numeric(split_hyphen[1])
    hms <- split_hyphen[2L]
  } else {
    days <- 0
    hms <- str
  }

  hms_split <- strsplit(hms, ":", fixed = TRUE)[[1L]]
  hours <- as.numeric(hms_split[1])
  minutes <- as.numeric(hms_split[2])
  seconds <- as.numeric(hms_split[3])

  hours <- days*24 + hours + minutes/60 + seconds/3600

  return(hours)
}

#' helper function to convert a number of minutes into a dhms-formatted string for submission
#' @param x string or charcater number of minutes
#' @return the dhms string representing this number of minutes in days, hours, minutes, and seconds
#' @keywords internal
minutes_to_dhms <- function(x) {
  # for now, don't even support seconds
  if (is.character(x)) x <- as.numeric(x)
  d <- 0
  h <- 0
  if (x > 1440) {
    d <- floor(x / (24 * 60))
    r <- x %% (24 * 60)
    h <- floor(r / 60)
    m <- r %% 60
  } else if (x > 60) {
    h <- floor(x / 60)
    m <- x %% 60
  }

  validate_dhms(paste0(d, "-", h, ":", m))
}

#' helper function to crawl over the target location, determine if it is a file or folder,
#' then locate all .inp files, and convert them to absolute paths
#' @param target a character vector where each element is a directory containing Mplus input files 
#'   (.inp) to run OR a single .inp file to be run. Elements may be a full path, relative path,
#'   or a filename within the working directory.
#' @param filefilter An optional PCRE expression for filtering input files of interest
#' @param recursive if TRUE, search for .inp files in subfolders of all elements of \code{target}
#' @return A vector of .inp file locaitons
#' @keywords internal
convert_to_filelist <- function(target, filefilter=NULL, recursive=FALSE) {
  filelist <- c()
  for (tt in target) {
    fi <- file.info(tt)
    if (isTRUE(is.na(fi$size))) { stop("Cannot find target: ", tt) } #do not tolerate missing files or folders
    
    if (isTRUE(fi$isdir)) {
      # remove trailing slash, which generates file.exists error on windows: https://bugs.r-project.org/bugzilla/show_bug.cgi?id=14721
      directory <- sub("(\\\\|/)?$", "", tt, perl=TRUE)
      # however, trailing slash is needed if at root of a drive on Windows
      if (is.windows() && isTRUE(grepl("^[a-zA-Z]:$", directory))) {
        directory <- paste0(directory, "/")
      }
      
      if (isFALSE(file.exists(directory))) stop("Cannot find directory: ", directory)
      
      # list files in the current directory
      this_set <- list.files(path=directory, recursive=recursive, pattern=".*\\.inp?$", full.names = TRUE)
      filelist <- c(filelist, this_set)
  } else {
      #element is a file
      #check file extension
      if (!grepl(".*\\.inp?$", tt, perl=TRUE)) {
        warning("Target: ", tt, "does not appear to be an .inp file. Ignoring it.")
        next
      } else {
        if (isFALSE(file.exists(tt))) { stop("Cannot find input file: ", tt) }
        
        filelist <- c(filelist, tt)
      }
    }
  }
  
  #apply user filter, if requested
  if (!is.null(filefilter)) filelist <- grep(filefilter, filelist, perl=TRUE, value=TRUE)
  
  #normalize paths to convert everything to absolute
  filelist <- normalizePath(filelist)
  return(filelist)
}

#' helper function to filter a set of Mplus input files based on whether the corresponding .out file already exists
#'
#' @param inp_files a vector of input file locations to check for corresponding .out files
#' @param replaceOutfile optional. Currently supports three settings: \dQuote{always}, which
#'   runs all models, regardless of whether an output file for the model exists; \dQuote{never},
#'   which does not run any model that has an existing output file; and \dQuote{modifiedDate}, which
#'   only runs a model if the modified date for the input file is more recent than
#'   the output file modified date (implying there have been updates to the model).
#' @param quiet whether to print out text indicating what files were skipped
#' @keywords internal
filter_inp_filelist <- function(inp_files, replaceOutfile = "always", quiet=TRUE) {
  checkmate::assert_character(inp_files)
  checkmate::assert_string(replaceOutfile)
  stopifnot(replaceOutfile %in% c("always", "never", "modifiedDate"))
  checkmate::assert_flag(quiet)

  if (replaceOutfile == "always") return(inp_files) # apply no filtering to .inp files

  # find .out files corresponding to each .inp file
  out_files <- sub("\\.inp?$", ".out", inp_files)
  out_exists <- file.exists(out_files)

  # drop file extension
  drop_out_extensions <- sapply(out_files, function(x) {
    if (isTRUE(nchar(x) >= 4)) {
      return(tolower(substr(x, 1, (nchar(x) - 4))))
    }
  })

  drop_pos <- c() # vector of positions to drop from inp_files
  
  for (i in seq_along(inp_files)) {

    # if there is a match in the out_files for this input file, then decide whether to filter it
    if (out_exists[i]) {
      if (isTRUE(replaceOutfile == "modifiedDate")) {
        # if check date is true, then the output file must exist and it must be older than the input file to re-run
        inpmtime <- file.info(inp_files[i])$mtime
        outmtime <- file.info(out_files[i])$mtime

        if (isTRUE(inpmtime <= outmtime)) {
          if (!quiet) cat(paste("Skipping model because output file is newer than input file:", inp_files[i]), "\n")
          drop_pos <- c(drop_pos, i)
        }
      } else if (isTRUE(replaceOutfile == "never")) {
        if (!quiet) cat(paste("Skipping model because output file already exists for:", inp_files[i]), "\n")
        drop_pos <- c(drop_pos, i)
      }
    }
  }

  return(inp_files[setdiff(seq_along(inp_files), drop_pos)])
}

#' Submit Mplus models to a high-performance cluster scheduler
#'
#' This function submits a group of Mplus models (.inp files) located within a
#' single directory or nested within subdirectories.
#'
#' @param target a character vector where each element is a directory containing Mplus input files 
#'   (.inp) to run OR a single .inp file to be run. Elements may be a full path, relative path,
#'   or a filename within the working directory.
#'   Defaults to the current working directory. Example: \dQuote{C:/Users/Michael/Mplus Runs}
#' @param recursive optional. If \code{TRUE}, run all models nested in subdirectories
#'   within \code{directory}. Defaults to \code{FALSE}. Not relevant if \code{target} is a single file.
#' @param filefilter a Perl regular expression (PCRE-compatible) specifying particular input
#'   files to be run among those found in \code{target}. See \code{regex} or \url{http://www.pcre.org/pcre.txt}
#'   for details about regular expression syntax.
#' @param replaceOutfile optional. Currently supports three settings: \dQuote{always}, which
#'   runs all models, regardless of whether an output file for the model exists; \dQuote{never},
#'   which does not run any model that has an existing output file; and \dQuote{modifiedDate}, which
#'   only runs a model if the modified date for the input file is more recent than
#'   the output file modified date (implying there have been updates to the model).
#' @param Mplus_command optional. N.B.: No need to pass this parameter for most users (has intelligent
#'   defaults). Allows the user to specify the name/path of the Mplus executable to be used for
#'   running models. This covers situations where Mplus is not in the system's path,
#'   or where one wants to test different versions of the Mplus program.
#' @param quiet optional. If \code{FALSE}, show status messages in the console.
#' @param scheduler Which scheduler to use for job submission. Options are 'qsub', 'torque', 'sbatch', 'slurm', or 'sh'.
#'      The terms 'qsub' and 'torque' are aliases (where 'torque' submits via the qsub command). Likewise for 'sbatch'
#'      and 'slurm'. The scheduler 'sh' does not submit to any scheduler at all, but instead executes the command
#'      immediately via sh.
#' @param sched_args A character vector of arguments to be included in the scheduling command. On TORQUE, these
#'      will typically begin with '-l' such as '-l wall_time=10:00:00'. These are added inside the submission script
#'      for each model and are shared across all models. To add model-specific arguments, include `! #SBATCH` or
#'      `! #PBS` lines inside the individual .inp files
#' @param env_variables A named character vector containing environment variables and their values to be passed
#'      to the \code{script} at execution time. This is handled by the '-v' directive on TORQUE clusters and
#'      by '--export' on Slurm clusters. The names of this vector are the environment variable names and
#'      the values of the vector are the environment variable values to be passed in.
#'      If you want to propagate the current value of an environment variable to the compute node at runtime,
#'      use NA as the value of the element in \code{env_variables}. See examples.
#' @param export_all Whether to export all environment variables to the compute node at runtime. Default: FALSE
#' @param cores_per_model How many cpus/cores are requested for each model (can be overriden using `! BATCH` directives in .inp files).
#'      Default: 1.
#' @param memgb_per_model amount of memory (RAM) requested for each model (in GB). Default: 8.
#' @param time_per_model amount of time requested for each model. Default: "1:00:00" (1 hour). If
#'   a number is provided, we will treat this as the number of minutes.
#' @param pre user-specified shell commands  to include in the job script prior to running Mplus (e.g., module load commands)
#' @param post user-specified shell commands to include in the job script after Mplus runs (e.g., execute results wrangling script)
#' @param batch_outdir the directory where job scripts should be written
#' @param job_script_prefix the filename prefix for each job script
#' @param combine_jobs if TRUE, \code{submitModels} will seek to combine similar models into batches to reduce the total number of jobs
#' @param max_time_per_job The maximum time (in days-hours:minutes:seconds format) allowed for a combined job
#' @param combine_memgb_tolerance The memory tolerance for combining similar models in GB. Defaults to 1 (i.e., models that differ by <= 1 GB can be combined)
#' @param combine_cores_tolerance The cores tolerance for combining similar models in number of cores. Defaults to 2 (i.e., models that whose core requests differ by <= 2 can be combined)
#' @param debug a logical indicating whether to actually submit the jobs (TRUE) or just create the scripts for inspection (FALSE)
#' @param fail_on_error Whether to stop execution of the script (TRUE), or issue a warning (FALSE) if the job
#'      submission fails. Defaults to TRUE.
#' @return None. Function is used for its side effects (submitting models).
#' @author Michael Hallquist
#' @export
#' @details
#'   Note that if `fail_on_error` is `TRUE` and submission of one model fails, the submission loop will stop, rather than 
#'   submitting further models.
#' @examples
#' \dontrun{
#'   submitModels("C:/Users/Michael/Mplus Runs", recursive=TRUE, showOutput=TRUE,
#'     replaceOutfile="modifiedDate", logFile="MH_RunLog.txt")
#' }
#' \dontrun{
#'   submitModels(getwd(), filefilter = "ex8.*", batch_outdir="~/mplus_batch_12")
#' }
submitModels <- function(target=getwd(), recursive=FALSE, filefilter = NULL,
    replaceOutfile="modifiedDate", Mplus_command = NULL, quiet = FALSE,
    scheduler="slurm", sched_args=NULL, env_variables=NULL, export_all=FALSE,
    cores_per_model = 1L, memgb_per_model = 8L, time_per_model="1:00:00",
    pre=NULL, post=NULL, batch_outdir=NULL, job_script_prefix=NULL,
    combine_jobs = TRUE, max_time_per_job = "24:00:00", combine_memgb_tolerance = 1, 
    combine_cores_tolerance = 2, debug = FALSE, fail_on_error = TRUE
    ) {

  checkmate::assert_character(target)
  checkmate::assert_flag(recursive)
  checkmate::assert_string(filefilter, null.ok=TRUE)
  checkmate::assert_string(replaceOutfile)
  stopifnot(replaceOutfile %in% c("always", "never", "modifiedDate"))
  checkmate::assert_string(Mplus_command, null.ok = TRUE)
  if (is.null(Mplus_command)) Mplus_command <- detectMplus() # default
  checkmate::assert_flag(quiet)
  checkmate::assert_string(scheduler)
  checkmate::assert_subset(scheduler, c("qsub", "torque", "sbatch", "slurm"))
  checkmate::assert_character(sched_args, null.ok = TRUE)
  checkmate::assert_character(env_variables, null.ok = TRUE)
  checkmate::assert_flag(export_all)
  checkmate::assert_integerish(cores_per_model, lower = 1, len = 1L)
  checkmate::assert_number(memgb_per_model, lower = 0.1)
  if (checkmate::test_number(time_per_model)) {
    time_per_model <- minutes_to_dhms(time_per_model)
  } else if (checkmate::test_string(time_per_model)) {
    time_per_model <- validate_dhms(time_per_model)
  }
  checkmate::assert_character(pre, null.ok = TRUE)
  checkmate::assert_character(post, null.ok = TRUE)
  checkmate::assert_string(batch_outdir, null.ok = TRUE)
  checkmate::assert_string(job_script_prefix, null.ok = TRUE)
  checkmate::assert_flag(debug)
  checkmate::assert_flag(fail_on_error)
  
  if (is.null(batch_outdir)) batch_outdir <- file.path(getwd(), "mplus_batch_files") # default batch files location
  if (!dir.exists(batch_outdir)) dir.create(batch_outdir, recursive = TRUE) # create batch folder if needed

  # internal note: adapted from fmri.pipeline::cluster_submit_jobs
  if (scheduler %in% c("torque", "qsub")) {
    scheduler <- "qsub"
    if (isTRUE(export_all)) {
      sched_args <- c("-V", sched_args)
    } # directive to export all environment variables to script
  } else if (scheduler %in% c("slurm", "sbatch")) {
    scheduler <- "sbatch"
    if (isTRUE(export_all)) {
      env_variables <- c(ALL = NA, env_variables)
    } # directive to export all environment variables to script
  }

  #subfunction to handle variable=value and variable combinations
  paste_args <- function(str_vec) {
    nms <- names(str_vec)
    sapply(seq_along(str_vec), function(x) {
      if (is.na(str_vec[x])) {
        return(nms[x]) # just the name of the env variable (forwards from environment)
      } else {
        # force argument to be quoted to avoid problems with spaces
        val <- ifelse(grepl("^[\"'].*[\"']$", str_vec[x], perl=TRUE), str_vec[x], paste0("\"", str_vec[x], "\""))
        return(paste0(nms[x], "=", val))
      }
    })
  }

  #pass through environment variables
  if (!is.null(env_variables)) {
    env_variables <- paste_args(env_variables) #convert to appropriate name-value pairs
    if (scheduler == "qsub") {
      env_variables <- paste("-v", paste(env_variables, collapse=","))
    } else if (scheduler == "sbatch") {
      env_variables <- paste0("--export=", paste(env_variables, collapse=","))
    }

    sched_args <- paste(sched_args, env_variables)
  }

  # currently not supporting job dependencies
  # if (!is.null(wait_jobs)) {
  #   jcomb <- paste(wait_jobs, collapse = ":") # multiple jobs are separated by colons
  #   if (scheduler == "qsub") {
  #     sched_args <- paste0(sched_args, " -W depend=", wait_signal, ":", jcomb)
  #   } else if (scheduler == "sbatch") {
  #     sched_args <- paste0(sched_args, " --dependency=", wait_signal, ":", jcomb)
  #   }
  # }

  #Use of ~/ home directory doesn't play well with call to sh later.
  if (grepl("^~/", Mplus_command, perl=TRUE)) Mplus_command <- normalizePath(Mplus_command)
  
  if (!quiet) {
    cat(c(
      paste("------Begin Mplus Model Submission: ", format(Sys.time(), "%d%b%Y %H:%M:%S"), "------", sep = ""),
      "Submission options:",
      paste("\tRecursive (run models in subdirectories):", as.character(recursive)),
      paste("\tReplace existing outfile:", replaceOutfile),
      "------"
    ), sep="\n")
  }

  #find all inp files in each element of target and return a single vector of absolute paths to inp files
  inp_files <- convert_to_filelist(target, filefilter, recursive)
  
  if(isTRUE(length(inp_files) < 1)) stop("No Mplus input files detected in the provided target: ", paste(target, collapse=", "))

  # filter out existing out files that shoudl not be re-run
  inp_files <- filter_inp_filelist(inp_files, replaceOutfile, quiet)

  if (length(inp_files) < 1L) stop("No Mplus input files to be submitted within: ", paste(target, collapse=", "))

  ####
  # build plan for scheduling jobs

  run_df <- data.frame(
    jobid = NA_character_, inp_file = inp_files, cores = cores_per_model, memgb = memgb_per_model,
    wall_time = time_per_model, sched_script = NA_character_
  )
  split_files <- lapply(inp_files, splitFilePath, normalize = TRUE)
  run_df$dir <- sapply(split_files, "[[", "directory")
  run_df$file <- sapply(split_files, "[[", "filename")
  run_df$pre <- run_df$post <- run_df$sched <- replicate(nrow(run_df), c()) # add empty list columns for pre, post, and sched

  if (!is.null(pre)) run_df$pre <- replicate(nrow(run_df), pre, simplify = FALSE) # overall pre lines for all models
  if (!is.null(post)) run_df$post <- replicate(nrow(run_df), post, simplify = FALSE) # overall post lines for all models
  if (!is.null(sched_args)) run_df$sched <- replicate(nrow(run_df), sched_args, simplify = FALSE) # overall scheduler lines for all models
  
  # unclear on this design decision, but for now, centralize all submission scripts in one folder
  ndigits <- floor(log10(abs(nrow(run_df)))) + 1
  
  # read inp files for parallel directives
  for (rr in seq_len(nrow(run_df))) {
    ff <- readLines(run_df$inp_file[rr])
    par_lines <- grep("^\\s*\\!\\s*(memgb|processors|time)\\s+", ff, ignore.case = TRUE, perl = TRUE, value = TRUE)
    sched_lines <- grep("^\\s*\\!\\s*#\\s*(SBATCH|PBS)\\s+", ff, ignore.case = TRUE, perl = TRUE, value = TRUE)
    pre_lines <- grep("^\\s*\\!\\s*pre\\s+", ff, ignore.case = TRUE, perl = TRUE, value = TRUE)
    post_lines <- grep("^\\s*\\!\\s*post\\s+", ff, ignore.case = TRUE, perl = TRUE, value = TRUE)

    if (length(par_lines) > 1L) {
      for (pp in par_lines) {
        if (grepl("^\\s*\\!\\s*time\\s+", pp, perl = TRUE, ignore.case = TRUE)) {
          run_df$wall_time[rr] <- validate_dhms(sub("^\\s*\\!\\s*time\\s+", "", pp, perl = TRUE, ignore.case = TRUE))
        } else if (grepl("^\\s*\\!\\s*processors\\s+", pp, perl = TRUE, ignore.case = TRUE)) {
          cores <- sub("^\\s*\\!\\s*processors\\s+", "", pp, perl = TRUE, ignore.case = TRUE)
          if (!grepl("^\\s*\\d+\\s*$", cores, perl = TRUE)) stop("Cannot interpret processors argument: ", cores, " for file: ", run_df$inp_file[rr])
          run_df$cores[rr] <- as.integer(cores)
        } else if (grepl("^\\s*\\!\\s*memgb\\s+", pp, perl = TRUE, ignore.case = TRUE)) {
          memgb <- sub("^\\s*\\!\\s*memgb\\s+", "", pp, perl = TRUE, ignore.case = TRUE)
          if (!grepl("^\\s*[0-9\\.]+\\s*$", memgb, perl = TRUE)) stop("Cannot interpret memgb argument: ", memgb, " for file: ", run_df$inp_file[rr])
          run_df$memgb[rr] <- as.numeric(memgb)
        }
      }
    }

    if (length(pre_lines) > 0L) {
      pre_lines <- sub("^\\s*\\!\\s*pre\\s+", "", pre_lines, ignore.case = TRUE, perl = TRUE)
      run_df$pre[[rr]] <- c(run_df$pre[[rr]], pre_lines)
    }

    if (length(post_lines) > 0L) {
      post_lines <- sub("^\\s*\\!\\s*post\\s+", "", post_lines, ignore.case = TRUE, perl = TRUE)
      run_df$post[[rr]] <- c(run_df$post[[rr]], post_lines)
    }

    if (length(sched_lines) > 0L) {
      sched_lines <- sub("^\\s*\\!\\s*#(SBATCH|PBS)\\s*", "", sched_lines, perl = TRUE)

      if (is.null(run_df$sched[[rr]])) {
        run_df$sched[[rr]] <- sched_lines
      } else {
        # ensure that we override overall arguments with model-specific arguments
        flag_this <- sub("^\\s*(-[A-z]|--[A-z\\-]+=).*", "\\1", sched_lines, perl = TRUE)
        flag_overall <- sub("^\\s*(-[A-z]|--[A-z\\-]+=).*", "\\1", run_df$sched[[rr]], perl = TRUE)
        m <- flag_overall %in% flag_this
        run_df$sched[[rr]] <- c(run_df$sched[[rr]][!m], sched_lines)
      }
    }
  }

  ####
  # chunk run_df into combined jobs if requested
  chunk_jobs <- function(combine_jobs, run_df, max_time_per_job, combine_memgb_tolerance, combine_cores_tolerance) {
    
    # first, convert inp_file, file, and dir to list columns to support multi-model rows
    run_df$inp_file <- as.list(run_df$inp_file)
    run_df$file <- as.list(run_df$file)
    run_df$dir <- as.list(run_df$dir)

    if (isFALSE(combine_jobs)) return(run_df) # no further chunking
    
    run_df$wall_hr <- sapply(run_df$wall_time, dhms_to_hours)
    time_max_hr <- dhms_to_hours(max_time_per_job)
    
    if (any(run_df$wall_hr > time_max_hr)) {
      nexceed <- sum(run_df$wall_hr > time_max_hr)
      warning(nexceed, " models have compute times longer than the allowed max_time_per_job for job combination. In these cases, single model jobs will be submitted with the compute time requested")
    }
    
    # we can only combine jobs that have the same scheduler arguments
    run_list <- run_df %>% group_by(sched) %>% group_split()
    out_list <- list()
    for (rr in run_list) {
      
      if (nrow(rr) == 1L) {
        out_list <- c(out_list, list(rr))
        next # no need to attempt chunking if only a single model has these scheduler arguments
      }
      
      # process rows of this sublist (that share scheduler arguments)
      toproc <- rep(TRUE, nrow(rr))
      
      while (any(toproc)) {
        # look for biggest jobs that can be chunked together based on memory and cores
        max_cores <- max(rr$cores[toproc])
        max_mem <- max(rr$memgb[toproc])
        elig_chunk <- which(toproc & (rr$memgb >= max_mem - combine_memgb_tolerance) & (rr$cores >= max_cores - combine_cores_tolerance))
        
        if (length(elig_chunk) == 1L) { # no model can be chunked with this one
          toproc[elig_chunk] <- FALSE
        } else {
          this_chunk <- rr[elig_chunk,]
          this_chunk <- this_chunk[order(this_chunk$wall_hr),] # sort in ascending order by wall time
          
          while (nrow(this_chunk) > 0L) {
            elig_times <- cumsum(this_chunk$wall_hr)
            
            # initialize chunked job with maximal memory and core demand
            this_job <- data.frame(jobid=NA_character_, cores=max_cores, memgb=max_mem,
                               wall_time=NA_character_, sched_script=NA_character_)
            
            included <- elig_times < time_max_hr
            # if all remaining jobs are more than the allowed max job time, revert to single jobs scheduled for their individually requested times
            if (all(included == FALSE)) included <- c(TRUE, rep(FALSE, nrow(this_chunk) - 1))
            
            this_job$inp_file <- list(unlist(this_chunk$inp_file[included]))
            this_job$dir <- list(unlist(this_chunk$dir[included]))
            this_job$file <- list(unlist(this_chunk$file[included]))
            this_job$pre <- list(this_chunk$pre[included])
            this_job$post <- list(this_chunk$post[included])
            this_job$wall_hr <- max(elig_times[included])
            this_job$wall_time <- validate_dhms(paste0(this_job$wall_hr, ":00:00"))
            this_job$sched <- this_chunk$sched[1L] # by definition, sched arguments apply to all models in this chunk
            
            out_list <- c(out_list, list(this_job))
            this_chunk <- this_chunk[!included,,drop=FALSE] # drop rows that are complete
          }
          
          toproc[elig_chunk] <- FALSE # drop this chunk now that it is done
          
        }
      }
    }
    return(do.call(rbind, out_list))
  }
  
  # handle job chunking, if requested
  run_df <- chunk_jobs(combine_jobs, run_df, max_time_per_job, combine_memgb_tolerance, combine_cores_tolerance)
  
  ####
  # loop over run_df and submit jobs
  for (rr in seq_len(nrow(run_df))) {
    batch_name <- ifelse(length(run_df$file[[rr]]) > 1L, paste0("batch", rr), sub(".inp", "", run_df$file[[rr]], fixed=TRUE))
    
    # always put job output file in the same directory as the Mplus model unless user states otherwise (also avoid .out extension)
    # if there are multiple models in a batch, put the output file in the directory of the the first model
    flags <- sub("^\\s*(-[A-z]|--[A-z\\-]+=).*", "\\1", run_df$sched[[rr]], perl = TRUE)
    if (!any(grepl("(--output=|-o)", flags, perl=T))) {
      run_df$sched[[rr]] <- c(run_df$sched[[rr]], paste0("-o ", run_df$dir[[rr]][1], "/mplus-", batch_name, "-job-%j.txt"))
    }
    
    nfiles <- length(run_df$file[[rr]])
    # core model execution code
    model_str <- "export TMPDIR=$( mktemp -d )" # create job-specific temporary directory
    for (ii in seq_len(nfiles)) {
      model_str <- c(
        model_str,
        "",
        paste0("export MPLUSDIR='", run_df$dir[[rr]][ii], "'"),
        paste0("export MPLUSINP='", run_df$file[[rr]][ii], "'"),
        paste0("cd \"", run_df$dir[[rr]][ii], "\""),
        "",
        if (!is.null(run_df$pre[[rr]][[ii]])) run_df$pre[[rr]][[ii]], # model-specific pre commands
        paste0("\"", Mplus_command, "\" \"", run_df$file[[rr]][ii], "\""),
        if (!is.null(run_df$post[[rr]][[ii]])) run_df$post[[rr]][[ii]]) # model-specific post commands
    }
    
    if (scheduler == "sbatch") {
      file_suffix <- ".sbatch"
      job_str <- c(
        "#!/bin/bash",
        "",
        "#SBATCH -N 1", # always request single node
        paste0("#SBATCH -n ", run_df$cores[rr]),
        paste0("#SBATCH --time=", run_df$wall_time[rr]),
        paste0("#SBATCH --mem=", run_df$memgb[rr], "G"),
        if (!is.null(run_df$sched[[rr]])) paste("#SBATCH", run_df$sched[[rr]]), # common and model-specific scheduler arguments
        ""
      )
    } else if (scheduler == "qsub") {
      file_suffix <- ".pbs"
      job_str <- c(
        "#!/bin/bash",
        "",
        paste0("#PBS -l nodes=1:ppn=", run_df$cores[rr]),
        paste0("#PBS -l walltime=", run_df$wall_time[rr]),
        paste0("#PBS -l mem=", run_df$memgb[rr], "gb"),
        if (!is.null(sched_args)) paste("#PBS", sched_args), # common and model-specific scheduler arguments
        ""
      )
    } else if (scheduler == "local") {
      file_suffix <- ".bash"
    }

    job_str <- na.omit(c(job_str, model_str)) # use na.omit to drop blanks from ifelse
    script <- file.path(batch_outdir, sprintf(paste0("%.", ndigits, "d_%s%s"), rr, batch_name, file_suffix))
    writeLines(job_str, con = script)
    run_df$sched_script[rr] <- script
    
    # use unique temp files to avoid parallel collisions in job tracking
    sub_stdout <- paste0(tempfile(), "_", tools::file_path_sans_ext(basename(script)), "_stdout") 
    sub_stderr <- paste0(tempfile(), "_", tools::file_path_sans_ext(basename(script)), "_stderr")
    sub_pid <- paste0(tempfile(), "_", tools::file_path_sans_ext(basename(script)), "_pid")

    if (!quiet) {
      cat(paste("Submitting model:", run_df$file[rr]), "\n")
      cat(paste(scheduler, script), "\n")
    }

    if (!debug) {
      # submit the job script and return the jobid
      jobres <- system2(scheduler, args = script, stdout = sub_stdout, stderr = sub_stderr)
      jobid <- if (file.exists(sub_stdout)) scan(file = sub_stdout, what = "char", sep = "\n", quiet = TRUE) else ""
    } else {
      jobres <- 0
      jobid <- paste0("dummy_", rr)
    }

    joberr <- if (file.exists(sub_stderr)) {
      paste(scan(file = sub_stderr, what = "char", sep = "\n", quiet = TRUE), collapse = ". ")
    } else {
      ""
    }

    if (jobres != 0) {
      jobid <- NULL
      if (isTRUE(fail_on_error)) {
        stop("Job submission failed: ", script, ", error: ", joberr, ", errcode: ", jobres)
      } else {
        warning("Job submission failed: ", script, ", error: ", joberr, ", errcode: ", jobres)
      }
    } else {
      jobid <- sub("Submitted batch job ", "", jobid, fixed = TRUE) # replace irrelevant details if needed
    }

    run_df$jobid[rr] <- jobid
  }

  if (!quiet)  cat(c("", paste("------End Mplus Model Submission: ", format(Sys.time(), "%d%b%Y %H:%M:%S"), "------", sep = "")), "\n")

  # tag to allow S3 methods and checks on job status
  class(run_df) <- c("mplus_submission_df", class(run_df))
  attr(run_df, "scheduler") <- scheduler
  
  return(run_df)
}



#' This function pauses execution of an R script while a scheduled qsub job is not yet complete.
#'
#' It is intended to give you control over job dependencies within R when the formal PBS
#' depend approach is insufficient, especially in the case of a script that spawns child jobs that
#' need to be scheduled or complete before the parent script should continue.
#'
#' @param job_ids One or more job ids of existing PBS or slurm jobs, or process ids of a local process for
#'   \code{scheduler="sh"}.
#' @param scheduler What scheduler is used for job execution.
#'   Options: c("torque", "qsub", "slurm", "sbatch", "sh", "local")
#' @param quiet If \code{TRUE}, \code{wait_for_job} will not print out any status updates on jobs. If \code{FALSE},
#'   the function prints out status updates for each tracked job so that the user knows what's holding up progress.
#'
#' @return A vector of job statuses corresponding to each job id
#'
#' @details Note that for the \code{scheduler} argument, "torque" and "qsub" are the same;
#'   "slurm" and "sbatch" are the same, and "sh" and "local" are the same.
#' @examples
#' \dontrun{
#' # example on qsub/torque cluster
#' get_job_status("7968857.torque01.util.production.int.aci.ics.psu.edu", scheduler = "torque")
#'
#' # example of checking two jobs on slurm cluster
#' get_job_status(c("24147864", "24147876"), scheduler = "slurm")
#'
#' # example of checking two jobs on local machine
#' get_job_status(c("9843", "9844"), scheduler = "local")
#' }
#'
#' @author Michael Hallquist
#' @keywords internal
get_job_status <- function(job_ids, scheduler = "slurm", quiet = TRUE) {
  scheduler <- tolower(scheduler) # ignore case
  checkmate::assert_subset(scheduler, c("torque", "qsub", "slurm", "sbatch", "sh", "local"))

  job_complete <- FALSE
  wait_start <- Sys.time()

  jstat <- function() { # use variables in parent environment
    if (scheduler %in% c("slurm", "sbatch")) {
      status <- slurm_job_status(job_ids)
      state <- sapply(status$State, function(x) {
        switch(x,
          "BOOT_FAIL" = "failed",
          "CANCELLED" = "cancelled",
          "COMPLETED" = "complete",
          "DEADLINE" = "failed",
          "FAILED" = "failed",
          "NODE_FAIL" = "failed",
          "OUT_OF_MEMORY" = "failed",
          "PENDING" = "queued",
          "PREEMPTED" = "failed",
          "RUNNING" = "running",
          "REQUEUED" = "queued",
          "REVOKED" = "failed",
          "SUSPENDED" = "suspended",
          "TIMEOUT" = "failed",
          "MISSING" = "missing" # scheduler has not registered the job
        )
      })
    } else if (scheduler %in% c("sh", "local")) {
      status <- local_job_status(job_ids)
      state <- sapply(status$STAT, function(x) {
        switch(x,
          "C" = "complete",
          "I" = "running", # idle/sleeping
          "R" = "running",
          "S" = "running", # sleeping
          "T" = "suspended",
          "U" = "running",
          "Z" = "failed", # zombie
          stop("Unable to understand job state: ", x)
        )
      })
    } else if (scheduler %in% c("torque", "qsub")) {
      # QSUB
      status <- torque_job_status(job_ids)
      state <- status$State

      # no need for additional mapping in simple torque results
      # state <- sapply(status$State, function(x) {
      #   switch(x,
      #     "C" = "complete",
      #     "R" = "running",
      #     "Q" = "queued",
      #     "H" = "suspended",
      #     "W" = "suspended", # waiting
      #     stop("Unable to understand job state: ", x)
      #   )
      # })
    } else {
      stop("unknown scheduler: ", scheduler)
    }
    return(state)
  }

  ret_code <- NULL # should be set to TRUE if all jobs complete and FALSE if any job fails

  status <- jstat()
  return(status)
}

#' check the status of jobs submitted on a SLURM cluster
#'
#' @param job_ids a vector of job ids to check
#' @param user the user who submitted the jobs
#' @param sacct_format the format string passed to sacct for job status
#' @details
#'   This function calls \code{sacct -j} to check the status of jobs on a slurm cluster
#' @return a data.frame containing the status of each job id
#' @keywords internal
slurm_job_status <- function(job_ids = NULL, user = NULL, sacct_format = "jobid,submit,timelimit,start,end,state") {
  if (!is.null(job_ids)) {
    jstring <- paste("-j", paste(job_ids, collapse = ","))
  } else {
    jstring <- ""
  }

  if (!is.null(user)) {
    ustring <- paste("-u", paste(user, collapse = ","))
  } else {
    ustring <- ""
  }

  # -P specifies a parsable output separated by pipes
  # -X avoids printing subsidiary jobs within each job id
  #cmd <- paste("sacct", jstring, ustring, "-X -P -o", sacct_format)
  cmd <- paste(jstring, ustring, "-X -P -o", sacct_format)
  # cat(cmd, "\n")
  res <- system2("sacct", args = cmd, stdout = TRUE)

  df_base <- data.frame(JobID = job_ids)
  df_empty <- df_base %>%
    mutate(
      Submit = NA_character_,
      Timelimit = NA_character_,
      Start = NA_character_,
      End = NA_character_,
      State = "MISSING"
    )

  # handle non-zero exit status -- return empty data
  if (!is.null(attr(res, "status"))) {
    warning("sacct call generated non-zero exit status")
    print(cmd)
    return(df_empty)
  }

  out <- data.table::fread(text = res, data.table=FALSE)

  if (!checkmate::test_subset(c("JobID", "State"), names(out))) {
    warning("Missing columns in sacct output")
    return(df_empty)
  }

  out$JobID <- as.character(out$JobID)
  df <- merge(df_base, out, by="JobID")
  df$State[is.na(df$State)] <- "MISSING" # indicate missing job states
  
  return(df)
}

#' helper function to get the status of jobs on a TORQUE cluster
#' @param job_ids a vector of job ids to check
#' @param user the user who submitted the jobs
#' @return a data.frame with the job status for each id
#' @details
#'    Torque does not keep information about completed jobs available in qstat or qselect.
#'    Thus, we need to log when a job is listed as queued, so that it 'going missing' is evidence of it being completed.
#' @keywords internal
torque_job_status <- function(job_ids, user = NULL) {
  
  # res <- system2("qstat", args = paste("-f", paste(job_ids, collapse=" "), "| grep -i 'job_state'"), stdout = TRUE)

  q_jobs <- system2("qselect", args = "-u $USER -s QW", stdout = TRUE) # queued jobs
  r_jobs <- system2("qselect", args = "-u $USER -s EHRT", stdout = TRUE) # running jobs
  c_jobs <- system2("qselect", args = "-u $USER -s C", stdout = TRUE) # complete jobs
  m_jobs <- setdiff(job_ids, c(q_jobs, r_jobs, c_jobs)) # missing jobs
  # state <- c("queued", "running", "complete", "missing")
  state <- c("queued", "running", "complete", "complete")

  # TORQUE clusters only keep jobs with status C (complete) for a limited period of time. After that, the job comes back as missing.
  # Because of this, if one job finishes at time X and another finishes at time Y, job X will be 'missing' if job Y takes a very long time.
  # Thus, we return any missing jobs as complete, which could be problematic if they are truly missing immediately after submission (as happened with slurm).
  # Ideally, we would track a job within wait_for_job such that it can be missing initially, then move into running, then move into complete.

  j_list <- list(q_jobs, r_jobs, c_jobs, m_jobs)
  state_list <- list()
  for (ii in seq_along(j_list)) {
    if (length(j_list[[ii]]) > 0L) {
      state_list[[state[ii]]] <- data.frame(JobID = j_list[[ii]], State = state[ii])
    }
  }

  state_df <- do.call(rbind, state_list)

  if (!is.null(attr(q_jobs, "status"))) {
    warning("qselect call generated non-zero exit status")
    return(data.frame(JobID = job_ids, State = "missing"))
  }

  # job_state <- sub(".*job_state = ([A-z]).*", "\\1", res, perl = TRUE)

  return(state_df)
}

#' helper function to get job status on local *nix-based machine
#'
#' @param job_ids a vector of job ids (process IDs) to check
#' @param user the user who owns the processes (defaults to current user)
#' @param ps_format the format string passed to ps
#' @return a data.table with job information for each job id
#' @importFrom data.table setnames fread
#' @importFrom utils type.convert
#' @keywords internal
local_job_status <- function(job_ids = NULL, user = NULL,
                             ps_format = "user,pid,state,time,etime,%cpu,%mem,comm,xstat") {
  job_ids <- type.convert(job_ids, as.is = T) # convert to integers
  checkmate::assert_integerish(job_ids)

  if (!is.null(job_ids)) {
    jstring <- paste("-p", paste(job_ids, collapse = ","))
  } else {
    jstring <- ""
  }

  if (!is.null(user)) {
    ustring <- paste("-u", paste(user, collapse = ","))
  } else {
    ustring <- ""
  }

  # cat(paste("ps", jstring, ustring, "-o", ps_format), sep = "\n")
  res <- suppressWarnings(system2("ps", args = paste(jstring, ustring, "-o", ps_format), stdout = TRUE))

  # need to trap res of length 1 (just header row) to avoid data.table bug.
  if (!is.null(attr(res, "status")) && attr(res, "status") != 0) {
    hrow <- strsplit(res, "\\s+")[[1]]
    dt <- data.frame(matrix(NA, nrow = length(job_ids), ncol = length(hrow)))
    names(dt) <- hrow
    dt$PID <- job_ids
  } else {
    stopifnot(length(res) > 1)
    # fread and any other parsing can break down with consecutive spaces in body of output.
    # This happens with lstart and start, avoid these for now.
    # header <- gregexpr("\\b", res[1], perl = T)
    # l2 <- gregexpr("\\b", res[2], perl=T)
    dt <- data.table::fread(text = res)
  }

  # fix difference in column naming between FreeBSD and *nux (make all like FreeBSD)
  data.table::setnames(dt, c("S", "COMMAND"), c("STAT", "COMM"), skip_absent = TRUE)

  # build df that fills in missing jobs (completed/killed)
  all_dt <- merge(
    data.frame(PID = as.integer(job_ids)),
    dt,
    by = "PID", all = TRUE
  )

  all_dt$STAT <- substr(all_dt$STAT, 1, 1) # only care about first character of state

  all_dt$STAT[is.na(all_dt$STAT)] <- "C" # complete

  return(all_dt)
}

#' check on the status of submitted Mplus jobs on the cluster
#' @param mplus_submission_df The data.frame returned by \code{submitModels} containing
#'   jobs to check on
#' @param quiet If \code{TRUE}, do not print out the submission data.frame with current status
#' @return invisibly, the \code{mplus_submission_df} with `$status` amd `$status_time` updated
#' @export
checkSubmission <- function(mplus_submission_df = NULL, quiet = FALSE) {
  checkmate::assert_class(mplus_submission_df, "mplus_submission_df")
  mplus_submission_df$status <- get_job_status(mplus_submission_df$jobid, attr(mplus_submission_df, "scheduler"))
  mplus_submission_df$status_time <- as.character(Sys.time())

  if (!quiet) {
    cat("Submission status as of:", mplus_submission_df$status_time[1L], "\n-------\n")
    print(mplus_submission_df[,c("jobid", "file", "status")])
  }
  return(invisible(mplus_submission_df))
}

#' summary function for submission from \code{submitModels}
#' @param x the \code{mplus_submission_df} object to summarize
#' @param refresh if \code{TRUE}, check the status of jobs for this object before printing
#' @method summary mplus_submission_df
#' @importFrom utils head
#' @export
summary.mplus_submission_df <- function(x, refresh=TRUE, ...) {
  checkmate::assert_class(x, "mplus_submission_df")
  checkmate::assert_flag(refresh)
  if (is.null(x$status)) x$status <- "missing"

  if (refresh) x <- tryCatch(checkSubmission(x, quiet = TRUE), error = function(e) x)

  cat("Number of models in submission: ", nrow(x), "\n")
  cat("Complete jobs: ", sum(x$status == "complete"), "\n")
  cat("Queued jobs: ", sum(x$status == "queued"), "\n")
  cat("Still running: ", sum(x$status == "running"), "\n")

  rdf <- x[x$status == "running", ]
  if (nrow(rdf) > 0L) {
    print(head(rdf, n=20))
  }
}