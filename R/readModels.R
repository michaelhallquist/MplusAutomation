#' Read Parameters, Summary Statistics, and Savedata from Mplus Output
#'
#' Extracts information from one or more Mplus output files, including fit statistics and parameters.
#' Its is to parse all (supported) aspects of Mplus output and to combine
#' these into a list object, with one element per output file identified.
#'
#' @param target the directory containing Mplus output files (.out) to
#'   	parse OR the single output file to be parsed. May be a full
#'   	path, relative path, or a filename within the working
#'   	directory. Defaults to the current working directory. Example:
#'   	"C:/Users/Michael/Mplus Runs"
#' @param recursive optional. If \code{TRUE}, parse all models nested in
#'   	subdirectories within \code{target}. Defaults to \code{FALSE}.
#' @param filefilter a Perl regular expression (PCRE-compatible)
#'   	specifying particular output files to be parsed within
#'   	\code{directory} based on their file name. See \code{regex} or
#'   	\url{http://www.pcre.org/pcre.txt} for details about regular
#'   	expression syntax.
#' @param pathfilter a Perl regular expression (PCRE-compatible) specifying particular
#'   paths to be parsed within \code{directory} based on their absolute paths.
#' @param what a character vector denoting what aspects of Mplus output to extract.
#'    Defaults to \code{"all"}, which will extract all supported output sections.
#'    See details for additional information.
#' @param quiet whether to suppress printing to the screen the file currently
#' being processed. Defaults to TRUE.
#'
#' @details
#'
#' The \code{what} parameter defaults to "all", which extracts all supported output. If you would like to extract a
#' reduced set of output sections (especially to speed up the function when reading many files), specify the sections
#' as a character vector from the following options:
#'
#' c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries",
#'      "parameters", "class_counts", "indirect", "mod_indices", "residuals",
#'      "savedata", "bparameters", "tech1", "tech3", "tech4", "tech7", "tech8",
#'      "tech9", "tech10", "tech12", "fac_score_stats", "lcCondMeans", "gh5",
#'      "output")
#'
#'
#' @return A list with one mplus.model per file. Each mplus.model object is composed of
#' elements containing major output sections, as detailed below. If
#' \code{target} is a single file, then the top-level elements will be
#' a single mplus.model object, not a list of files. Specific elements are:
#'   \item{input}{Mplus input syntax parsed into a list by major section}
#'   \item{warnings}{Syntax and estimation warnings as a list}
#'   \item{errors}{Syntax and estimation errors as a list}
#'   \item{data_summary}{Output of SUMMARY OF DATA section, including cluster sizes and ICCs}
#'   \item{sampstat}{Sample statistics provided by OUTPUT: SAMPSTAT, if specified}
#'   \item{covariance_coverage}{Covariance coverage matrix for checking missingness patterns}
#'   \item{summaries}{Summary statistics from \code{extractModelSummaries}, having structure as specified by that function}
#'   \item{parameters}{Model parameters from \code{extractModelParameters}, having structure as specified by that function}
#'   \item{class_counts}{Latent class counts and proportions for models that include a categorical latent variable}
#'   \item{indirect}{Output of MODEL INDIRECT if available in output. Contains \code{$overall} and \code{$specific} data.frames for each indirect effect section}
#'   \item{mod_indices}{Model modification indices from \code{extractModIndices}, having structure as specified by that function}
#'   \item{residuals}{a list containing relevant information from OUTPUT: RESIDUALS}
#'   \item{savedata_info}{File information about SAVEDATA files related to this output}
#'   \item{savedata}{SAVEDATA file as an R \code{data.frame}, as described in \code{getSavedata_Data}}
#'   \item{bparameters}{an \code{mcmc.list} object containing the draws from the MCMC chains for a Bayesian model that uses the
#'   	SAVEDATA: BPARAMETERS command}
#'   \item{tech1}{a list containing parameter specification and starting values from OUTPUT: TECH1}
#'   \item{tech3}{a list containing parameter covariance and correlation matrices from OUTPUT: TECH3}
#'   \item{tech4}{a list containing means, covariances, and correlations for latent variables from OUTPUT: TECH4}
#'   \item{tech7}{a list containing sample statistics for each latent class from OUTPUT: TECH7}
#'   \item{tech8}{a list containing optimization history of the model. Currently only supports potential scale reduction in BAYES. OUTPUT: TECH8}
#'   \item{tech9}{a list containing warnings/errors from replication runs for MONTECARLO analyses from OUTPUT: TECH9}
#'   \item{tech10}{a list containing model fit information from OUTPUT: TECH10}
#'   \item{tech12}{a list containing observed versus estimated sample statistics for TYPE=MIXTURE analyses from OUTPUT: TECH12}
#'   \item{fac_score_stats}{factor score mean, correlation, and covariance structure from SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES section}
#'   \item{lcCondMeans}{conditional latent class means and pairwise comparisons, obtained using auxiliary(e) syntax in latent class models}
#'   \item{r3step}{predictors of latent class membership using the 3-step procedure (R3STEP)}
#'   \item{gh5}{a list containing data from the gh5 (graphics) file corresponding to this output. (Requires rhdf5 package)}
#'   \item{h5results}{a list containing data from h5results file produced by Mplus v8.11+. (Requires rhdf5 package)}
#'   \item{output}{The entire, raw output file.}
#' @author Michael Hallquist
#' @keywords interface
#' @export
#' @examples
#' \dontrun{
#'   allOutput <- readModels(
#'     "C:/Program Files/Mplus/Mplus Examples/User's Guide Examples", recursive=TRUE)
#' }
readModels <- function(target=getwd(), recursive=FALSE, filefilter, pathfilter, what="all", quiet=TRUE) {
  #large wrapper function to read summaries, parameters, and savedata from one or more output files.

  ## enforce quiet being logical and length 1 as used in if else statements
  stopifnot(identical(length(quiet), 1L) && is.logical(quiet))
  allsections <- c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries",
      "invariance_testing", "parameters", "class_counts", "indirect", "mod_indices", "residuals",
      "savedata", "bparameters", "tech1", "tech3", "tech4", "tech7", "tech8",
      "tech9", "tech10", "tech12", "tech15", "fac_score_stats", "lcCondMeans", "r3step", 
      "gh5", "h5results", "output")

  if (isTRUE(what[1L] == "all")) {
    what <- allsections
  } else {
    if (isTRUE(any(whichneg <- grepl("^-", what, perl=TRUE)))) {
      negate <- sub("^-", "", what[whichneg], perl=TRUE)
      pos <- what[!whichneg] #this doesn't have any use at the moment
      what <- c(allsections[!allsections %in% negate])
    }
  }

  outfiles <- getOutFileList(target, recursive, filefilter)

  allFiles <- list()
  
  ## Loop over files and add the results of each to the allFiles list
  for (curfile in outfiles) {
    if (isFALSE(quiet)) { cat("Reading model: ", curfile, "\n") }

    #if not recursive, then each element is uniquely identified (we hope!) by filename alone
    if (isFALSE(recursive))	listID <- make.names(splitFilePath(curfile)$filename) #each list element is named by the respective file
    else listID <- make.names(curfile) #each list element is named by the respective file

    rawtext <- readLines(curfile)
    outfiletext <- parse_into_sections(rawtext) #identify and cache top-level header section

    ### COMPULSORY SECTIONS NEEDED FOR DOWNSTREAM FUNCTIONS: input, summaries, savedata_info
    
    ### Parse Mplus input into a list by section
    inp <- tryCatch(extractInput_1file(rawtext, curfile), error=function(e) {
      message("Error parsing input section of output file: ", curfile); print(e)
      return(list())
    })
    
    #Model summary output (including MODEL FIT INFORMATION)
    summaries <- tryCatch(extractSummaries_1file(outfiletext, curfile, input=inp), error=function(e) {
      message("Error extracting model summaries in output file: ", curfile); print(e)
      return(list())
    })
    
    #SAVEDATA file information -- always extract so that downstream code (e.g., bparameters and h5results) can find info if needed
    savedata_info <- tryCatch(l_getSavedata_Fileinfo(curfile, outfiletext, summaries), error=function(e) {
      message("Error extracting SAVEDATA file information in output file: ", curfile); print(e)
      return(list())
    })
    
    if (isTRUE("warn_err" %in% what)) {
      #Parse warnings and errors in output file
      warn_err <- tryCatch(extractWarningsErrors_1file(outfiletext, curfile, input=inp), error=function(e) {
            message("Error parsing warnings and errors in output file: ", curfile); print(e)
            return(list())
          })

      allFiles[[listID]]$warnings <- warn_err$warnings
      allFiles[[listID]]$errors <- warn_err$errors
    }

    if (isTRUE("data_summary" %in% what)) {
      #SUMMARY OF DATA output
      allFiles[[listID]]$data_summary <- tryCatch(extractDataSummary(outfiletext, curfile), error=function(e) {
          message("Error extracting SUMMARY OF DATA in output file: ", curfile); print(e)
          return(list())
        })
    }

    if (isTRUE("sampstat" %in% what)) {
      #SAMPSTAT output
      allFiles[[listID]]$sampstat <- tryCatch(extractSampstat(outfiletext, curfile), error=function(e) {
            message("Error extracting SAMPSTAT in output file: ", curfile); print(e)
            return(list())
          })
    }

    if (isTRUE("covariance_coverage" %in% what)) {
      #COVARIANCE COVERAGE OF DATA output
      allFiles[[listID]]$covariance_coverage <- tryCatch(extractCovarianceCoverage(outfiletext, curfile), error=function(e) {
            message("Error extracting COVARIANCE COVERAGE OF DATA in output file: ", curfile); print(e)
            return(list())
          })
    }

    if (isTRUE("summaries" %in% what)) {
      
      # Fix for multigroup models, where Observations were not parsed correctly
      # Some applications need the number of observations by group. I'm
      # including them as an attribute of the $summaries data.frame, so any
      # downstream operations on $summaries don't break down due to different
      # column names / number of columns.

      if (isFALSE(is.null(summaries))) {
        if (isFALSE(is.na(summaries[["NGroups"]])) && isTRUE(summaries[["NGroups"]] > 1)) {
          obs <- outfiletext[(grep("^\\s*(Average )*Number of observations\\s*", outfiletext, ignore.case = TRUE)[1L] + 1):(grep("^\\s*(Total sample size|Number of dependent variables|Number of replications)", outfiletext)[1L] - 1)]
          obs <- gsub("Group", "", obs)
          obs <- unlist(strsplit(trimws(obs), "\\s+"))
          if (isTRUE(length(obs) %% 2 == 0)) {
            Observations <- as.numeric(obs[seq(2, to = length(obs), by = 2)])
            names(Observations) <- obs[seq(1, to = length(obs), by = 2)]
            attr(summaries, "Observations") <- Observations
          }
        }
      }
    }

    if (isTRUE("invariance_testing" %in% what)) {
      #Invariance Testing output (v8)
      allFiles[[listID]]$invariance_testing <- tryCatch(extractInvarianceTesting(outfiletext, curfile), error=function(e) {
          message("Error extracting invarinace testing in output file: ", curfile); print(e)
          return(list())
        })
    }

    is_efa <- any(grepl(
        "(EXPLORATORY FACTOR ANALYSIS WITH \\d+ FACTOR\\(S\\):|EXPLORATORY FACTOR ANALYSIS WITH \\d+ WITHIN FACTOR\\(S\\) AND \\d+ BETWEEN FACTOR\\(S\\):)",
        outfiletext,
      perl = TRUE
    ))
    
    if (isTRUE("parameters" %in% what)) {
      #Model parameters (MODEL RESULTS section)
      allFiles[[listID]]$parameters <- tryCatch(extractParameters_1file(outfiletext, curfile, efa = is_efa), error=function(e) {
            message("Error extracting MODEL RESULTS in output file: ", curfile); print(e)
            return(list())
          })
    }

    if (isTRUE("class_counts" %in% what)) {
      #Latent class counts
      allFiles[[listID]]$class_counts <- tryCatch(extractClassCounts(outfiletext, curfile, summaries), error=function(e) {
            message("Error extracting latent class counts in output file: ", curfile); print(e)
            return(list())
          })
      
      #add total number of latent classes to summaries (in case of multiple categorical latent variables, this is just the product)
      if (isTRUE("summaries" %in% what) && !is.null(allFiles[[listID]]$class_counts$modelEstimated)) {
        summaries$NLatentClasses <- nrow(allFiles[[listID]]$class_counts$modelEstimated)
      }
    }

    if (isTRUE("indirect" %in% what)) {
      #MODEL INDIRECT
      allFiles[[listID]]$indirect <- tryCatch(extractIndirect(outfiletext, curfile), error=function(e) {
            message("Error extracting MODEL INDIRECT in output file: ", curfile); print(e)
            return(list())
          })
    }

    if (isTRUE("mod_indices" %in% what)) {
      #MODINDICES
      allFiles[[listID]]$mod_indices <- tryCatch(extractModIndices_1file(outfiletext, curfile), error=function(e) {
            message("Error extracting MODINDICES in output file: ", curfile); print(e)
            return(list())
          })
    }

    if (isTRUE("residuals" %in% what)) {
      #Extract model residuals (RESIDUAL)
      allFiles[[listID]]$residuals <- tryCatch(extractResiduals(outfiletext, curfile), error=function(e) {
            message("Error extracting RESIDUAL section in output file: ", curfile); print(e)
            return(list())
          })
    }

    if (isTRUE("savedata" %in% what)) {
      #missing widths indicative of MI/MC run
      if (isFALSE(is.null(savedata_info)) && isTRUE(is.na(savedata_info[["fileVarWidths"]]))) {
        allFiles[[listID]]$savedata <- tryCatch(getSavedata_readRawFile(curfile, outfiletext, format="free", fileName=savedata_info[["fileName"]], varNames=savedata_info[["fileVarNames"]], input=inp),
            error=function(e) {
              message("Error reading SAVEDATA rawfile: ", savedata_info[["fileName"]] , " in output file: ", curfile); print(e)
              return(list())
            })
      } else {
        allFiles[[listID]]$savedata <- tryCatch(getSavedata_readRawFile(curfile, outfiletext, format="fixed", fileName=savedata_info[["fileName"]], varNames=savedata_info[["chunkVarNames"]], varWidths=savedata_info[["chunkVarWidths"]], input=inp),
            error=function(e) {
              message("Error reading SAVEDATA rawfile: ", savedata_info[["fileName"]] , " in output file: ", curfile); print(e)
              return(list())
            })
      }
    }

    if (isTRUE("bparameters" %in% what)) {
      #Read BPARAMETERS (posterior draws) file from disk
      allFiles[[listID]]$bparameters <- tryCatch(l_getSavedata_Bparams(curfile, outfiletext, savedata_info, discardBurnin=FALSE), error=function(e) {
            message("Error reading BPARAMETERS file: ", savedata_info[["bayesFile"]], " in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECHNICAL OUTPUT
    if (isTRUE("tech1" %in% what)) {
      #TECH1: parameter specification and starting values
      allFiles[[listID]]$tech1 <- tryCatch(extractTech1(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH1 in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECH3: covariance/correlation matrix of parameter estimates
    if (isTRUE("tech3" %in% what)) {
      allFiles[[listID]]$tech3 <- tryCatch(extractTech3(outfiletext, savedata_info, curfile), error=function(e) {
            message("Error extracting TECH3 in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECH4: latent means
    if (isTRUE("tech4" %in% what)) {
      allFiles[[listID]]$tech4 <- tryCatch(extractTech4(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH4 in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECH7: sample stats for each class
    if (isTRUE("tech7" %in% what)) {
      allFiles[[listID]]$tech7 <- tryCatch(extractTech7(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH7 in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECH8: optimization history and chain tests in Bayes
    if (isTRUE("tech8" %in% what)) {
      allFiles[[listID]]$tech8 <- tryCatch(extractTech8(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH8 in output file: ", curfile); print(e)
            lempty <- list(); class(lempty) <- c("mplus.tech8", "list")
            return(lempty)
          })
    }

    #TECH9: errors and warnings for Monte Carlo output
    if (isTRUE("tech9" %in% what)) {
      allFiles[[listID]]$tech9 <- tryCatch(extractTech9(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH9 in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECH10: errors and warnings for model fit info
    if ("tech10" %in% what) {
      allFiles[[listID]]$tech10 <- tryCatch(extractTech10(outfiletext, curfile), error=function(e) {
        message("Error extracting TECH10 in output file: ", curfile); print(e)
        return(list())
      })
    }    
    
    ### TECH12: observed versus estimated sample stats for TYPE=MIXTURE
    if (isTRUE("tech12" %in% what)) {
      allFiles[[listID]]$tech12 <- tryCatch(extractTech12(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH12 in output file: ", curfile); print(e)
            return(list())
          })
    }

    ### TECH15: CONDITIONAL PROBABILITIES FOR THE CLASS VARIABLES for TYPE=MIXTURE
    if (isTRUE("tech15" %in% what)) {
      allFiles[[listID]]$tech15 <- tryCatch(extractTech15(outfiletext, curfile), error=function(e) {
        message("Error extracting TECH15 in output file: ", curfile); print(e)
        return(list())
      })
    }
    
    ### Factor score statistics
    if (isTRUE("fac_score_stats" %in% what)) {
      allFiles[[listID]]$fac_score_stats <- extractFacScoreStats(outfiletext, curfile) #factor scores mean, cov, corr assoc with PLOT3
    }

    if (isTRUE("lcCondMeans" %in% what)) {
      # aux(e) means and pairwise comparisons
      allFiles[[listID]]$lcCondMeans <- extractAux(outfiletext, curfile)
    }
    
    ### R3Step output (3-step procedure)
    if (isTRUE("r3step" %in% what)) {
      # 3-step logistic regression approach
      allFiles[[listID]]$r3step <- tryCatch(extractR3step(outfiletext, curfile), error=function(e) {
        message("Error extracting R3STEP in output file: ", curfile); print(e)
        return(list())
      })
    }

    #add class tag for use with compareModels
    class(allFiles[[listID]]) <- c("mplus.model", "list")
    attr(allFiles[[listID]], "filename") <- curfile

    #cleanup summary columns containing only NAs
    for (col in names(summaries)) {
      if (isTRUE(all(is.na(summaries[[col]])))) {
        summaries[[col]] <- NULL
      }
    }

    ### GH5 file
    gh5 <- list() # default empty field
    if (isTRUE("gh5" %in% what)) {
      #check for gh5 file, and load if possible
      gh5fname <- sub("^(.*)\\.out$", "\\1.gh5", curfile, ignore.case=TRUE, perl=TRUE)
      gh5 <- read_h5(gh5fname)
    }
    allFiles[[listID]]$gh5 <- gh5
    
    ### H5 results
    h5results <- list() # default empty field
    if (isTRUE("h5results" %in% what) && isFALSE(is.null(savedata_info)) && !is.na(savedata_info$h5resultsFile[1L])) {
      #check for h5results file, and load if possible
      h5resultsfname <- savedata_info$h5resultsFile[1L]
      
      if (is.na(splitFilePath(h5resultsfname)$directory)) { # if just a local file, add curfile directory
        h5resultsfname <- file.path(dirname(curfile), h5resultsfname)
      }
      
      h5results <- read_h5(h5resultsfname)
    }
    allFiles[[listID]]$h5results <- h5results
    
    ### Text of .out file
    if (isTRUE("output" %in% what)) {
      allFiles[[listID]]$output <- rawtext
    }
    
    # add compulsory sections to object if requested by user
    if (isTRUE("input" %in% what)) allFiles[[listID]]$input <- inp
    if (isTRUE("summaries" %in% what)) allFiles[[listID]]$summaries <- summaries
    if (isTRUE("savedata" %in% what)) allFiles[[listID]]$savedata_info <- savedata_info
  }

  if (identical(length(outfiles), 1L)) {
    allFiles <- allFiles[[1]] #no need for single top-level element when there is only one file
  } else {
    class(allFiles) <- c("mplus.model.list", "list")
  }

  return(allFiles)
}

# helper function to read hdf5 files
read_h5 <- function(file) {
  if (!file.exists(file)) return(list()) # empty return
  
  if (isTRUE(requireNamespace("rhdf5", quietly = TRUE))) {
    h5 <- tryCatch({ rhdf5::h5dump(file=file, recursive=TRUE, load=TRUE) },
                          error = function(e) NULL)
    
    # The h5dump error seems to occur in some rhdf5 versions when h5dump is used on
    # a file with a non-standard file extension (as opposed to the canonical .h5).
    # Thus, attempt a file extension workaround.
    if (is.null(h5[1L])) {
      tmp_h5 <- tempfile(fileext=".h5")
      file.copy(h5, to=tmp_h5)
      h5 <- tryCatch({rhdf5::h5dump(file=tmp_h5, recursive=TRUE, load=TRUE)},
                            error = function(e) list() )
      unlink(tmp_h5) #cleanup temp
    }
  } else {
    warning(paste(c("Unable to read hdf5 file because rhdf5 package not installed.\n",
                    "To install, in an R session, type:\n",
                    "  install.packages(\"BiocManager\")\n",
                    "  BiocManager::install(\"rhdf5\")\n")))
    
    h5 <- list()
  }
  
  return(h5)
}
