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
#'   	\code{directory}. See \code{regex} or
#'   	\url{http://www.pcre.org/pcre.txt} for details about regular
#'   	expression syntax.
#' @param what a character vector denoting what aspects of Mplus output to extract.
#'    Defaults to \code{"all"}, which will extract all supported output sections.
#'    See details for additional information.
#' @param quiet whether to suppress printing to the screen the file currently being processed. Defaults to FALSE.
#'
#' @details
#'
#' The \code{what} parameter defaults to "all", which extracts all supported output. If you would like to extract a
#' reduced set of output sections (especially to speed up the function when reading many files), specify the sections
#' as a character vector from the following options:
#'
#' c("input", "warn_err", "sampstat", "covariance_coverage", "summaries",
#'      "parameters", "class_counts", "indirect", "mod_indices", "residuals",
#'      "savedata", "bparameters", "tech1", "tech3", "tech4", "tech7", "tech8",
#'      "tech9", "tech12", "fac_score_stats", "lcCondMeans", "gh5")
#'
#'
#' @return A list with one mplus.model per file. Each mplus.model object is composed of
#' elements containing major output sections, as detailed below. If
#' \code{target} is a single file, then the top-level elements will be
#' a single mplus.model object, not a list of files. Specific elements are:
#'   \item{input}{Mplus input syntax parsed into a list by major section}
#'   \item{warnings}{Syntax and estimation warnings as a list}
#'   \item{errors}{Syntax and estimation errors as a list}
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
#'   \item{tech12}{a list containing observed versus estimated sample statistics for TYPE=MIXTURE analyses from OUTPUT: TECH12}
#'   \item{fac_score_stats}{factor score mean, correlation, and covariance structure from SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES section}
#'   \item{lcCondMeans}{conditional latent class means and pairwise comparisons, obtained using auxiliary(e) syntax in latent class models}
#'   \item{gh5}{a list containing data from the gh5 (graphics) file corresponding to this output. (Requires rhdf5 package)}
#' @author Michael Hallquist
#' @keywords interface
#' @export
#' @examples
#' \dontrun{
#'   allOutput <- readModels(
#'     "C:/Program Files/Mplus/Mplus Examples/User's Guide Examples", recursive=TRUE)
#' }
readModels <- function(target=getwd(), recursive=FALSE, filefilter, what="all", quiet=FALSE) {
  #large wrapper function to read summaries, parameters, and savedata from one or more output files.

  allsections <- c("input", "warn_err", "sampstat", "covariance_coverage", "summaries",
      "parameters", "class_counts", "indirect", "mod_indices", "residuals",
      "savedata", "bparameters", "tech1", "tech3", "tech4", "tech7", "tech8",
      "tech9", "tech12", "fac_score_stats", "lcCondMeans", "gh5")

  if (what[1L] == "all") {
    what <- allsections
  } else {
    if (any(whichneg <- grepl("^-", what, perl=TRUE))) {
      negate <- sub("^-", "", what[whichneg], perl=TRUE)
      pos <- what[!whichneg] #this doesn't have any use at the moment
      what <- c(allsections[!allsections %in% negate])
    }
  }

  outfiles <- getOutFileList(target, recursive, filefilter)

  allFiles <- list()
  for (curfile in outfiles) {
    if (!quiet) { cat("Reading model: ", curfile, "\n") }

    #if not recursive, then each element is uniquely identified (we hope!) by filename alone
    if (recursive==FALSE)	listID <- make.names(splitFilePath(curfile)$filename) #each list element is named by the respective file
    else listID <- make.names(curfile) #each list element is named by the respective file

    rawtext <- readLines(curfile)
    outfiletext <- parse_into_sections(rawtext) #identify and cache top-level header section

    #Parse Mplus input into a list by section
    #if ("input" %in% what) {
      allFiles[[listID]]$input <- inp <- tryCatch(extractInput_1file(rawtext, curfile), error=function(e) {
            message("Error parsing input section of output file: ", curfile); print(e)
            return(list())
          })
    #}
    ## cleanup escaped quotes around data file name
    ## if it happened to be quoted in Mplus
    allFiles[[listID]]$input$data$file <- gsub("\\\"", "",
     allFiles[[listID]]$input$data$file)


    if ("warn_err" %in% what) {
      #Parse warnings and errors in output file
      warn_err <- tryCatch(extractWarningsErrors_1file(outfiletext, curfile, input=inp), error=function(e) {
            message("Error parsing warnings and errors in output file: ", curfile); print(e)
            return(list())
          })

      allFiles[[listID]]$warnings <- warn_err$warnings
      allFiles[[listID]]$errors <- warn_err$errors
    }

    if ("sampstat" %in% what) {
      #SAMPSTAT output
      allFiles[[listID]]$sampstat <- tryCatch(extractSampstat(outfiletext, curfile), error=function(e) {
            message("Error extracting SAMPSTAT in output file: ", curfile); print(e)
            return(list())
          })
    }

    if ("covariance_coverage" %in% what) {
      #COVARIANCE COVERAGE OF DATA output
      allFiles[[listID]]$covariance_coverage <- tryCatch(extractCovarianceCoverage(outfiletext, curfile), error=function(e) {
            message("Error extracting COVARIANCE COVERAGE OF DATA in output file: ", curfile); print(e)
            return(list())
          })
    }

    if ("summaries" %in% what) {
      #Model summary output (including MODEL FIT INFORMATION)
      allFiles[[listID]]$summaries <- tryCatch(extractSummaries_1file(outfiletext, curfile, input=inp), error=function(e) {
            message("Error extracting model summaries in output file: ", curfile); print(e)
            return(list())
          })
    }

    if ("parameters" %in% what) {
      #Model parameters (MODEL RESULTS section)
      allFiles[[listID]]$parameters <- tryCatch(extractParameters_1file(outfiletext, curfile), error=function(e) {
            message("Error extracting MODEL RESULTS in output file: ", curfile); print(e)
            return(list())
          })
    }

    if ("class_counts" %in% what) {
      #Latent class counts
      allFiles[[listID]]$class_counts <- tryCatch(extractClassCounts(outfiletext, curfile, allFiles[[listID]]$summaries), error=function(e) {
            message("Error extracting latent class counts in output file: ", curfile); print(e)
            return(list())
          })
    }

    if ("indirect" %in% what) {
      #MODEL INDIRECT
      allFiles[[listID]]$indirect <- tryCatch(extractIndirect(outfiletext, curfile), error=function(e) {
            message("Error extracting MODEL INDIRECT in output file: ", curfile); print(e)
            return(list())
          })
    }

    if ("mod_indices" %in% what) {
      #MODINDICES
      allFiles[[listID]]$mod_indices <- tryCatch(extractModIndices_1file(outfiletext, curfile), error=function(e) {
            message("Error extracting MODINDICES in output file: ", curfile); print(e)
            return(list())
          })
    }

    if ("residuals" %in% what) {
      #Extract model residuals (RESIDUAL)
      allFiles[[listID]]$residuals <- tryCatch(extractResiduals(outfiletext, curfile), error=function(e) {
            message("Error extracting RESIDUAL section in output file: ", curfile); print(e)
            return(list())
          })
    }

    if ("savedata" %in% what) {
      #SAVEDATA file information
      allFiles[[listID]]$savedata_info <- fileInfo <- tryCatch(l_getSavedata_Fileinfo(curfile, outfiletext, allFiles[[listID]]$summaries), error=function(e) {
            message("Error extracting SAVEDATA file information in output file: ", curfile); print(e)
            return(list())
          })

      #missing widths indicative of MI/MC run
      if (!is.null(fileInfo) && is.na(fileInfo[["fileVarWidths"]])) {
        allFiles[[listID]]$savedata <- tryCatch(l_getSavedata_readRawFile(curfile, outfiletext, format="free", fileName=fileInfo[["fileName"]], varNames=fileInfo[["fileVarNames"]], input=inp),
            error=function(e) {
              message("Error reading SAVEDATA rawfile: ", fileInfo[["fileName"]] , " in output file: ", curfile); print(e)
              return(list())
            })
      } else {
        allFiles[[listID]]$savedata <- tryCatch(l_getSavedata_readRawFile(curfile, outfiletext, format="fixed", fileName=fileInfo[["fileName"]], varNames=fileInfo[["fileVarNames"]], varWidths=fileInfo[["fileVarWidths"]], input=inp),
            error=function(e) {
              message("Error reading SAVEDATA rawfile: ", fileInfo[["fileName"]] , " in output file: ", curfile); print(e)
              return(list())
            })
      }
    }

    if ("bparameters" %in% what) {
      #Read BPARAMETERS (posterior draws) file from disk
      allFiles[[listID]]$bparameters <- tryCatch(l_getSavedata_Bparams(curfile, outfiletext, fileInfo, discardBurnin=FALSE), error=function(e) {
            message("Error reading BPARAMETERS file: ", fileInfo[["bayesFile"]], " in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECHNICAL OUTPUT
    if ("tech1" %in% what) {
      #TECH1: parameter specification and starting values
      allFiles[[listID]]$tech1 <- tryCatch(extractTech1(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH1 in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECH3: covariance/correlation matrix of parameter estimates
    if ("tech3" %in% what) {
      allFiles[[listID]]$tech3 <- tryCatch(extractTech3(outfiletext, fileInfo, curfile), error=function(e) {
            message("Error extracting TECH3 in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECH4: latent means
    if ("tech4" %in% what) {
      allFiles[[listID]]$tech4 <- tryCatch(extractTech4(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH4 in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECH7: sample stats for each class
    if ("tech7" %in% what) {
      allFiles[[listID]]$tech7 <- tryCatch(extractTech7(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH7 in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECH8: optimization history and chain tests in Bayes
    if ("tech8" %in% what) {
      allFiles[[listID]]$tech8 <- tryCatch(extractTech8(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH8 in output file: ", curfile); print(e)
            lempty <- list(); class(lempty) <- c("list", "mplus.tech8")
            return(lempty)
          })
    }

    #TECH9: errors and warnings for Monte Carlo output
    if ("tech9" %in% what) {
      allFiles[[listID]]$tech9 <- tryCatch(extractTech9(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH9 in output file: ", curfile); print(e)
            return(list())
          })
    }

    #TECH12: observed versus estimated sample stats for TYPE=MIXTURE
    if ("tech12" %in% what) {
      allFiles[[listID]]$tech12 <- tryCatch(extractTech12(outfiletext, curfile), error=function(e) {
            message("Error extracting TECH12 in output file: ", curfile); print(e)
            return(list())
          })
    }

    if ("fac_score_stats" %in% what) {
      allFiles[[listID]]$fac_score_stats <- extractFacScoreStats(outfiletext, curfile) #factor scores mean, cov, corr assoc with PLOT3
    }

    if ("lcCondMeans" %in% what) {
      #aux(e) means and pairwise comparisons
      allFiles[[listID]]$lcCondMeans <- extractAux(outfiletext, curfile)
    }

    #add class tag for use with compareModels
    class(allFiles[[listID]]) <- c("mplus.model", "list")
    attr(allFiles[[listID]], "filename") <- curfile

    #cleanup summary columns containing only NAs
    for (col in names(allFiles[[listID]]$summaries)) {
      if (all(is.na(allFiles[[listID]]$summaries[[col]]))) allFiles[[listID]]$summaries[[col]] <- NULL
    }

    if ("gh5" %in% what) {
      #check for gh5 file, and load if possible
      gh5 <- list()
      gh5fname <- sub("^(.*)\\.out$", "\\1.gh5", curfile, ignore.case=TRUE, perl=TRUE)
      if (file.exists(gh5fname)) {
        if (requireNamespace("rhdf5", quietly = TRUE)) {
          gh5 <- rhdf5::h5dump(file=gh5fname, recursive=TRUE, load=TRUE)
        } else { warning(paste(c("Unable to read gh5 file because rhdf5 package not installed.\n",
                      "To install, in an R session, type:\n",
                      "  source(\"http://bioconductor.org/biocLite.R\")\n",
                      "  biocLite(\"rhdf5\")\n")))
        }
      }
      allFiles[[listID]]$gh5 <- gh5
    }

  }

  if (length(outfiles)==1) {
    allFiles <- allFiles[[1]] #no need for single top-level element when there is only one file
  } else {
    class(allFiles) <- c("mplus.model.list", "list")
  }

  return(allFiles)
}
