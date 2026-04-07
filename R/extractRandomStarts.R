#' Extract random starts details from Mplus output
#'
#' Internal helper used by `readModels()` to parse the optional
#' random starts specification block and the ranked final-stage
#' loglikelihood table.
#'
#' @param outfiletext Parsed Mplus output text.
#' @param filename Name of the output file, used in warnings/errors.
#' @return A list containing random starts specifications and the
#'   final-stage results table, or `NULL` if neither section is present.
#' @keywords internal
extractRandomStarts <- function(outfiletext, filename) {
  randomStartsSpecs <- getMultilineSection(
    "Random Starts Specifications", outfiletext, filename
  )
  randomStartsResults <- getSection(
    "^RANDOM STARTS RESULTS RANKED FROM THE BEST TO THE WORST (FIT FUNCTION|LOGLIKELIHOOD) VALUES$",
    outfiletext
  )

  if (is.null(randomStartsSpecs) && is.null(randomStartsResults)) {
    return(NULL)
  }

  out <- list(
    final_stage = data.frame(
      log_likelihood = numeric(0L),
      seed = integer(0L),
      initial_stage_start_number = integer(0L),
      stringsAsFactors = FALSE
    ),
    initial_stage_random_starts = NA_integer_,
    final_stage_optimizations = NA_integer_,
    initial_stage_iterations = NA_integer_,
    initial_stage_convergence_criterion = NA_real_,
    random_starts_scale = NA_real_,
    random_seed = NA_integer_
  )

  if (!is.null(randomStartsSpecs)) {
    specFields <- list(
      initial_stage_random_starts = list(
        label = "Number of initial stage random starts",
        type = "integer"
      ),
      final_stage_optimizations = list(
        label = "Number of final stage optimizations",
        type = "integer"
      ),
      initial_stage_iterations = list(
        label = "Number of initial stage iterations",
        type = "integer"
      ),
      initial_stage_convergence_criterion = list(
        label = "Initial stage convergence criterion",
        type = "numeric"
      ),
      random_starts_scale = list(
        label = "Random starts scale",
        type = "numeric"
      ),
      random_seed = list(
        label = "Random seed for generating random starts",
        type = "integer"
      )
    )

    for (fieldName in names(specFields)) {
      out[[fieldName]] <- extractRandomStartsValue(
        randomStartsSpecs,
        label = specFields[[fieldName]]$label,
        type = specFields[[fieldName]]$type,
        filename = filename
      )
    }
  }

  if (!is.null(randomStartsResults)) {
    finalStageLines <- getMultilineSection(
      "{+2b}Final stage (loglikelihood|fit function) values at local maxima, seeds, and initial stage start numbers:",
      randomStartsResults,
      filename,
      ignore.case = TRUE
    )

    if (!is.null(finalStageLines) && length(finalStageLines) > 0L) {
      rowFields <- strsplit(trimSpace(finalStageLines), "\\s+", perl = TRUE)
      badRows <- which(lengths(rowFields) != 3L)

      if (length(badRows) > 0L) {
        warning(
          "Unable to parse one or more random starts result rows in: ",
          filename
        )
        rowFields <- rowFields[lengths(rowFields) == 3L]
      }

      if (length(rowFields) > 0L) {
        out$final_stage <- data.frame(
          log_likelihood = mplus_as.numeric(vapply(rowFields, `[`, "", 1L)),
          seed = as.integer(mplus_as.numeric(vapply(rowFields, `[`, "", 2L))),
          initial_stage_start_number = as.integer(mplus_as.numeric(vapply(rowFields, `[`, "", 3L))),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  class(out) <- c("mplus.random_starts", "list")
  out
}

extractRandomStartsValue <- function(section, label, type = c("integer", "numeric"), filename) {
  type <- match.arg(type)
  matchLine <- grep(
    paste0("^\\s*", label, "\\s+"),
    section,
    perl = TRUE,
    ignore.case = TRUE
  )

  if (length(matchLine) == 0L) {
    return(if (identical(type, "integer")) NA_integer_ else NA_real_)
  }
  if (length(matchLine) > 1L) {
    stop("More than one match found for random starts field: ", label, "\n  ", filename)
  }

  valueText <- sub(
    paste0("^\\s*", label, "\\s+"),
    "",
    section[matchLine[1L]],
    perl = TRUE,
    ignore.case = TRUE
  )
  value <- mplus_as.numeric(trimSpace(valueText))

  if (identical(type, "integer")) as.integer(value) else value
}
