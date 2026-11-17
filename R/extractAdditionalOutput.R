# Keep categorical tables consistent with the existing probability.scale table.
format_probability_parameters <- function(parameters) {
  parameters$paramHeader <- NULL
  parameters$category <- sub("^.*\\.Cat\\.(\\d+)$", "\\1", parameters$param, perl = TRUE)
  parameters$param <- sub("^(.*)\\.Cat\\.\\d+$", "\\1", parameters$param, perl = TRUE)
  columns <- c("param", "category", "est")
  parameters[, c(columns, setdiff(names(parameters), columns)), drop = FALSE]
}

extractProbabilityIntervals <- function(outfiletext, filename) {
  section <- getSection("^(CONFIDENCE|CREDIBILITY) INTERVALS IN PROBABILITY SCALE$", outfiletext)
  if (is.null(section)) return(NULL)
  # Mplus omits the repeated interval column header in this section.
  model_ci <- getSection("^(CONFIDENCE|CREDIBILITY) INTERVALS OF MODEL RESULTS$", outfiletext)
  if (is.null(model_ci)) return(NULL)
  columns <- detectColumnNames(filename, model_ci, "confidence_intervals")
  if (is.null(columns)) return(NULL)
  section <- c(model_ci[attr(columns, "header_lines")], section)
  parameters <- extractParameters_1section(filename, section, "ci.probability.scale")$ci.probability.scale
  if (is.null(parameters) || nrow(parameters) == 0L) return(NULL)
  out <- format_probability_parameters(parameters)
  attr(out, "filename") <- filename
  out
}

extractIndicatorOdds <- function(outfiletext, filename, intervals = FALSE) {
  header <- "LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES"
  if (intervals) header <- paste("CONFIDENCE INTERVALS FOR THE", header)
  section <- getSection(paste0("^", header, "$"), outfiletext)
  if (is.null(section)) return(NULL)
  if (intervals) {
    model_ci <- getSection("^CONFIDENCE INTERVALS OF MODEL RESULTS$", outfiletext)
    if (is.null(model_ci)) return(NULL)
    columns <- detectColumnNames(filename, model_ci, "confidence_intervals")
    column_header <- model_ci[attr(columns, "header_lines")]
    section_name <- "ci.probability.scale"
  } else {
    columns <- detectColumnNames(filename, section, "model_results")
    column_header <- section[attr(columns, "header_lines")]
    section_name <- "probability.scale"
  }
  if (is.null(columns)) return(NULL)
  comparison_pattern <- "^\\s*Latent Class (.+?) Compared to Latent Class (.+?)\\s*$"
  comparisons <- getMultilineSection(comparison_pattern, section, filename, allowMultiple = TRUE)
  if (section_is_missing(comparisons)) return(NULL)
  labels <- extract_pcre_groups(section[section_matchlines(comparisons)], comparison_pattern)
  tables <- lapply(seq_along(comparisons), function(i) {
    lines <- comparisons[[i]]
    category_lines <- grep("^\\s*Category\\s+[>=]\\s+\\d+", lines, perl = TRUE)
    operators <- sub("^\\s*Category\\s+([>=]).*$", "\\1", lines[category_lines], perl = TRUE)
    lines <- sub("Category\\s+[>=]\\s+", "Category ", lines, perl = TRUE)
    table <- extractParameters_1section(filename, c(column_header, lines), section_name)[[section_name]]
    if (is.null(table) || nrow(table) == 0L) return(NULL)
    table <- format_probability_parameters(table)
    if (nrow(table) != length(operators)) stop("Incomplete indicator odds table in ", filename)
    table$category <- paste(operators, table$category)
    table$classA <- labels[[i]][2L]
    table$classB <- labels[[i]][3L]
    table
  })
  tables <- Filter(Negate(is.null), tables)
  if (!length(tables)) return(NULL)
  out <- do.call(rbind, tables)
  rownames(out) <- NULL
  attr(out, "filename") <- filename
  out
}

extractIndicatorProfiles <- function(outfiletext, filename) {
  section <- getSection("^LATENT CLASS INDICATOR MEANS AND PROBABILITIES FOR EACH LATENT CLASS$", outfiletext)
  if (is.null(section)) return(NULL)
  rows <- list()
  classes <- character()
  class_variable <- param <- NA_character_
  for (line in section) {
    if (grepl("^\\s*MEAN/PROBABILITY PROFILES FOR ", line)) {
      class_variable <- sub("^\\s*MEAN/PROBABILITY PROFILES FOR\\s+", "", trimws(line))
      classes <- character()
      next
    }
    fields <- strsplit(trimws(line), "\\s+")[[1L]]
    if (!length(fields) || !nzchar(fields[1L])) next
    if (all(grepl("^\\d+$", fields))) {
      classes <- fields
      next
    }
    if (!length(classes)) next
    if (length(fields) == 1L) {
      param <- fields[1L]
      next
    }
    category <- NA_character_
    if (fields[1L] == "Category") {
      category <- fields[2L]
      values <- fields[-c(1L, 2L)]
    } else {
      param <- fields[1L]
      values <- fields[-1L]
    }
    if (length(values) != length(classes) || is.na(param)) next
    estimates <- suppressWarnings(mplus_as.numeric(values))
    if (any(is.na(estimates) & !grepl("^\\*{2,}$", values))) next
    rows[[length(rows) + 1L]] <- data.frame(
      param = param, category = category, est = estimates,
      LatentClass = classes, class_variable = class_variable, stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  class(out) <- c("mplus.params", "data.frame")
  attr(out, "filename") <- filename
  evaluated_at <- grep("^\\s*EVALUATED AT ", section, value = TRUE)
  if (length(evaluated_at)) attr(out, "evaluated_at") <- trimws(evaluated_at[1L])
  out
}

extractBrantTests <- function(outfiletext, filename) {
  section <- getSection("^BRANT WALD TEST FOR PROPORTIONAL ODDS$", outfiletext)
  if (is.null(section)) return(NULL)
  rows <- list()
  param <- NA_character_
  for (line in section) {
    fields <- strsplit(trimws(line), "\\s+")[[1L]]
    if (length(fields) == 1L && nzchar(fields[1L])) param <- fields[1L]
    if (length(fields) < 4L || is.na(param)) next
    raw_values <- tail(fields, 3L)
    values <- suppressWarnings(mplus_as.numeric(raw_values))
    if (any(is.na(values) & !grepl("^\\*{2,}$", raw_values))) next
    rows[[length(rows) + 1L]] <- data.frame(
      param = param, test = paste(head(fields, -3L), collapse = " "),
      chisq = values[1L], df = values[2L], pval = values[3L], stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  class(out) <- c("mplus.params", "data.frame")
  attr(out, "filename") <- filename
  out
}

extractLogrankTests <- function(outfiletext, filename) {
  section <- getSection("^LOGRANK OUTPUT$", outfiletext)
  if (is.null(section)) return(NULL)
  pattern <- "^\\s*LOGRANK TEST FOR SURVIVAL VARIABLE (\\S+) COMPARING CLASS (\\S+) AGAINST CLASS (\\S+)\\s*$"
  tests <- getMultilineSection(pattern, section, filename, allowMultiple = TRUE)
  if (section_is_missing(tests)) return(NULL)
  labels <- extract_pcre_groups(section[section_matchlines(tests)], pattern)
  out <- do.call(rbind, lapply(seq_along(tests), function(i) {
    data.frame(
      param = labels[[i]][2L], classA = labels[[i]][3L], classB = labels[[i]][4L],
      chisq = extractValue("^\\s*Chi-Square Value\\s*", tests[[i]], filename, type = "dec"),
      df = extractValue("^\\s*Degrees of Freedom\\s*", tests[[i]], filename),
      pval = extractValue("^\\s*P-value\\s*", tests[[i]], filename, type = "dec"),
      stringsAsFactors = FALSE
    )
  }))
  class(out) <- c("mplus.params", "data.frame")
  attr(out, "filename") <- filename
  out
}

extractAdditionalParameters <- function(outfiletext, filename) {
  tables <- list(
    ci.probability.scale = extractProbabilityIntervals(outfiletext, filename),
    indicator.profiles = extractIndicatorProfiles(outfiletext, filename),
    indicator.odds = extractIndicatorOdds(outfiletext, filename),
    ci.indicator.odds = extractIndicatorOdds(outfiletext, filename, intervals = TRUE),
    brant = extractBrantTests(outfiletext, filename),
    logrank = extractLogrankTests(outfiletext, filename)
  )
  Filter(Negate(is.null), tables)
}

# These short variable tables also appear without a SAMPLE STATISTICS section.
extractVariableStatistics <- function(section, columns) {
  rows <- list()
  group <- NA_character_
  for (line in section) {
    if (grepl("^\\s*Group ", line)) {
      group <- sub("^\\s*Group (\\w+).*$", "\\1", line)
      next
    }
    fields <- strsplit(trimws(line), "\\s+")[[1L]]
    if (length(fields) != length(columns)) next
    values <- suppressWarnings(mplus_as.numeric(fields[-1L]))
    if (any(is.na(values) & !grepl("^\\*{2,}$", fields[-1L]))) next
    row <- data.frame(variable = fields[1L], as.list(values), stringsAsFactors = FALSE)
    names(row) <- columns
    row$Group <- group
    rows[[length(rows) + 1L]] <- row
  }
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  if (all(is.na(out$Group))) out$Group <- NULL
  out
}
