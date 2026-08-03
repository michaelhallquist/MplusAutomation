# Convert supported H5RESULTS parameter tables to the established mplus.params
# representation. More complex result layouts (for example, EFA and multiple
# MODEL RESULTS sections) intentionally return NULL so that readModels uses its
# established text parser.
extractH5ResultTable <- function(result_section, filename,
                                 result_headings = NULL,
                                 statement_headings = NULL) {
  if (is.null(result_section)) return(NULL)

  results <- result_section$Results
  statements <- result_section$Statements
  if (is.null(results) || is.null(statements) ||
      !is.matrix(results) || !is.matrix(statements) ||
      nrow(results) == 0L || nrow(results) != nrow(statements)) {
    return(NULL)
  }

  default_result_headings <- c("Estimate", "S.E.", "Estimate/S.E.", "Two-Tailed P-Value")
  default_statement_headings <- c(
    "Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable"
  )
  if (is.null(result_headings) && ncol(results) == length(default_result_headings)) {
    result_headings <- default_result_headings
  }
  if (is.null(statement_headings) && ncol(statements) == length(default_statement_headings)) {
    statement_headings <- default_statement_headings
  }
  if (is.null(result_headings) || is.null(statement_headings)) return(NULL)

  result_headings <- trimws(as.character(result_headings))
  statement_headings <- trimws(as.character(statement_headings))
  needed_statements <- c("Section", "Keyword", "Right-hand variable")
  if (!all(needed_statements %in% statement_headings) ||
      !any(grepl("^(Estimate|Posterior Estimate)$", result_headings, ignore.case = TRUE))) {
    return(NULL)
  }

  lhs_heading <- intersect(
    c("Left-hand variable/Variable/Parameter label", "Left-hand variable", "Variable", "Parameter label"),
    statement_headings
  )
  if (length(lhs_heading) != 1L) return(NULL)

  statement_values <- matrix(
    trimws(as.character(statements)), nrow = nrow(statements), ncol = ncol(statements)
  )
  colnames(statement_values) <- statement_headings
  section <- statement_values[, "Section"]
  lhs <- statement_values[, lhs_heading]
  keyword <- statement_values[, "Keyword"]
  rhs <- statement_values[, "Right-hand variable"]
  item_discrimination <- section == "Item Discriminations" &
    nzchar(keyword) & nzchar(rhs)

  param_header <- ifelse(
    item_discrimination,
    paste(lhs, keyword, sep = "."),
    ifelse(
      nzchar(section),
      make.names(section),
      ifelse(nzchar(keyword), paste(lhs, keyword, sep = "."), lhs)
    )
  )
  param <- ifelse(
    item_discrimination,
    rhs,
    ifelse(
      nzchar(section),
      paste0(lhs, keyword),
      ifelse(nzchar(rhs), rhs, lhs)
    )
  )

  result_values <- matrix(
    as.numeric(results), nrow = nrow(results), ncol = ncol(results)
  )
  colnames(result_values) <- result_headings
  value_column <- function(pattern) {
    match <- grep(pattern, result_headings, ignore.case = TRUE, perl = TRUE)
    if (length(match) == 1L) result_values[, match] else rep(NA_real_, nrow(result_values))
  }
  fixed_variance <- section %in% c("Variances", "Residual Variances") &
    !nzchar(keyword) & !nzchar(rhs) & value_column("^S\\.E\\.$") == 0
  param[fixed_variance] <- "FALSE"

  if ("Posterior S.D." %in% result_headings) {
    params <- data.frame(
      paramHeader = as.character(param_header),
      param = as.character(param),
      est = value_column("^(Estimate|Posterior Estimate)$"),
      posterior_sd = value_column("^Posterior S\\.D\\.$"),
      pval = value_column("P-Value$"),
      lower_2.5ci = value_column("^Lower 2\\.5% C\\.I\\.$"),
      upper_2.5ci = value_column("^Upper 2\\.5% C\\.I\\.$"),
      sig = as.logical(value_column("^Significance$")),
      stringsAsFactors = FALSE
    )
  } else {
    params <- data.frame(
      paramHeader = as.character(param_header),
      param = as.character(param),
      est = value_column("^(Estimate|Posterior Estimate)$"),
      se = value_column("^(S\\.E\\.|Posterior S\\.D\\.)$"),
      est_se = value_column("^(Estimate|Est\\.)/S\\.E\\.$"),
      pval = value_column("P-Value$"),
      stringsAsFactors = FALSE
    )
  }
  class(params) <- c("mplus.params", "data.frame")
  attr(params, "filename") <- filename
  params
}

extractH5ModelResults <- function(h5results, filename,
                                  result_headings = NULL,
                                  statement_headings = NULL) {
  params <- extractH5ResultTable(
    h5results[["Model Results"]],
    filename = filename,
    result_headings = result_headings,
    statement_headings = statement_headings
  )
  if (is.null(params)) return(NULL)
  list(unstandardized = params)
}

read_h5_attributes <- function(file, dataset) {
  if (!isTRUE(requireNamespace("rhdf5", quietly = TRUE))) return(NULL)
  tryCatch(rhdf5::h5readAttributes(file, dataset), error = function(e) NULL)
}

extractH5ModelResults_1file <- function(h5results, h5file, filename) {
  result_attributes <- read_h5_attributes(h5file, "Model Results/Results")
  statement_attributes <- read_h5_attributes(h5file, "Model Results/Statements")
  extractH5ModelResults(
    h5results,
    filename = filename,
    result_headings = result_attributes$Headings,
    statement_headings = statement_attributes$Headings
  )
}

h5_result_headings <- function(h5file, section) {
  attributes <- read_h5_attributes(h5file, paste(section, "Results", sep = "/"))
  attributes$Headings
}

h5_statement_headings <- function(h5file, section) {
  attributes <- read_h5_attributes(h5file, paste(section, "Statements", sep = "/"))
  attributes$Headings
}

new_h5_params <- function(param_header, param, values, filename) {
  out <- data.frame(
    paramHeader = as.character(param_header),
    param = as.character(param),
    values,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  class(out) <- c("mplus.params", "data.frame")
  attr(out, "filename") <- filename
  out
}

extractH5ConfidenceIntervalTable <- function(result_section, filename,
                                              result_headings = NULL,
                                              statement_headings = NULL) {
  if (!is.null(result_section) && is.null(result_headings) &&
      is.matrix(result_section$Results) && ncol(result_section$Results) == 7L) {
    result_headings <- c(
      "Lower .5%", "Lower 2.5%", "Lower 5%", "Estimate",
      "Upper 5%", "Upper 2.5%", "Upper .5%"
    )
  }
  base <- extractH5ResultTable(
    result_section, filename, result_headings, statement_headings
  )
  if (is.null(base)) return(NULL)

  result_headings <- trimws(as.character(result_headings))
  result_values <- matrix(
    as.numeric(result_section$Results),
    nrow = nrow(result_section$Results), ncol = ncol(result_section$Results)
  )
  column_names <- c(
    "Lower .5%" = "low.5",
    "Lower 2.5%" = "low2.5",
    "Lower 5%" = "low5",
    "Estimate" = "est",
    "Upper 5%" = "up5",
    "Upper 2.5%" = "up2.5",
    "Upper .5%" = "up.5"
  )
  if (!all(names(column_names) %in% result_headings)) return(NULL)

  values <- as.data.frame(result_values[, match(names(column_names), result_headings), drop = FALSE])
  names(values) <- unname(column_names)
  new_h5_params(base$paramHeader, base$param, values, filename)
}

extractH5OddsRatioTable <- function(result_section, filename,
                                    result_headings = NULL,
                                    statement_headings = NULL) {
  if (is.null(result_section) || is.null(result_section$Results) ||
      is.null(result_section$Statements) || !is.matrix(result_section$Results) ||
      !is.matrix(result_section$Statements) ||
      nrow(result_section$Results) != nrow(result_section$Statements)) {
    return(NULL)
  }

  if (is.null(result_headings) && ncol(result_section$Results) == 4L) {
    result_headings <- c("Estimate", "S.E.", "Lower 2.5%", "Upper 2.5%")
  }
  if (is.null(statement_headings) && ncol(result_section$Statements) == 4L) {
    statement_headings <- c(
      "Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable"
    )
  }

  result_headings <- trimws(as.character(result_headings))
  statement_headings <- trimws(as.character(statement_headings))
  lhs_heading <- intersect(
    c("Left-hand variable/Variable/Parameter label", "Left-hand variable", "Variable", "Parameter label"),
    statement_headings
  )
  required_results <- c("Estimate", "S.E.", "Lower 2.5%", "Upper 2.5%")
  if (length(lhs_heading) != 1L || !all(required_results %in% result_headings) ||
      !all(c("Keyword", "Right-hand variable") %in% statement_headings)) {
    return(NULL)
  }

  statements <- matrix(
    trimws(as.character(result_section$Statements)),
    nrow = nrow(result_section$Statements), ncol = ncol(result_section$Statements)
  )
  colnames(statements) <- statement_headings
  predictors <- ifelse(
    nzchar(statements[, "Right-hand variable"]),
    statements[, "Right-hand variable"],
    statements[, "Keyword"]
  )
  if (any(!nzchar(statements[, lhs_heading])) || any(!nzchar(predictors))) return(NULL)

  results <- matrix(
    as.numeric(result_section$Results),
    nrow = nrow(result_section$Results), ncol = ncol(result_section$Results)
  )
  values <- as.data.frame(results[, match(required_results, result_headings), drop = FALSE])
  names(values) <- c("est", "se", "lower_2.5ci", "upper_2.5ci")
  new_h5_params(
    paste(statements[, lhs_heading], "ON", sep = "."), predictors, values, filename
  )
}

interval_result_section <- function(interval_type = c("Confidence", "Credibility"),
                                    standardized = FALSE) {
  interval_type <- match.arg(interval_type)
  paste(
    interval_type, "Intervals of",
    if (isTRUE(standardized)) "Standardized Model Results" else "Model Results"
  )
}

extractH5ConfidenceIntervals <- function(h5results, filename,
                                         result_headings = NULL,
                                         statement_headings = NULL,
                                         interval_type = c("Confidence", "Credibility")) {
  section <- interval_result_section(interval_type)
  params <- extractH5ConfidenceIntervalTable(
    h5results[[section]], filename,
    result_headings, statement_headings
  )
  if (is.null(params)) return(NULL)
  list("ci.unstandardized" = params)
}

extractH5ConfidenceIntervals_1file <- function(h5results, h5file, filename,
                                               interval_type = c("Confidence", "Credibility")) {
  section <- interval_result_section(interval_type)
  extractH5ConfidenceIntervals(
    h5results, filename,
    h5_result_headings(h5file, section), h5_statement_headings(h5file, section),
    interval_type = interval_type
  )
}

extractH5OddsRatioResults <- function(h5results, filename,
                                      result_headings = NULL,
                                      statement_headings = NULL,
                                      confidence_intervals = NULL) {
  odds <- extractH5OddsRatioTable(
    h5results[["Logistic Regression Odds Ratio Results"]], filename,
    result_headings, statement_headings
  )
  if (is.null(odds)) return(NULL)

  out <- list(odds = odds)
  if (!is.null(confidence_intervals)) {
    ci_keys <- paste(confidence_intervals$paramHeader, confidence_intervals$param, sep = "\r")
    odds_keys <- paste(odds$paramHeader, odds$param, sep = "\r")
    ci_rows <- match(odds_keys, ci_keys)
    if (!anyNA(ci_rows)) {
      ci_values <- exp(confidence_intervals[ci_rows, c("low.5", "low2.5", "low5", "est", "up5", "up2.5", "up.5")])
      out[["ci.odds"]] <- new_h5_params(
        odds$paramHeader, odds$param, ci_values, filename
      )
    }
  }
  out
}

extractH5OddsRatioResults_1file <- function(h5results, h5file, filename,
                                            confidence_intervals = NULL) {
  section <- "Logistic Regression Odds Ratio Results"
  extractH5OddsRatioResults(
    h5results, filename,
    h5_result_headings(h5file, section), h5_statement_headings(h5file, section),
    confidence_intervals = confidence_intervals
  )
}

extractH5ProbabilityScaleTable <- function(result_section, filename,
                                           result_headings = NULL,
                                           statement_headings = NULL) {
  if (is.null(result_section) || is.null(result_section$Results) ||
      is.null(result_section$Statements) || !is.matrix(result_section$Results) ||
      !is.matrix(result_section$Statements) ||
      nrow(result_section$Results) != nrow(result_section$Statements) ||
      is.null(result_headings) || is.null(statement_headings)) {
    return(NULL)
  }

  result_headings <- trimws(as.character(result_headings))
  statement_headings <- trimws(as.character(statement_headings))
  lhs_heading <- intersect(
    c("Left-hand variable/Variable/Parameter label", "Left-hand variable", "Variable", "Parameter label"),
    statement_headings
  )
  if (length(lhs_heading) != 1L || !"Section" %in% statement_headings ||
      !"Estimate" %in% result_headings) {
    return(NULL)
  }

  statements <- matrix(
    trimws(as.character(result_section$Statements)),
    nrow = nrow(result_section$Statements), ncol = ncol(result_section$Statements)
  )
  colnames(statements) <- statement_headings
  category <- sub("^Category\\s+", "", statements[, lhs_heading], ignore.case = TRUE)
  if (any(!nzchar(statements[, "Section"])) || any(!nzchar(category)) ||
      any(category == statements[, lhs_heading])) {
    return(NULL)
  }

  results <- matrix(
    as.numeric(result_section$Results),
    nrow = nrow(result_section$Results), ncol = ncol(result_section$Results)
  )
  value_names <- c(
    "Estimate" = "est",
    "S.E." = "se",
    "Estimate/S.E." = "est_se",
    "Two-Tailed P-Value" = "pval"
  )
  available <- names(value_names)[names(value_names) %in% result_headings]
  values <- as.data.frame(results[, match(available, result_headings), drop = FALSE])
  names(values) <- unname(value_names[available])
  params <- data.frame(
    param = statements[, "Section"],
    category = category,
    values,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if ("Class" %in% statement_headings) {
    params$LatentClass <- as.numeric(sub("^.*?(\\d+)\\s*$", "\\1", statements[, "Class"]))
  }
  class(params) <- c("mplus.params", "data.frame")
  attr(params, "filename") <- filename
  params
}

extractH5ProbabilityScaleResults <- function(h5results, filename,
                                             result_headings = NULL,
                                             statement_headings = NULL) {
  params <- extractH5ProbabilityScaleTable(
    h5results[["Results in Probability Scale"]], filename,
    result_headings, statement_headings
  )
  if (is.null(params)) return(NULL)
  list("probability.scale" = params)
}

extractH5ProbabilityScaleResults_1file <- function(h5results, h5file, filename) {
  section <- "Results in Probability Scale"
  extractH5ProbabilityScaleResults(
    h5results, filename,
    h5_result_headings(h5file, section), h5_statement_headings(h5file, section)
  )
}

extractH5IRTParameterization <- function(h5results, filename,
                                         result_headings = NULL,
                                         statement_headings = NULL) {
  params <- extractH5ResultTable(
    h5results[["IRT Parameterization"]], filename,
    result_headings, statement_headings
  )
  if (is.null(params)) return(NULL)

  statement_headings <- trimws(as.character(statement_headings))
  statements <- matrix(
    trimws(as.character(h5results[["IRT Parameterization"]]$Statements)),
    nrow = nrow(h5results[["IRT Parameterization"]]$Statements),
    ncol = ncol(h5results[["IRT Parameterization"]]$Statements)
  )
  colnames(statements) <- statement_headings
  lhs_heading <- intersect(
    c("Left-hand variable/Variable/Parameter label", "Left-hand variable", "Variable", "Parameter label"),
    statement_headings
  )
  if (length(lhs_heading) == 1L &&
      all(c("Section", "Keyword", "Right-hand variable") %in% statement_headings)) {
    discriminations <- statements[, "Section"] == "Item Discriminations" &
      nzchar(statements[, "Keyword"]) & nzchar(statements[, "Right-hand variable"])
    params$paramHeader[discriminations] <- paste(
      statements[discriminations, lhs_heading], statements[discriminations, "Keyword"], sep = "."
    )
    params$param[discriminations] <- statements[discriminations, "Right-hand variable"]
  }
  list("irt.parameterization" = params)
}

extractH5IRTParameterization_1file <- function(h5results, h5file, filename) {
  section <- "IRT Parameterization"
  extractH5IRTParameterization(
    h5results, filename,
    h5_result_headings(h5file, section), h5_statement_headings(h5file, section)
  )
}

extractH5StandardizedConfidenceIntervals <- function(h5results, filename,
                                                     interval_type = c("Confidence", "Credibility")) {
  section <- interval_result_section(interval_type, standardized = TRUE)
  standard_results <- h5results[[section]]
  if (is.null(standard_results)) return(NULL)

  standard_groups <- c(
    "ci.stdyx.standardized" = "STDYX Standardization",
    "ci.stdy.standardized" = "STDY Standardization",
    "ci.std.standardized" = "STD Standardization"
  )
  out <- list()
  for (section_name in names(standard_groups)) {
    params <- extractH5ConfidenceIntervalTable(
      standard_results[[standard_groups[[section_name]]]], filename
    )
    if (!is.null(params)) out[[section_name]] <- params
  }
  if (length(out) == 0L) NULL else out
}

extractH5StandardizedConfidenceIntervals_1file <- function(h5results, h5file, filename,
                                                           interval_type = c("Confidence", "Credibility")) {
  section <- interval_result_section(interval_type, standardized = TRUE)
  standard_results <- h5results[[section]]
  if (is.null(standard_results)) return(NULL)

  standard_groups <- c(
    "ci.stdyx.standardized" = "STDYX Standardization",
    "ci.stdy.standardized" = "STDY Standardization",
    "ci.std.standardized" = "STD Standardization"
  )
  out <- list()
  for (section_name in names(standard_groups)) {
    h5_section <- paste(section, standard_groups[[section_name]], sep = "/")
    params <- extractH5ConfidenceIntervalTable(
      standard_results[[standard_groups[[section_name]]]], filename,
      h5_result_headings(h5file, h5_section),
      h5_statement_headings(h5file, h5_section)
    )
    if (!is.null(params)) out[[section_name]] <- params
  }
  if (length(out) == 0L) NULL else out
}

extractH5StandardizedModelResults <- function(h5results, filename) {
  standard_results <- h5results[["Standardized Model Results"]]
  if (is.null(standard_results)) return(NULL)

  standard_groups <- c(
    "stdyx.standardized" = "STDYX Standardization",
    "stdy.standardized" = "STDY Standardization",
    "std.standardized" = "STD Standardization"
  )
  out <- list()
  for (section_name in names(standard_groups)) {
    params <- extractH5ResultTable(standard_results[[standard_groups[[section_name]]]], filename)
    if (!is.null(params)) out[[section_name]] <- params
  }

  r2_table <- extractH5ResultTable(h5results[["R-Square"]], filename)
  if (!is.null(r2_table)) {
    r2 <- r2_table[, c("param", "est", "se", "est_se", "pval"), drop = FALSE]
    class(r2) <- c("mplus.params", "data.frame")
    attr(r2, "filename") <- filename
    out$r2 <- r2
  }

  if (length(out) == 0L) NULL else out
}

extractH5StandardizedModelResults_1file <- function(h5results, h5file, filename) {
  standard_results <- h5results[["Standardized Model Results"]]
  if (is.null(standard_results)) return(NULL)

  standard_groups <- c(
    "stdyx.standardized" = "STDYX Standardization",
    "stdy.standardized" = "STDY Standardization",
    "std.standardized" = "STD Standardization"
  )
  out <- list()
  for (section_name in names(standard_groups)) {
    h5_section <- standard_groups[[section_name]]
    result_attributes <- read_h5_attributes(
      h5file, paste("Standardized Model Results", h5_section, "Results", sep = "/")
    )
    statement_attributes <- read_h5_attributes(
      h5file, paste("Standardized Model Results", h5_section, "Statements", sep = "/")
    )
    params <- extractH5ResultTable(
      standard_results[[h5_section]],
      filename = filename,
      result_headings = result_attributes$Headings,
      statement_headings = statement_attributes$Headings
    )
    if (!is.null(params)) out[[section_name]] <- params
  }

  r2_result_attributes <- read_h5_attributes(h5file, "R-Square/Results")
  r2_statement_attributes <- read_h5_attributes(h5file, "R-Square/Statements")
  r2_table <- extractH5ResultTable(
    h5results[["R-Square"]],
    filename = filename,
    result_headings = r2_result_attributes$Headings,
    statement_headings = r2_statement_attributes$Headings
  )
  if (!is.null(r2_table)) {
    r2 <- r2_table[, c("param", "est", "se", "est_se", "pval"), drop = FALSE]
    class(r2) <- c("mplus.params", "data.frame")
    attr(r2, "filename") <- filename
    out$r2 <- r2
  }

  if (length(out) == 0L) NULL else out
}

h5results_file_path <- function(savedata_info, outfile) {
  if (is.null(savedata_info) || is.null(savedata_info$h5resultsFile) ||
      length(savedata_info$h5resultsFile) == 0L || is.na(savedata_info$h5resultsFile[1L])) {
    return(NA_character_)
  }
  h5file <- savedata_info$h5resultsFile[1L]
  if (is.na(splitFilePath(h5file)$directory)) h5file <- file.path(dirname(outfile), h5file)
  h5file
}

# Decide whether H5RESULTS completely covers the parameter sections displayed
# in an output file. Returning NULL deliberately delegates to the established
# text parser, keeping readModels itself focused on file-level orchestration.
has_output_section <- function(outfiletext, pattern) {
  any(grepl(
    paste0("^\\s*", pattern, "\\s*$"), outfiletext,
    ignore.case = TRUE, perl = TRUE
  ))
}

has_output_prefix <- function(outfiletext, pattern) {
  any(grepl(paste0("^\\s*", pattern), outfiletext, ignore.case = TRUE, perl = TRUE))
}

expected_standardized_sections <- function(outfiletext, prefix = "") {
  standard_sections <- c(
    "stdyx.standardized" = "STDYX Standardization",
    "stdy.standardized" = "STDY Standardization",
    "std.standardized" = "STD Standardization"
  )
  if (nzchar(prefix)) names(standard_sections) <- paste0(prefix, names(standard_sections))
  names(standard_sections)[vapply(
    unname(standard_sections),
    function(section) has_output_section(outfiletext, section),
    logical(1L)
  )]
}

output_interval_type <- function(outfiletext, standardized = FALSE) {
  suffix <- if (isTRUE(standardized)) {
    "STANDARDIZED MODEL RESULTS"
  } else {
    "MODEL RESULTS"
  }
  interval_types <- c("Confidence", "Credibility")
  matches <- vapply(
    interval_types,
    function(type) has_output_section(outfiletext, paste(type, "INTERVALS OF", suffix)),
    logical(1L)
  )
  if (sum(matches) == 1L) interval_types[matches] else NA_character_
}

order_h5_parameters <- function(parameters) {
  section_order <- c(
    "unstandardized", "unstandardized.alt", "r2", "ci.unstandardized",
    "irt.parameterization", "probability.scale", "odds", "ci.odds",
    "stdyx.standardized", "ci.stdyx.standardized",
    "stdy.standardized", "ci.stdy.standardized",
    "std.standardized", "ci.std.standardized", "wilevel.standardized", "efa"
  )
  parameters[intersect(section_order, names(parameters))]
}

add_irt_metric_attribute <- function(parameters, outfiletext) {
  definition <- grep(
    "^\\s*where the (probit|logit) is\\s+", outfiletext,
    value = TRUE, ignore.case = TRUE, perl = TRUE
  )
  if (length(definition) == 1L) {
    metric <- tolower(sub(
      "^\\s*where the (probit|logit) is.*$", "\\1", definition,
      ignore.case = TRUE, perl = TRUE
    ))
    attr(parameters, metric) <- tolower(sub(
      "^\\s*where the (?:probit|logit) is\\s+(.*)$", "\\1", definition,
      ignore.case = TRUE, perl = TRUE
    ))
  }
  parameters
}

extractH5Parameters_1file <- function(outfiletext, filename, h5results, h5file,
                                      efa = FALSE) {
  has_unsupported_sections <- any(grepl(
    "^\\s*(ALTERNATIVE PARAMETERIZATIONS|WITHIN-LEVEL STANDARDIZED MODEL RESULTS)",
    outfiletext, perl = TRUE
  ))
  has_multiple_model_results <- any(grepl("^\\s*MODEL RESULTS FOR ", outfiletext, perl = TRUE))
  if (is.null(h5results) || isTRUE(efa) || has_multiple_model_results || has_unsupported_sections) {
    return(NULL)
  }

  parameters <- tryCatch(
    extractH5ModelResults_1file(h5results, h5file, filename),
    error = function(e) NULL
  )
  if (is.null(parameters)) return(NULL)

  if (has_output_section(outfiletext, "STANDARDIZED MODEL RESULTS")) {
    standardized <- tryCatch(
      extractH5StandardizedModelResults_1file(h5results, h5file, filename),
      error = function(e) NULL
    )
    expected <- expected_standardized_sections(outfiletext)
    if (is.null(standardized) || length(expected) == 0L ||
        !all(expected %in% names(standardized)) ||
        (has_output_section(outfiletext, "R-SQUARE") && !"r2" %in% names(standardized))) {
      return(NULL)
    }
    parameters <- c(parameters, standardized)
  }

  interval_type <- output_interval_type(outfiletext)
  if (!is.na(interval_type)) {
    confidence_intervals <- tryCatch(
      extractH5ConfidenceIntervals_1file(
        h5results, h5file, filename, interval_type = interval_type
      ),
      error = function(e) NULL
    )
    if (is.null(confidence_intervals) || is.null(confidence_intervals$ci.unstandardized)) {
      return(NULL)
    }
    parameters <- c(parameters, confidence_intervals)
  }

  standardized_interval_type <- output_interval_type(outfiletext, standardized = TRUE)
  if (!is.na(standardized_interval_type)) {
    standardized_ci <- tryCatch(
      extractH5StandardizedConfidenceIntervals_1file(
        h5results, h5file, filename, interval_type = standardized_interval_type
      ),
      error = function(e) NULL
    )
    expected_ci <- expected_standardized_sections(outfiletext, prefix = "ci.")
    if (is.null(standardized_ci) || length(expected_ci) == 0L ||
        !all(expected_ci %in% names(standardized_ci))) {
      return(NULL)
    }
    parameters <- c(parameters, standardized_ci)
  }

  if (has_output_prefix(outfiletext, "IRT PARAMETERIZATION")) {
    irt <- tryCatch(
      extractH5IRTParameterization_1file(h5results, h5file, filename),
      error = function(e) NULL
    )
    if (is.null(irt) || is.null(irt$irt.parameterization)) return(NULL)
    irt$irt.parameterization <- add_irt_metric_attribute(irt$irt.parameterization, outfiletext)
    parameters <- c(parameters, irt)
  }

  if (has_output_section(outfiletext, "RESULTS IN PROBABILITY SCALE")) {
    probability_scale <- tryCatch(
      extractH5ProbabilityScaleResults_1file(h5results, h5file, filename),
      error = function(e) NULL
    )
    if (is.null(probability_scale) || is.null(probability_scale$probability.scale)) {
      return(NULL)
    }
    parameters <- c(parameters, probability_scale)
  }

  has_odds <- has_output_prefix(outfiletext, "LOGISTIC REGRESSION ODDS RATIO RESULTS")
  has_odds_ci <- has_output_section(
    outfiletext, "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS"
  )
  if (has_odds) {
    odds <- tryCatch(
      extractH5OddsRatioResults_1file(
        h5results, h5file, filename,
        confidence_intervals = parameters$ci.unstandardized
      ),
      error = function(e) NULL
    )
    if (is.null(odds) || is.null(odds$odds) || (has_odds_ci && is.null(odds$ci.odds))) {
      return(NULL)
    }
    parameters <- c(parameters, odds)
  }

  order_h5_parameters(parameters)
}

extractParameters_1file_with_h5 <- function(outfiletext, filename, h5results,
                                             h5file, prefer_h5 = TRUE, efa = FALSE) {
  h5_parameters <- if (isTRUE(prefer_h5)) {
    extractH5Parameters_1file(outfiletext, filename, h5results, h5file, efa)
  } else {
    NULL
  }
  if (is.null(h5_parameters)) {
    extractParameters_1file(outfiletext, filename, efa = efa)
  } else {
    h5_parameters
  }
}
