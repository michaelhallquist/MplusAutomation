# Internal helpers for extracting observed variable names from Mplus syntax.

.mplus_append_unique_ci <- function(base, add) {
  if (length(add) == 0L) return(base)
  out <- base
  out_lc <- tolower(base)
  for (a in add) {
    if (!nzchar(a)) next
    a_lc <- tolower(a)
    if (!a_lc %in% out_lc) {
      out <- c(out, a)
      out_lc <- c(out_lc, a_lc)
    }
  }
  out
}

.mplus_match_ci <- function(x, reference) {
  if (length(x) == 0L || length(reference) == 0L) return(character(0))
  ref_lc <- tolower(reference)
  out <- character(0)
  for (token in x) {
    idx <- match(tolower(token), ref_lc)
    if (!is.na(idx)) out <- .mplus_append_unique_ci(out, reference[idx])
  }
  out
}

.mplus_strip_comments <- function(x) {
  if (length(x) == 0L) return(x)
  gsub("\\s*!.*$", "", x, perl = TRUE)
}

.mplus_flatten_section <- function(x) {
  if (is.null(x)) return("")
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x <- gsub("\r", "", x, fixed = TRUE)
  x <- .mplus_strip_comments(x)
  paste(x, collapse = " ")
}

.mplus_clean_var_token <- function(token) {
  tok <- trimws(token)
  if (!nzchar(tok)) return("")
  tok <- gsub("^[\\[\\]\\(\\)\\{\\},;]+", "", tok, perl = TRUE)
  tok <- gsub("[\\[\\]\\(\\)\\{\\},;]+$", "", tok, perl = TRUE)
  tok <- sub("\\$\\d+$", "", tok, perl = TRUE)
  tok <- sub("#\\d+$", "", tok, perl = TRUE)
  tok <- sub("([@*]).*$", "", tok, perl = TRUE)
  tok <- sub("\\(.*$", "", tok, perl = TRUE)
  tok <- gsub("^\\.+", "", tok, perl = TRUE)
  tok <- gsub("\\.+$", "", tok, perl = TRUE)
  if (!nzchar(tok)) return("")
  if (grepl("^[+-]?(?:\\d+\\.?\\d*|\\.\\d+)$", tok, perl = TRUE)) return("")
  tok
}

.mplus_filter_identifier_keywords <- function(tokens) {
  if (length(tokens) == 0L) return(tokens)
  keywords <- c(
    "title", "data", "file", "variable", "define", "analysis", "model", "output",
    "savedata", "plot", "montecarlo", "is", "are", "all", "on", "by", "with",
    "within", "between", "overall", "if", "then", "else", "and", "or", "not",
    "to", "at", "new", "loop", "series", "lagged", "type", "seed", "starts",
    "processors", "estimator", "algorithm", "integration", "knownclass", "classes",
    "grouping", "missing", "names", "usevariables", "idvariable", "cluster",
    "auxiliary", "categorical", "nominal", "count", "censored", "censoring",
    "survival", "binary", "ordered", "unordered", "weight", "weights",
    "fweight", "freqweight", "bweight", "stratification", "training", "impute",
    "nobservations", "nreps", "nrep", "ncsizes", "csizes", "repsave", "save",
    "cutpoints", "center", "grandmean", "groupmean", "clustermean",
    "useobservations", "subpopulation", "population", "coverage", "descriptive",
    "yes", "no", "ml", "mlr", "wlsmv", "bayes", "swmatrix", "tscores"
  )
  funcs <- c(
    "abs", "sqrt", "exp", "log", "log10", "sin", "cos", "tan", "asin", "acos",
    "atan", "floor", "ceiling", "round", "mod", "min", "max", "sum", "mean",
    "sd", "var", "int"
  )
  drop <- c(keywords, funcs)
  tokens[!tolower(tokens) %in% drop]
}

.mplus_parse_var_list <- function(text) {
  x <- .mplus_flatten_section(text)
  if (!nzchar(x)) return(character(0))
  x <- gsub("%[^%]*%", " ", x, perl = TRUE)
  x <- gsub("\"[^\"]*\"|'[^']*'", " ", x, perl = TRUE)
  x <- gsub("\\([^\\)]*\\)", " ", x, perl = TRUE)
  x <- expandCmd(x, expand_numeric = FALSE)
  x <- gsub("[,|]", " ", x, perl = TRUE)
  tokens <- unlist(strsplit(x, "\\s+", perl = TRUE), use.names = FALSE)
  tokens <- vapply(tokens, .mplus_clean_var_token, character(1), USE.NAMES = FALSE)
  tokens <- tokens[nzchar(tokens)]
  .mplus_filter_identifier_keywords(tokens)
}

.mplus_parse_missing_vars <- function(text) {
  x <- .mplus_flatten_section(text)
  if (!nzchar(x)) return(character(0))
  x <- gsub("(?i)\\ball\\s*\\([^\\)]*\\)", " ", x, perl = TRUE)
  x <- expandCmd(x, expand_numeric = FALSE)
  hits <- gregexpr("([A-Za-z_][A-Za-z0-9_\\.]*(?:\\s*-\\s*[A-Za-z_][A-Za-z0-9_\\.]*)?)\\s*\\(", x, perl = TRUE)[[1L]]
  out <- character(0)
  if (hits[1L] > 0L) {
    caps <- attr(hits, "capture.start")
    lens <- attr(hits, "capture.length")
    for (i in seq_along(hits)) {
      term <- substr(x, caps[i, 1L], caps[i, 1L] + lens[i, 1L] - 1L)
      out <- .mplus_append_unique_ci(out, .mplus_parse_var_list(term))
    }
  } else {
    out <- .mplus_parse_var_list(x)
  }
  out
}

.mplus_parse_knownclass_vars <- function(text) {
  x <- .mplus_flatten_section(text)
  if (!nzchar(x)) return(character(0))
  out <- character(0)
  parens <- gregexpr("\\(([^\\)]*)\\)", x, perl = TRUE)[[1L]]
  if (parens[1L] > 0L) {
    caps <- attr(parens, "capture.start")
    lens <- attr(parens, "capture.length")
    for (i in seq_along(parens)) {
      inside <- substr(x, caps[i, 1L], caps[i, 1L] + lens[i, 1L] - 1L)
      eqs <- gregexpr("([A-Za-z_][A-Za-z0-9_\\.]*)\\s*=", inside, perl = TRUE)[[1L]]
      if (eqs[1L] > 0L) {
        ecaps <- attr(eqs, "capture.start")
        elens <- attr(eqs, "capture.length")
        for (j in seq_along(eqs)) {
          out <- .mplus_append_unique_ci(
            out,
            substr(inside, ecaps[j, 1L], ecaps[j, 1L] + elens[j, 1L] - 1L)
          )
        }
      } else {
        out <- .mplus_append_unique_ci(out, .mplus_parse_var_list(inside))
      }
    }
  }
  .mplus_filter_identifier_keywords(out)
}

.mplus_parse_grouping_var <- function(text) {
  x <- .mplus_flatten_section(text)
  if (!nzchar(x)) return(character(0))
  m <- regexpr("^[[:space:]]*([A-Za-z_][A-Za-z0-9_\\.]*)", x, perl = TRUE)
  if (m[1L] < 0L) return(character(0))
  caps <- attr(m, "capture.start")
  lens <- attr(m, "capture.length")
  token <- substr(x, caps[1L], caps[1L] + lens[1L] - 1L)
  token <- .mplus_clean_var_token(token)
  if (!nzchar(token)) return(character(0))
  .mplus_filter_identifier_keywords(token)
}

.mplus_extract_identifiers <- function(text) {
  x <- .mplus_flatten_section(text)
  if (!nzchar(x)) return(character(0))
  x <- gsub("%[^%]*%", " ", x, perl = TRUE)
  x <- gsub("\"[^\"]*\"|'[^']*'", " ", x, perl = TRUE)
  x <- expandCmd(x, expand_numeric = FALSE)
  hits <- gregexpr("[A-Za-z_][A-Za-z0-9_\\.#$]*", x, perl = TRUE)[[1L]]
  if (hits[1L] < 0L) return(character(0))
  tokens <- regmatches(x, list(hits))[[1L]]
  tokens <- vapply(tokens, .mplus_clean_var_token, character(1), USE.NAMES = FALSE)
  tokens <- tokens[nzchar(tokens)]
  .mplus_filter_identifier_keywords(tokens)
}

.mplus_as_variable_fields <- function(variable) {
  if (is.null(variable)) return(NULL)
  if (is.list(variable)) return(variable)
  if (!is.character(variable)) return(NULL)
  lines <- unlist(strsplit(variable, "\n", fixed = TRUE), use.names = FALSE)
  if (length(lines) == 0L) return(NULL)
  lines <- gsub("\r", "", lines, fixed = TRUE)
  lines <- .mplus_strip_comments(lines)
  lines[1L] <- sub("^[^:]+:(.*)$", "\\1", lines[1L], perl = TRUE)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0L) return(NULL)
  tryCatch(divideIntoFields(lines), error = function(e) NULL)
}

#' Extract observed variable names from Mplus syntax
#'
#' Internal utility used by `mplusModel` and `detectVariables`.
#'
#' @param parsed_syntax Parsed Mplus syntax list (typically from `parseMplusSyntax`).
#' @param variable Mplus VARIABLE section (character string or parsed list).
#' @param define Mplus DEFINE section.
#' @param model Mplus MODEL section.
#' @param model_sections Optional list of MODEL* sections to scan.
#' @param data_names Optional character vector of data.frame column names used
#'   to filter detected identifiers down to observed variables.
#' @return Character vector of detected observed variables.
#' @keywords internal
extractMplusVariables <- function(parsed_syntax = NULL, variable = NULL, define = NULL,
                                  model = NULL, model_sections = NULL, data_names = NULL) {
  if (!is.null(parsed_syntax)) {
    if (is.character(parsed_syntax)) {
      parsed_syntax <- parseMplusSyntax(parsed_syntax, dropSectionNames = TRUE)
    }
    if (is.list(parsed_syntax)) {
      if (is.null(variable) && !is.null(parsed_syntax$variable)) variable <- parsed_syntax$variable
      if (is.null(define) && !is.null(parsed_syntax$define)) define <- parsed_syntax$define
      if (is.null(model) && !is.null(parsed_syntax$model)) model <- parsed_syntax$model
      if (is.null(model_sections)) {
        mnames <- names(parsed_syntax)[grepl("^model", names(parsed_syntax))]
        if (length(mnames) > 0L) model_sections <- unname(parsed_syntax[mnames])
      }
    }
  }
  if (is.null(model_sections)) model_sections <- list(model)

  variable_fields <- .mplus_as_variable_fields(variable)

  declared <- character(0)
  if (!is.null(variable_fields$names)) {
    declared <- .mplus_parse_var_list(variable_fields$names)
  }

  detected <- character(0)

  if (!is.null(variable_fields$usevariables)) {
    detected <- .mplus_append_unique_ci(detected, .mplus_parse_var_list(variable_fields$usevariables))
  }

  role_fields <- c(
    "idvariable", "cluster", "within", "between", "auxiliary",
    "categorical", "nominal", "count", "censored", "censoring",
    "survival", "binary", "ordered", "unordered", "weight",
    "weights", "fweight", "freqweight", "bweight", "stratification",
    "training", "impute", "tscores", "lagged"
  )
  for (field in role_fields) {
    if (!is.null(variable_fields[[field]])) {
      detected <- .mplus_append_unique_ci(detected, .mplus_parse_var_list(variable_fields[[field]]))
    }
  }

  if (!is.null(variable_fields$grouping)) {
    detected <- .mplus_append_unique_ci(detected, .mplus_parse_grouping_var(variable_fields$grouping))
  }
  if (!is.null(variable_fields$knownclass)) {
    detected <- .mplus_append_unique_ci(detected, .mplus_parse_knownclass_vars(variable_fields$knownclass))
  }
  if (!is.null(variable_fields$missing)) {
    detected <- .mplus_append_unique_ci(detected, .mplus_parse_missing_vars(variable_fields$missing))
  }

  expr_fields <- c("useobservations", "subpopulation")
  for (field in expr_fields) {
    if (!is.null(variable_fields[[field]])) {
      detected <- .mplus_append_unique_ci(detected, .mplus_extract_identifiers(variable_fields[[field]]))
    }
  }

  if (!is.null(define)) {
    detected <- .mplus_append_unique_ci(detected, .mplus_extract_identifiers(define))
  }
  for (sec in model_sections) {
    detected <- .mplus_append_unique_ci(detected, .mplus_extract_identifiers(sec))
  }

  if (!is.null(data_names) && length(data_names) > 0L) {
    data_names <- as.character(data_names)
    out <- .mplus_match_ci(detected, data_names)
    if (length(out) == 0L && length(declared) > 0L) {
      out <- .mplus_match_ci(declared, data_names)
    }
    return(out)
  }

  if (length(declared) > 0L) {
    out <- .mplus_match_ci(detected, declared)
    if (length(out) == 0L) out <- declared
    return(out)
  }

  detected
}
