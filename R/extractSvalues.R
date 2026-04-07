#' Extract SVALUES syntax and parsed parameter table
#'
#' Internal parser for the "MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING
#' VALUES" section. This section is syntax-oriented rather than table-oriented,
#' so it is parsed directly from the raw text instead of relying on MODEL
#' RESULTS.
#'
#' @param outfiletext Parsed output text
#' @param filename Name of the output file
#' @param input Parsed Mplus input, if available
#' @return A list with `text` and `parameters` elements
#' @keywords internal
extractSvalues <- function(outfiletext, filename, input = NULL) {
  svalues_text <- getSection(
    "^MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES$",
    outfiletext
  )
  if (section_is_missing(svalues_text)) {
    return(list(text = NULL, parameters = NULL))
  }
  
  parsed <- parseSvaluesSyntax(as.character(svalues_text), filename = filename, input = input)
  list(text = as.character(svalues_text), parameters = parsed)
}

svalues_default_state <- function() {
  list(
    raw_block = NULL,
    is_overall = FALSE,
    latent_class = NULL,
    between_within = NULL,
    group = NULL
  )
}

svalues_parse_block <- function(block, state) {
  inner <- trimws(sub("^%(.*)%$", "\\1", trimws(block), perl = TRUE))
  inner_uc <- toupper(inner)
  
  new_state <- state
  new_state$raw_block <- inner
  new_state$is_overall <- grepl("\\bOVERALL\\b", inner_uc, perl = TRUE)
  
  if (grepl("\\bBETWEEN\\b", inner_uc, perl = TRUE)) {
    new_state$between_within <- "Between"
  } else if (grepl("\\bWITHIN\\b", inner_uc, perl = TRUE)) {
    new_state$between_within <- "Within"
  } else if (isTRUE(new_state$is_overall)) {
    new_state$between_within <- NULL
  }
  
  lc_match <- regexec("\\bC#(\\d+)\\b", inner_uc, perl = TRUE)
  lc_parts <- regmatches(inner_uc, lc_match)[[1L]]
  if (length(lc_parts) > 1L) {
    new_state$latent_class <- lc_parts[2L]
    new_state$is_overall <- FALSE
  } else if (isTRUE(new_state$is_overall)) {
    new_state$latent_class <- NULL
  }
  
  new_state
}

svalues_parse_model_header <- function(line, state) {
  match <- regexec("^MODEL\\s+([^:]+):\\s*$", trimws(line), perl = TRUE, ignore.case = TRUE)
  parts <- regmatches(trimws(line), match)[[1L]]
  if (length(parts) != 2L) return(NULL)
  
  hdr <- trimws(parts[2L])
  hdr_uc <- toupper(hdr)
  new_state <- state
  
  if (hdr_uc %in% c("", ":", "OVERALL")) {
    new_state$group <- NULL
    return(new_state)
  }
  
  new_state$group <- hdr_uc
  new_state$is_overall <- FALSE
  new_state$latent_class <- NULL
  new_state$between_within <- NULL
  new_state
}

svalues_split_statements <- function(section_lines) {
  section_lines <- sanitize_mplus_text(section_lines)
  
  state <- svalues_default_state()
  statements <- list()
  idx <- 1L
  buffer <- ""
  discovered_classes <- character(0)
  
  finalize_statement <- function(text, state_snapshot) {
    stmt <- trimws(text)
    if (!nzchar(stmt)) return()
    statements[[idx]] <<- list(statement = stmt, state = state_snapshot)
    idx <<- idx + 1L
  }
  
  for (line in section_lines) {
    line <- sub("\\s*!.*$", "", line, perl = TRUE)
    line <- trimws(line)
    if (!nzchar(line)) next
    
    if (grepl("^%.*%$", line, perl = TRUE)) {
      if (nzchar(trimws(buffer))) {
        finalize_statement(buffer, state)
        buffer <- ""
      }
      state <- svalues_parse_block(line, state)
      if (!is.null(state$latent_class)) {
        discovered_classes <- unique(c(discovered_classes, state$latent_class))
      }
      next
    }
    
    model_state <- svalues_parse_model_header(line, state)
    if (!is.null(model_state)) {
      if (nzchar(trimws(buffer))) {
        finalize_statement(buffer, state)
        buffer <- ""
      }
      state <- model_state
      next
    }
    
    buffer <- paste(trimws(c(buffer, line)), collapse = " ")
    repeat {
      semi_pos <- regexpr(";", buffer, fixed = TRUE)
      if (semi_pos[1L] < 0L) break
      finalize_statement(substr(buffer, 1L, semi_pos[1L] - 1L), state)
      buffer <- substr(buffer, semi_pos[1L] + 1L, nchar(buffer))
    }
  }
  
  if (nzchar(trimws(buffer))) {
    finalize_statement(buffer, state)
  }
  
  list(statements = statements, class_labels = discovered_classes)
}

svalues_prepare_statement <- function(statement) {
  stmt <- trimws(statement)
  labels <- character(0)
  
  repeat {
    match <- regexec("^(.*)\\(([^()]*)\\)\\s*$", stmt, perl = TRUE)
    parts <- regmatches(stmt, match)[[1L]]
    if (length(parts) != 3L) break
    labels <- c(svalues_split_tokens(parts[3L]), labels)
    stmt <- trimws(parts[2L])
  }
  
  list(statement = stmt, labels = labels)
}

svalues_split_tokens <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) return(character(0))
  unlist(strsplit(text, "\\s+", perl = TRUE), use.names = FALSE)
}

svalues_parse_token <- function(token) {
  token <- trimws(token)
  if (!nzchar(token)) {
    return(list(var = "", modifier = "", value = NA_real_))
  }
  
  mod_pos <- regexpr("[@*]", token, perl = TRUE)
  if (mod_pos[1L] < 0L) {
    return(list(var = token, modifier = "", value = NA_real_))
  }
  
  modifier <- substr(token, mod_pos[1L], mod_pos[1L])
  var <- trimws(substr(token, 1L, mod_pos[1L] - 1L))
  value_text <- trimws(substr(token, mod_pos[1L] + 1L, nchar(token)))
  value <- if (nzchar(value_text)) mplus_as.numeric(value_text) else NA_real_
  
  list(var = var, modifier = modifier, value = value)
}

svalues_make_row <- function(paramHeader, param, est, fixed, state, overall = FALSE) {
  data.frame(
    paramHeader = paramHeader,
    param = toupper(param),
    est = est,
    fixed = fixed,
    label = NA_character_,
    LatentClass = if (is.null(state$latent_class)) NA_character_ else state$latent_class,
    BetweenWithin = if (is.null(state$between_within)) NA_character_ else state$between_within,
    Group = if (is.null(state$group)) NA_character_ else state$group,
    .svalues_overall = overall,
    stringsAsFactors = FALSE
  )
}

svalues_apply_labels <- function(df, labels) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  if (length(labels) == 0L) return(df)
  
  if (length(labels) == 1L) {
    df$label[] <- labels[1L]
    return(df)
  }
  
  n <- nrow(df)
  k <- length(labels)
  if (k <= n) {
    idx <- seq.int(from = n - k + 1L, to = n)
    df$label[idx] <- labels
  } else {
    df$label[] <- labels[seq_len(n)]
  }
  
  df
}

svalues_detect_growth_factors <- function(statements) {
  out <- character(0)
  for (x in statements) {
    stmt <- svalues_prepare_statement(x$statement)$statement
    if (!grepl("|", stmt, fixed = TRUE)) next
    match <- regexec("^(.*?)\\s*\\|\\s*(.*)$", stmt, perl = TRUE)
    parts <- regmatches(stmt, match)[[1L]]
    if (length(parts) != 3L) next
    lhs_tokens <- svalues_split_tokens(expandCmd(parts[2L], expand_numeric = FALSE))
    if (length(lhs_tokens) > 0L) out <- unique(c(out, toupper(lhs_tokens)))
  }
  out
}

svalues_detect_latent_and_endogenous <- function(statements, growth_factors = character(0)) {
  latent_vars <- toupper(growth_factors)
  endogenous_vars <- character(0)
  
  for (x in statements) {
    stmt <- svalues_prepare_statement(x$statement)$statement
    if (!nzchar(stmt)) next
    
    match <- regexec("^(.*?)\\s+(ON|BY)\\s+(.*)$", stmt, perl = TRUE, ignore.case = TRUE)
    parts <- regmatches(stmt, match)[[1L]]
    if (length(parts) != 4L) next
    
    lhs_tokens <- toupper(svalues_split_tokens(expandCmd(parts[2L], expand_numeric = FALSE)))
    op <- toupper(parts[3L])
    
    if (op == "ON") {
      endogenous_vars <- unique(c(endogenous_vars, lhs_tokens))
    } else if (op == "BY") {
      latent_vars <- unique(c(latent_vars, lhs_tokens))
    }
  }
  
  list(latent_vars = latent_vars, endogenous_vars = endogenous_vars)
}

svalues_parse_growth <- function(statement, state) {
  match <- regexec("^(.*?)\\s*\\|\\s*(.*)$", statement, perl = TRUE)
  parts <- regmatches(statement, match)[[1L]]
  if (length(parts) != 3L) return(NULL)
  
  lhs_tokens <- svalues_split_tokens(expandCmd(parts[2L], expand_numeric = FALSE))
  rhs_tokens <- svalues_split_tokens(expandCmd(parts[3L], expand_numeric = FALSE))
  rhs_parsed <- lapply(rhs_tokens, svalues_parse_token)
  rhs_parsed <- rhs_parsed[vapply(rhs_parsed, function(x) nzchar(x$var), logical(1))]
  if (length(lhs_tokens) == 0L || length(rhs_parsed) == 0L) return(NULL)
  
  rows <- lapply(seq_along(lhs_tokens), function(i) {
    factor_name <- toupper(lhs_tokens[i])
    do.call(rbind, lapply(rhs_parsed, function(rhs) {
      est <- if (i == 1L) 1 else if (!is.na(rhs$value)) rhs$value^(i - 1L) else NA_real_
      if (is.na(est)) return(NULL)
      svalues_make_row(
        paramHeader = paste0(factor_name, ".|"),
        param = rhs$var,
        est = est,
        fixed = if (i == 1L) TRUE else identical(rhs$modifier, "@"),
        state = state,
        overall = isTRUE(state$is_overall)
      )
    }))
  })
  
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

svalues_parse_bracket <- function(statement, state, context = list()) {
  inner <- sub("^\\[(.*)\\]$", "\\1", statement, perl = TRUE)
  tokens <- svalues_split_tokens(expandCmd(inner, expand_numeric = FALSE))
  parsed <- lapply(tokens, svalues_parse_token)
  latent_vars <- toupper(if (is.null(context$latent_vars)) character(0) else context$latent_vars)
  endogenous_vars <- toupper(if (is.null(context$endogenous_vars)) character(0) else context$endogenous_vars)
  
  rows <- lapply(parsed, function(tok) {
    if (!nzchar(tok$var) || is.na(tok$value)) return(NULL)
    var_uc <- toupper(tok$var)
    header <- if (grepl("$", tok$var, fixed = TRUE)) {
      "Thresholds"
    } else if (var_uc %in% latent_vars && !var_uc %in% endogenous_vars) {
      "Means"
    } else {
      "Intercepts"
    }
    svalues_make_row(
      paramHeader = header,
      param = tok$var,
      est = tok$value,
      fixed = identical(tok$modifier, "@"),
      state = state,
      overall = isTRUE(state$is_overall)
    )
  })
  
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

svalues_parse_brace <- function(statement, state) {
  inner <- sub("^\\{(.*)\\}$", "\\1", statement, perl = TRUE)
  tokens <- svalues_split_tokens(expandCmd(inner, expand_numeric = FALSE))
  parsed <- lapply(tokens, svalues_parse_token)
  
  rows <- lapply(parsed, function(tok) {
    if (!nzchar(tok$var) || is.na(tok$value)) return(NULL)
    svalues_make_row(
      paramHeader = "Scales",
      param = tok$var,
      est = tok$value,
      fixed = identical(tok$modifier, "@"),
      state = state,
      overall = isTRUE(state$is_overall)
    )
  })
  
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

svalues_parse_relation <- function(statement, state) {
  match <- regexec("^(.*?)\\s+(ON|WITH|BY)\\s+(.*)$", statement, perl = TRUE, ignore.case = TRUE)
  parts <- regmatches(statement, match)[[1L]]
  if (length(parts) != 4L) return(NULL)
  
  lhs_tokens <- svalues_split_tokens(expandCmd(parts[2L], expand_numeric = FALSE))
  operator <- toupper(parts[3L])
  rhs_tokens <- svalues_split_tokens(expandCmd(parts[4L], expand_numeric = FALSE))
  rhs_parsed <- lapply(rhs_tokens, svalues_parse_token)
  rhs_parsed <- rhs_parsed[vapply(rhs_parsed, function(x) nzchar(x$var) && !is.na(x$value), logical(1))]
  if (length(lhs_tokens) == 0L || length(rhs_parsed) == 0L) return(NULL)
  
  rows <- lapply(lhs_tokens, function(lhs) {
    do.call(rbind, lapply(rhs_parsed, function(rhs) {
      svalues_make_row(
        paramHeader = paste0(toupper(lhs), ".", operator),
        param = rhs$var,
        est = rhs$value,
        fixed = identical(rhs$modifier, "@"),
        state = state,
        overall = isTRUE(state$is_overall)
      )
    }))
  })
  
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

svalues_parse_variance <- function(statement, state, context = list()) {
  tokens <- svalues_split_tokens(expandCmd(statement, expand_numeric = FALSE))
  parsed <- lapply(tokens, svalues_parse_token)
  latent_vars <- toupper(if (is.null(context$latent_vars)) character(0) else context$latent_vars)
  endogenous_vars <- toupper(if (is.null(context$endogenous_vars)) character(0) else context$endogenous_vars)
  
  rows <- lapply(parsed, function(tok) {
    if (!nzchar(tok$var) || is.na(tok$value)) return(NULL)
    var_uc <- toupper(tok$var)
    svalues_make_row(
      paramHeader = if (var_uc %in% latent_vars && !var_uc %in% endogenous_vars) "Variances" else "Residual.Variances",
      param = tok$var,
      est = tok$value,
      fixed = identical(tok$modifier, "@"),
      state = state,
      overall = isTRUE(state$is_overall)
    )
  })
  
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

svalues_is_cat_latent_row <- function(df) {
  grepl("#", df$paramHeader, fixed = TRUE) | grepl("#", df$param, fixed = TRUE)
}

svalues_expand_overall_rows <- function(df, class_labels) {
  if (!".svalues_overall" %in% names(df) || nrow(df) == 0L) return(df)
  
  overall_idx <- which(df$.svalues_overall)
  if (length(overall_idx) == 0L) {
    df$.svalues_overall <- NULL
    return(df)
  }
  
  expanded <- lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]
    if (!isTRUE(row$.svalues_overall)) return(row)
    
    if (svalues_is_cat_latent_row(row)) {
      row$LatentClass <- "Categorical.Latent.Variables"
      return(row)
    }
    
    if (length(class_labels) == 0L) {
      row$LatentClass <- NA_character_
      return(row)
    }
    
    out <- row[rep(1L, length(class_labels)), , drop = FALSE]
    out$LatentClass <- class_labels
    out
  })
  
  df <- do.call(rbind, expanded)
  df$.svalues_overall <- NULL
  rownames(df) <- NULL
  df
}

svalues_finalize <- function(df, filename) {
  if (is.null(df) || nrow(df) == 0L) return(NULL)
  
  optional <- c("LatentClass", "BetweenWithin", "Group", "ReferenceClass")
  keep_optional <- optional[optional %in% names(df)]
  if (length(keep_optional) > 0L) {
    drop_cols <- keep_optional[vapply(df[keep_optional], function(x) all(is.na(x)), logical(1))]
    if (length(drop_cols) > 0L) df[drop_cols] <- NULL
  }
  
  class(df) <- c("mplus.params", "data.frame")
  attr(df, "filename") <- filename
  df
}

parseSvaluesSyntax <- function(section_lines, filename, input = NULL) {
  parsed <- svalues_split_statements(section_lines)
  statements <- parsed$statements
  class_labels <- parsed$class_labels
  growth_factors <- svalues_detect_growth_factors(statements)
  latent_endogenous <- svalues_detect_latent_and_endogenous(statements, growth_factors = growth_factors)
  context <- c(list(growth_factors = growth_factors), latent_endogenous)
  
  if (length(statements) == 0L) return(NULL)
  
  rows <- lapply(statements, function(x) {
    stmt_info <- svalues_prepare_statement(x$statement)
    stmt <- stmt_info$statement
    labels <- stmt_info$labels
    if (!nzchar(stmt)) return(NULL)
    
    if (grepl("|", stmt, fixed = TRUE)) {
      res <- svalues_parse_growth(stmt, x$state)
      if (!is.null(res)) return(svalues_apply_labels(res, labels))
    }
    
    if (grepl("^\\[.*\\]$", stmt, perl = TRUE)) {
      res <- svalues_parse_bracket(stmt, x$state, context = context)
      if (!is.null(res)) return(svalues_apply_labels(res, labels))
    }
    
    if (grepl("^\\{.*\\}$", stmt, perl = TRUE)) {
      res <- svalues_parse_brace(stmt, x$state)
      if (!is.null(res)) return(svalues_apply_labels(res, labels))
    }
    
    if (grepl("\\s+(ON|WITH|BY)\\s+", stmt, perl = TRUE, ignore.case = TRUE)) {
      res <- svalues_parse_relation(stmt, x$state)
      if (!is.null(res)) return(svalues_apply_labels(res, labels))
    }
    
    svalues_apply_labels(svalues_parse_variance(stmt, x$state, context = context), labels)
  })
  
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(NULL)
  
  df <- do.call(rbind, rows)
  df <- svalues_expand_overall_rows(df, class_labels)
  svalues_finalize(df, filename)
}
