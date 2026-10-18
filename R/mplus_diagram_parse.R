# Conservative adapter for continuous, single-group/single-level Mplus results.
# Defaults follow Mplus User's Guide chapters 14 and 17. Syntax is data only.

.mplus_diagram_range <- function(token, order = NULL) {
  token <- toupper(trimws(token))
  if (!grepl("-", token, fixed = TRUE)) return(token)
  ends <- strsplit(token, "-", fixed = TRUE)[[1L]]
  if (length(ends) != 2L) .mplus_diagram_abort(paste("Invalid variable range:", token), "unsupported")
  if (!is.null(order)) {
    at <- match(ends, order)
    if (anyNA(at) || at[1L] > at[2L])
      .mplus_diagram_abort(paste("Cannot resolve range in declared variable order:", token), "unsupported")
    return(order[seq.int(at[1L], at[2L])])
  }
  parts <- regmatches(ends, regexec("^([A-Z_][A-Z_0-9]*?)([0-9]+)$", ends, perl = TRUE))
  if (any(lengths(parts) != 3L) || parts[[1L]][2L] != parts[[2L]][2L])
    .mplus_diagram_abort(paste("Cannot expand declaration or label range:", token), "unsupported")
  first <- as.integer(parts[[1L]][3L]); last <- as.integer(parts[[2L]][3L])
  if (last < first || last - first > 10000)
    .mplus_diagram_abort(paste("Invalid range:", token), "unsupported")
  paste0(parts[[1L]][2L], seq.int(first, last))
}

.mplus_diagram_declaration <- function(text, order = NULL) {
  text <- toupper(gsub("\\s*-\\s*", "-", paste(text, collapse = " ")))
  tokens <- strsplit(trimws(gsub("[,;]", " ", text)), "[[:space:]]+")[[1L]]
  if (!length(tokens) || any(!grepl("^[A-Z_][A-Z_0-9]*(-[A-Z_][A-Z_0-9]*)?$", tokens)))
    .mplus_diagram_abort("Missing or unsupported VARIABLE names/usevariables declaration.", "unsupported")
  unlist(lapply(tokens, .mplus_diagram_range, order = order), use.names = FALSE)
}

# Preserve physical lines: a parenthesized label applies to preceding entries
# on that line; a range of labels assigns one label to each preceding entry.
.mplus_diagram_terms <- function(text, order, modifiers = TRUE) {
  text <- gsub("\\s*-\\s*", "-", text)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  terms <- list()
  for (line in lines) {
    rest <- trimws(gsub(",", " ", line, fixed = TRUE)); pending <- integer()
    while (nzchar(rest)) {
      if (startsWith(rest, "(")) {
        hit <- regmatches(rest, regexec("^\\(([A-Za-z_0-9-]+)\\)", rest))[[1L]]
        if (!modifiers || !length(hit) || !length(pending))
          .mplus_diagram_abort(paste("Ambiguous parameter label:", line), "unsupported")
        label <- hit[2L]
        if (grepl("^[0-9]+-[0-9]+$", label)) {
          ends <- as.integer(strsplit(label, "-", fixed = TRUE)[[1L]])
          if (ends[2L] < ends[1L] || diff(ends) > 10000) .mplus_diagram_abort("Invalid numeric label range.", "unsupported")
          labels <- as.character(seq.int(ends[1L], ends[2L]))
        } else if (grepl("-", label, fixed = TRUE)) {
          labels <- .mplus_diagram_range(label)
          prefix <- sub("[0-9]+-.*$", "", label)
          labels <- paste0(prefix, sub("^[A-Z_][A-Z_0-9]*?([0-9]+)$", "\\1", labels, perl = TRUE))
        } else labels <- label
        if (length(labels) != 1L && length(labels) != length(pending))
          .mplus_diagram_abort(paste("Label range does not match parameters:", line), "unsupported")
        for (j in seq_along(pending)) terms[[pending[j]]]$label <- labels[if (length(labels) == 1L) 1L else j]
        pending <- integer(); rest <- trimws(substring(rest, nchar(hit[1L]) + 1L)); next
      }
      hit <- regmatches(rest, regexec(
        "^([A-Z_][A-Z_0-9]*\\^?(?:-[A-Z_][A-Z_0-9]*\\^?)?)(?:\\s*([@*])\\s*([+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[ED][+-]?[0-9]+)?)?)?",
        toupper(rest), perl = TRUE))[[1L]]
      if (!length(hit)) .mplus_diagram_abort(paste("Unsupported MODEL syntax near:", rest), "unsupported")
      if (!modifiers && nzchar(hit[3L])) .mplus_diagram_abort("Modifiers on the left of a relationship are unsupported.", "unsupported")
      vars <- .mplus_diagram_range(hit[2L], order)
      if (any(!vars %in% order)) .mplus_diagram_abort(paste("Undeclared variable:", paste(vars[!vars %in% order], collapse = ", ")), "unsupported")
      # Bare @ uses an engine-dependent default; refuse to invent that value.
      if (hit[3L] == "@" && !nzchar(hit[4L])) .mplus_diagram_abort("Use an explicit numeric value after @ for diagram parsing.", "unsupported")
      value <- if (nzchar(hit[4L])) as.numeric(gsub("D", "E", hit[4L], fixed = TRUE)) else NULL
      for (var in vars) {
        terms[[length(terms) + 1L]] <- list(name = var, modifier = hit[3L], value = value, label = NULL)
        pending <- c(pending, length(terms))
      }
      rest <- trimws(substring(rest, nchar(hit[1L]) + 1L))
    }
  }
  if (!length(terms)) .mplus_diagram_abort("Empty MODEL relationship.", "unsupported")
  terms
}

.mplus_diagram_identity <- function(type, from, to) {
  if (type == "covariance") { pair <- sort(c(from, to)); from <- pair[1L]; to <- pair[2L] }
  list(id = paste(type, from, to, sep = ":"), from = paste0("node:", from),
       to = paste0("node:", to), type = type)
}

.mplus_diagram_result_identity <- function(header, param) {
  header <- toupper(header); param <- toupper(param)
  if (header %in% c("MEANS", "INTERCEPTS", "NEW.ADDITIONAL.PARAMETERS")) return(NULL)
  if (header %in% c("VARIANCES", "RESIDUAL.VARIANCES"))
    return(.mplus_diagram_identity("variance", param, param))
  bits <- regmatches(header, regexec("^([A-Z_][A-Z_0-9]*\\^?)\\.(BY|ON|WITH|\\|)$", header))[[1L]]
  if (!length(bits) || !grepl("^[A-Z_][A-Z_0-9]*\\^?$", param))
    .mplus_diagram_abort(paste("Unsupported result parameter:", header, param), "unsupported")
  switch(bits[3L], `|` = .mplus_diagram_identity("loading", bits[2L], param), BY = .mplus_diagram_identity("loading", bits[2L], param),
         ON = .mplus_diagram_identity("regression", param, bits[2L]),
         WITH = .mplus_diagram_identity("covariance", bits[2L], param))
}

.mplus_diagram_parse <- function(model, mapping = NULL) {
  fail <- function(message, code = "unsupported") .mplus_diagram_abort(message, code)
  diagnostics <- list()
  note <- function(code, message, severity = "warning") {
    diagnostics[[length(diagnostics) + 1L]] <<- list(severity = severity, code = code, message = message)
  }
  summary <- model$summaries
  if (!is.data.frame(summary) || nrow(summary) != 1L) fail("A single model summary is required.", "results")
  field <- function(name) if (name %in% names(summary)) summary[[name]][1L] else NULL
  scalar_text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!scalar_text(field("Estimator")) || !scalar_text(field("AnalysisType")) ||
      !is.numeric(field("NGroups")) || length(field("NGroups")) != 1L || !is.finite(field("NGroups")))
    fail("The complete model summary must identify Estimator, AnalysisType and NGroups without missing values.", "results")
  estimator <- toupper(field("Estimator"))
  if (!estimator %in% c("ML", "MLR")) fail("Diagram parsing currently requires continuous ML or MLR results.")
  if (field("NGroups") != 1 || toupper(field("AnalysisType")) != "GENERAL")
    fail("Only single-group TYPE=GENERAL models are supported.")
  if (!is.character(model[["output"]]) || !length(model[["output"]]) || anyNA(model[["output"]]))
    fail("Supply the complete mplus.model object or its .out file: raw output is required to verify termination and preserve diagnostics.", "results")
  errors <- unlist(model$errors, use.names = FALSE)
  warnings <- unlist(model$warnings, use.names = FALSE)
  raw_output <- paste(model$output, collapse = "\n")
  bad <- "DID NOT TERMINATE NORMALLY|NO CONVERGENCE|ESTIMATION (DID NOT|FAILED)|MODEL ESTIMATION HAS NOT|STANDARD ERRORS.*COULD NOT BE COMPUTED"
  if (grepl(bad, paste(c(raw_output, warnings, errors), collapse = "\n"), ignore.case = TRUE))
    fail("Mplus estimation failed or valid standard errors were unavailable; inspect the original output.", "results")
  normal <- grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", raw_output, fixed = TRUE)
  # Some TYPE=GENERAL STARTS outputs use ranked converged local maxima instead
  # of the normal-termination sentence (official Mplus book example 1.15 and
  # Web Talk 4; https://www.statmodel.com/mplusbook/chap1/ex1.15.html).
  # Require the complete ranked block, replicated best fit, model
  # results and numerical-quality report; a bare results heading is insufficient.
  if (!normal && length(model$output)) {
    lines <- trimws(model$output)
    start <- which(lines == "Fit function values at local maxima and random start numbers:")
    finish <- which(lines == "MODEL FIT INFORMATION")
    if (length(start) == 1L && length(finish) == 1L && finish > start &&
        any(lines == "MODEL RESULTS") && any(lines == "QUALITY OF NUMERICAL RESULTS")) {
      block <- lines[seq.int(start + 1L, finish - 1L)]
      rows <- block[grepl("^[+-]?[0-9]+\\.[0-9]+[[:space:]]+([0-9]+|unperturbed)$", block)]
      values <- suppressWarnings(as.numeric(sub("[[:space:]].*$", "", rows)))
      if (length(values) > 1L && all(is.finite(values)) && sum(values == values[1L]) >= 2L) {
        normal <- TRUE
        note("random_starts_termination", "Mplus reported replicated best local maxima from random starts; this output format omits the normal-termination sentence.", "info")
      }
    }
    if (!normal) fail("The output does not confirm normal model-estimation termination.", "results")
  }
  if (grepl("\\*\\*\\*[[:space:]]*ERROR", raw_output, ignore.case = TRUE) ||
      (length(errors) && !length(model$output)))
    fail(paste("Mplus reported errors:", paste(errors, collapse = "\n")), "results")
  for (warning in warnings) note("mplus_warning", warning)
  # MplusAutomation also places non-positive-definite/scale diagnostics in its
  # errors slot after normal termination. Preserve these visible diagnostics;
  # do not silently reclassify the solution as trustworthy or discard results.
  for (diagnostic in errors) note("mplus_diagnostic", diagnostic)
  input <- model$input
  if (!is.list(input) || !is.character(input[["model"]]) || !length(input[["model"]]))
    fail("The complete echoed MODEL syntax is required to establish topology and fixed parameters.")
  unsupported_sections <- setdiff(grep("^model", names(input), value = TRUE), c("model", "model.constraint", "model.indirect", "model.test"))
  if (length(unsupported_sections)) fail(paste("Unsupported model context:", paste(unsupported_sections, collapse = ", ")))
  variable <- input$variable
  if (!is.list(variable)) fail("The echoed VARIABLE declaration is required.")
  variable_keys <- c("names", "usevariables", "missing", "idvariable", "auxiliary", "useobservations", "weight", "frequency", "subpopulation", "categorical", "nominal", "censored", "count", "classes", "grouping", "cluster", "within", "between", "knownclass", "survival", "tscores", "stratification", "repweights", "replicate", "fweight", "bweight", "wtscale", "bwtscale")
  canonical <- pmatch(tolower(names(variable)), variable_keys)
  if (anyNA(canonical) || anyDuplicated(canonical)) fail("Ambiguous or unsupported VARIABLE option abbreviation.")
  names(variable) <- variable_keys[canonical]
  unsupported_variables <- intersect(names(variable), c("categorical", "nominal", "censored", "count", "classes", "grouping", "cluster", "within", "between", "knownclass", "survival", "tscores"))
  if (length(unsupported_variables)) fail(paste("Unsupported VARIABLE options:", paste(unsupported_variables, collapse = ", ")))
  if (!is.null(input$analysis) && !is.list(input$analysis)) fail("Malformed ANALYSIS input section.", "input")
  analysis_option <- function(name) {
    keys <- names(input$analysis)
    matches <- which(vapply(tolower(keys), function(key) nzchar(key) && startsWith(name, key), logical(1)))
    if (length(matches) > 1L) fail(paste("Ambiguous ANALYSIS option:", name))
    if (length(matches)) input$analysis[[matches]] else NULL
  }
  analysis_type <- analysis_option("type")
  if (!is.null(analysis_type) && (!scalar_text(analysis_type) || !startsWith("GENERAL", toupper(analysis_type))))
    fail("Unsupported ANALYSIS TYPE; only single-level GENERAL models are supported.")
  analysis <- paste(unlist(input$analysis), collapse = " ")
  if (grepl("TWOLEVEL|MIXTURE|RANDOM|BAYES|EFA", toupper(analysis)))
    fail("Unsupported ANALYSIS settings require a dedicated adapter.")
  model_defaults <- toupper(analysis_option("model"))
  if (length(model_defaults) && !all(model_defaults %in% c("NOCOV", "NOCOVARIANCES", "NOMEANSTRUCTURE")))
    fail("Unsupported ANALYSIS MODEL defaults.")
  no_cov <- any(model_defaults %in% c("NOCOV", "NOCOVARIANCES"))
  declared <- .mplus_diagram_declaration(variable$names)
  observed <- if (!is.null(variable$usevariables)) .mplus_diagram_declaration(variable$usevariables, declared) else declared
  syntax <- paste(sub("!.*$", "", input[["model"]]), collapse = "\n")
  if (grepl("[%{}$#]|\\bXWITH\\b", syntax, perl = TRUE)) fail("MODEL contains categorical, interaction or context syntax requiring a dedicated adapter.")
  if (!grepl(";[[:space:]]*$", syntax)) fail("MODEL syntax has an unterminated statement.")
  statements <- strsplit(syntax, ";", fixed = TRUE)[[1L]]
  statements <- statements[nzchar(trimws(statements))]
  growth_roles <- character(); growth_time <- numeric(); growth_construct <- character()
  growth_sources <- list(); expanded <- character()
  for (statement in statements) {
    if (!grepl("|", statement, fixed = TRUE)) { expanded <- c(expanded, statement); next }
    halves <- strsplit(statement, "|", fixed = TRUE)[[1L]]
    if (length(halves) != 2L) fail("Only a linear two-factor growth statement is supported.")
    factors <- strsplit(trimws(toupper(halves[1L])), "[[:space:]]+")[[1L]]
    if (length(factors) != 2L || any(!grepl("^[A-Z_][A-Z_0-9]*$", factors)))
      fail("Only linear growth with one intercept and one slope is supported; nonlinear/random-effect syntax is unsupported.")
    times <- .mplus_diagram_terms(halves[2L], observed)
    if (length(times) < 3L || any(vapply(times, function(t) t$modifier != "@" || !is.null(t$label), logical(1))))
      fail("Linear growth requires at least three indicators with explicit fixed time scores.")
    scores <- vapply(times, `[[`, numeric(1), "value")
    variables <- vapply(times, `[[`, character(1), "name")
    if (anyDuplicated(scores) || anyDuplicated(variables) || any(factors %in% names(growth_roles)) || any(variables %in% names(growth_time)))
      fail("Ambiguous or overlapping linear growth definitions.")
    growth_roles[factors] <- c("growth_intercept", "growth_slope")
    growth_time[variables] <- scores; growth_construct[variables] <- factors[1L]
    for (k in 1:2) {
      values <- if (k == 1L) rep(1, length(scores)) else scores
      expanded <- c(expanded, paste(factors[k], "BY", paste0(variables, "@", values, collapse = " ")))
      for (v in variables) growth_sources[[paste("loading", factors[k], v, sep = ":")]] <- trimws(statement)
    }
  }
  statements <- expanded
  has_hat <- grepl("^", syntax, fixed = TRUE)
  hat_order <- if (has_hat) paste0(observed, "^") else character()
  latent <- character()
  for (statement in statements) {
    hit <- regmatches(toupper(statement), regexec("^\\s*([A-Z_][A-Z_0-9]*)\\s+BY\\b", toupper(statement), perl = TRUE))[[1L]]
    if (length(hit)) latent <- c(latent, hit[2L])
  }
  latent <- unique(latent)
  if (length(intersect(latent, observed))) fail("A BY-defined latent variable conflicts with an observed variable declaration.")
  order <- c(observed, latent, hat_order); edges <- list(); means <- list(); by_seen <- character()
  put <- function(edge) {
    if (!is.null(edges[[edge$id]])) fail(paste("Repeated parameter specification needs explicit resolution:", edge$id))
    edges[[edge$id]] <<- edge
  }
  for (statement in statements) {
    original <- trimws(statement)
    rel <- regexpr("\\b(BY|ON|WITH|PON|PWITH)\\b", toupper(statement), perl = TRUE)
    if (rel[1L] > 0L) {
      op <- toupper(regmatches(statement, rel)); paired <- op %in% c("PON", "PWITH")
      op <- sub("^P", "", op)
      lhs <- .mplus_diagram_terms(substr(statement, 1L, rel[1L] - 1L), order, FALSE)
      rhs <- .mplus_diagram_terms(substring(statement, rel[1L] + attr(rel, "match.length")), order)
      if (op == "BY" && length(lhs) != 1L) fail("BY requires one explicitly named latent variable.")
      if (paired && length(lhs) != length(rhs)) fail("PON/PWITH requires equally sized left and right variable lists.")
      for (li in seq_along(lhs)) {
        left <- lhs[[li]]
        if (op == "BY" && left$name %in% by_seen) fail("Repeated BY statements require a dedicated loading-default adapter.")
        for (j in if (paired) li else seq_along(rhs)) {
          right <- rhs[[j]]
          type <- switch(op, BY = "loading", ON = "regression", WITH = "covariance")
          edge <- if (op == "ON") .mplus_diagram_identity(type, right$name, left$name) else .mplus_diagram_identity(type, left$name, right$name)
          default_fixed <- op == "BY" && j == 1L && right$modifier == ""
          edge$fixed <- right$modifier == "@" || default_fixed
          edge$fixed_value <- if (right$modifier == "@") right$value else if (default_fixed) 1 else NULL
          edge$label <- if (default_fixed) NULL else right$label
          edge$origin <- "explicit"; edge$source <- paste0("MODEL: ", original)
          edge$fixed_source <- if (default_fixed) "Mplus default first loading" else if (edge$fixed) "explicit @" else NULL
          if (!is.null(growth_sources[[edge$id]])) {
            edge$source <- paste0("MODEL: ", growth_sources[[edge$id]])
            edge$fixed_source <- "Mplus linear growth definition"
          }
          put(edge)
        }
        if (op == "BY") by_seen <- c(by_seen, left$name)
      }
    } else {
      is_mean <- grepl("^\\s*\\[", statement)
      if (is_mean) {
        if (!grepl("^\\s*\\[[^][]+\\]\\s*(\\([^()]+\\))?\\s*$", statement)) fail(paste("Unsupported mean statement:", original))
        statement <- gsub("[\\[\\]]", "", statement, perl = TRUE)
      }
      terms <- .mplus_diagram_terms(statement, order)
      for (term in terms) {
        edge <- .mplus_diagram_identity("variance", term$name, term$name)
        edge$fixed <- term$modifier == "@"; edge$fixed_value <- if (edge$fixed) term$value else NULL
        edge$label <- term$label; edge$origin <- "explicit"; edge$source <- paste0("MODEL: ", original)
        if (is_mean) means[[length(means) + 1L]] <- list(type = "mean", name = term$name, fixed = edge$fixed, fixed_value = edge$fixed_value, label = edge$label, source = edge$source) else put(edge)
      }
    }
  }
  # Mplus ^ denotes the residual of the original observed variable. Preserve
  # its own identity. The deterministic decomposition X = predicted(X) + X^
  # adds a unit connection and a zero leftover measurement variance, equivalent
  # to the explicit within-factor form (official comparison, pp. 1-3):
  # https://www.statmodel.com/download/RI-CLPM_with_hats.pdf
  hats <- unique(unlist(lapply(edges, function(e) sub("^node:", "", c(e$from, e$to)))))
  hats <- hats[endsWith(hats, "^")]
  for (hat in hats) {
    observed_name <- sub("\\^$", "", hat)
    incoming <- Filter(function(e) e$type %in% c("loading", "regression") && e$to == paste0("node:", observed_name), edges)
    if (!length(incoming)) fail(paste("Residual-hat variable has no modeled observed parent:", hat))
    for (type in c("loading", "variance")) {
      edge <- if (type == "loading") .mplus_diagram_identity(type, hat, observed_name) else .mplus_diagram_identity(type, observed_name, observed_name)
      if (!is.null(edges[[edge$id]])) fail(paste("Explicit observed variance conflicts with residual-hat decomposition:", observed_name))
      edge$fixed <- TRUE; edge$fixed_value <- if (type == "loading") 1 else 0
      edge$origin <- "default"; edge$source <- paste0("Mplus residual definition: ", observed_name, " = model prediction + ", hat)
      edge$fixed_source <- "Mplus residual-hat definition (not an estimated result row)"
      put(edge)
    }
  }
  if (!is.list(model$parameters)) fail("Model parameter tables are missing.", "results")
  params <- model$parameters$unstandardized
  if (!is.data.frame(params) || !nrow(params) || !all(c("paramHeader", "param", "est") %in% names(params))) fail("Unstandardized model parameter results are missing.", "results")
  tables <- list(unstandardized = params, stdyx = model$parameters$stdyx.standardized)
  groups <- character()
  for (scale in names(tables)) {
    table <- tables[[scale]]
    if (is.null(table)) next
    if (!is.data.frame(table)) fail(paste("Malformed", scale, "result table."), "results")
    if (!nrow(table)) next
    if (anyDuplicated(names(table)) || !all(c("paramHeader", "param", "est") %in% names(table)))
      fail(paste("Malformed", scale, "result table columns."), "results")
    for (key in c("paramHeader", "param")) {
      if (!is.character(table[[key]]) || anyNA(table[[key]]) || any(!nzchar(trimws(table[[key]]))))
        fail(paste("Missing or invalid parameter identity in", scale, "results."), "results")
    }
    for (key in intersect(c("est", "se", "est_se", "pval"), names(table)))
      if (!is.numeric(table[[key]])) fail(paste("Result column", key, "must be numeric in", scale, "results."), "results")
    if (scale == "unstandardized" && !any(is.finite(table$est)))
      fail("Unstandardized parameter results contain no finite estimates; a fitted diagram cannot be established.", "results")
    for (key in intersect(c("Group", "BetweenWithin", "LatentClass"), names(table))) {
      contexts <- unique(as.character(table[[key]][!is.na(table[[key]])]))
      contexts <- contexts[nzchar(trimws(contexts))]
      if (key == "Group") groups <- union(groups, contexts)
      else if (length(contexts)) fail(paste("Unsupported result context:", key, paste(contexts, collapse = ", ")))
    }
  }
  if (length(groups) > 1L) fail("Multiple group contexts cannot share single-group parameter identities.")
  results <- list(); active <- unique(unlist(lapply(edges, function(e) sub("^node:", "", c(e$from, e$to)))))
  # Recover only relationships actually reported by Mplus, never sample covariances.
  for (i in seq_len(nrow(params))) {
    ident <- .mplus_diagram_result_identity(params$paramHeader[i], params$param[i])
    if (is.null(ident)) next
    vars <- sub("^node:", "", c(ident$from, ident$to))
    if (any(!vars %in% order)) fail(paste("Result references an undeclared variable:", ident$id))
    if (is.null(edges[[ident$id]])) {
      if (ident$type %in% c("loading", "regression")) fail(paste("A result relationship is absent from echoed syntax:", ident$id))
      ident$fixed <- NULL; ident$fixed_value <- NULL; ident$label <- NULL
      ident$origin <- "unknown"; ident$source <- "Mplus result table; no explicit MODEL statement"
      if (ident$type == "variance") { ident$fixed <- FALSE; ident$origin <- "default" }
      if (ident$type == "covariance") {
        incoming <- vapply(edges, function(e) e$type %in% c("loading", "regression") && e$to %in% paste0("node:", vars), logical(1))
        if (!no_cov && all(vars %in% latent) && !any(incoming)) {
          ident$fixed <- FALSE; ident$origin <- "default"
          ident$source <- "Mplus default: covariances among continuous latent independent variables"
        }
      }
      edges[[ident$id]] <- ident
    }
    active <- union(active, vars)
  }
  if (!length(edges)) fail("No supported model relationships were found.")
  dependent_observed <- unique(vapply(Filter(function(e) e$type %in% c("loading", "regression") &&
                                      sub("^node:", "", e$to) %in% observed, edges),
                                     function(e) sub("^node:", "", e$to), character(1)))
  for (name in unique(c(latent, hats, dependent_observed))) {
    if (is.null(edges[[paste("variance", name, name, sep = ":")]]))
      fail(paste("Required latent or dependent residual variance is absent from syntax and results:", name), "results")
  }
  if (!no_cov) {
    independent_latent <- latent[!vapply(latent, function(name) any(vapply(edges,
      function(e) e$type %in% c("loading", "regression") && e$to == paste0("node:", name), logical(1))), logical(1))]
    if (length(independent_latent) > 1L) {
      pairs <- utils::combn(independent_latent, 2L, simplify = FALSE)
      for (pair in pairs) {
        id <- .mplus_diagram_identity("covariance", pair[1L], pair[2L])$id
        if (is.null(edges[[id]])) fail(paste("Required default covariance is absent from results:", id), "results")
      }
    }
  }
  # A syntax-only edge may have no result only if it is structurally fixed.
  number <- function(row, column) {
    if (!column %in% names(row)) return(NULL)
    value <- suppressWarnings(as.numeric(row[[column]][1L]))
    if (!length(value) || !is.finite(value)) NULL else value
  }
  for (scale in names(tables)) {
    table <- tables[[scale]]
    if (is.null(table) || !nrow(table)) next
    if (!all(c("paramHeader", "param", "est") %in% names(table))) fail(paste("Malformed", scale, "result table."), "results")
    seen <- character()
    for (i in seq_len(nrow(table))) {
      ident <- .mplus_diagram_result_identity(table$paramHeader[i], table$param[i])
      if (is.null(ident)) next
      id <- ident$id
      if (id %in% seen) fail(paste("Duplicate parameter identity in", scale, "results:", id), "results")
      if (is.null(edges[[id]])) fail(paste("Standardized result has no unstandardized structural identity:", id), "results")
      seen <- c(seen, id); row <- table[i, , drop = FALSE]
      est <- number(row, "est"); se <- number(row, "se"); z <- number(row, "est_se"); p <- number(row, "pval")
      if (scale == "unstandardized" && isTRUE(edges[[id]]$fixed) && !is.null(est)) {
        fixed <- edges[[id]]$fixed_value
        # Allow rounding in printed engine results, while rejecting a different
        # estimate attached to this structural constraint. Do not compare the
        # standardized loading with its original-scale fixed value.
        tolerance <- max(0.00051, abs(fixed) * 1e-7)
        if (!is.finite(fixed) || abs(est - fixed) > tolerance)
          fail(paste("Unstandardized result contradicts the fixed MODEL value:", id), "results")
      }
      # Coefficients and coherent large SE/z values can equal 999. Inferential
      # placeholders are recognized using their accompanying missing statistics.
      # A finite free-parameter z of 999 with a valid p-value is not by itself
      # evidence that a standard error of 999 is a placeholder. Missing optional
      # test columns likewise do not invalidate a supplied free-parameter SE.
      sentinel_se <- identical(se, 999) && ((isTRUE(edges[[id]]$fixed) && scale == "unstandardized") ||
                     identical(p, 999))
      if (!is.null(se) && (sentinel_se || se < 0)) se <- NULL
      if (!is.null(z) && abs(z) == 999 &&
          (isTRUE(edges[[id]]$fixed) && scale == "unstandardized" ||
           identical(number(row, "se"), 0) || identical(number(row, "pval"), 999))) z <- NULL
      if (!is.null(p) && (p == 999 || p < 0 || p > 1)) p <- NULL
      results[[id]][[scale]] <- list(estimate = est, se = se, z = z, p = p)
    }
  }
  for (id in names(edges)) {
    if (is.null(results[[id]]$unstandardized)) {
      if (!isTRUE(edges[[id]]$fixed)) fail(paste("No unstandardized result for free structural parameter:", id), "results")
      results[[id]]$unstandardized <- list(estimate = edges[[id]]$fixed_value, se = NULL, z = NULL, p = NULL)
    }
  }
  available_stdyx <- vapply(results, function(r) !is.null(r$stdyx$estimate) && is.finite(r$stdyx$estimate), logical(1))
  if (!any(available_stdyx)) note("missing_stdyx", "STDYX results are unavailable; the entire graph uses unstandardized estimates.")
  else if (!all(available_stdyx)) note("partial_stdyx", "Some parameters have no STDYX result and will be shown as unavailable on that scale.")
  if (any(vapply(edges, function(e) identical(e$origin, "unknown"), logical(1)))) note("unknown_origin", "Some reported covariances have unverified origins and remain visible by default.")
  constraints <- means
  if (no_cov) constraints[[length(constraints) + 1L]] <- list(type = "analysis_default", source = "MODEL=NOCOVARIANCES")
  if (length(input$model.constraint)) {
    constraints[[length(constraints) + 1L]] <- list(type = "model_constraint", source = paste(input$model.constraint, collapse = "\n"))
    note("model_constraints", "MODEL CONSTRAINT equations are preserved as source information; derived parameters are not diagram edges.", "info")
  }
  if (is.null(mapping)) mapping <- list()
  if (!is.list(mapping) || (length(mapping) && (is.null(names(mapping)) || anyDuplicated(names(mapping))))) fail("mapping must be a named list.", "input")
  if (length(setdiff(names(mapping), c("family", "waves", "constructs", "roles")))) fail("Unknown mapping field.", "input")
  for (key in intersect(names(mapping), c("waves", "constructs", "roles"))) {
    values <- mapping[[key]]
    if (is.null(names(values)) || anyNA(names(values)) || any(!nzchar(names(values))) || anyDuplicated(toupper(names(values)))) fail(paste("mapping", key, "must have unique original variable names."), "input")
    names(values) <- toupper(names(values))
    if (any(!names(values) %in% active) || anyNA(values)) fail(paste("mapping", key, "contains unknown variables or missing values."), "input")
    if (key == "waves" && (!is.numeric(values) || any(!is.finite(values)))) fail("mapping waves must be finite numeric values.", "input")
    if (key != "waves" && (!is.character(values) || any(!nzchar(values)))) fail(paste("mapping", key, "must contain nonempty character values."), "input")
    mapping[[key]] <- values
  }
  for (construct in unique(growth_construct)) {
    variables <- names(growth_construct)[growth_construct == construct]
    variables <- variables[variables %in% names(mapping$waves)]
    if (length(variables) > 1L) {
      variables <- variables[order(growth_time[variables])]
      if (any(diff(mapping$waves[variables]) <= 0))
        fail("Wave mapping contradicts the chronology of fixed growth time scores.", "input")
    }
  }
  inferred_roles <- growth_roles
  if (length(hats)) inferred_roles[hats] <- "within"
  # Identify RI components from their fixed decomposition, never from names.
  # Every repeated indicator must have either a ^ component, or a distinct
  # single-indicator latent component with fixed loading 1 and observed error 0.
  ri_candidates <- list()
  for (factor in setdiff(latent, names(growth_roles))) {
    loadings <- Filter(function(e) e$type == "loading" && e$from == paste0("node:", factor), edges)
    if (length(loadings) < 3L || !all(vapply(loadings, function(e) isTRUE(e$fixed) && identical(e$fixed_value, 1), logical(1)))) next
    targets <- vapply(loadings, function(e) sub("^node:", "", e$to), character(1))
    if (any(!targets %in% observed)) next
    within <- character()
    for (target in targets) {
      if (paste0(target, "^") %in% hats) { within <- c(within, paste0(target, "^")); next }
      variance <- edges[[paste("variance", target, target, sep = ":")]]
      if (is.null(variance) || !isTRUE(variance$fixed) || !identical(variance$fixed_value, 0)) break
      other <- Filter(function(e) e$type == "loading" && e$to == paste0("node:", target) && e$from != paste0("node:", factor), edges)
      if (length(other) != 1L || !isTRUE(other[[1L]]$fixed) || !identical(other[[1L]]$fixed_value, 1)) break
      name <- sub("^node:", "", other[[1L]]$from)
      if (sum(vapply(edges, function(e) e$type == "loading" && e$from == paste0("node:", name), logical(1))) != 1L) break
      within <- c(within, name)
    }
    if (length(within) != length(targets)) next
    dynamic <- Filter(function(e) e$type == "regression" && (e$from %in% paste0("node:", within) || e$to %in% paste0("node:", within)), edges)
    if (!length(dynamic)) next
    factor_incoming <- any(vapply(edges, function(e) e$type == "regression" && e$to == paste0("node:", factor), logical(1)))
    if (factor_incoming) next
    ri_candidates[[factor]] <- within
  }
  if (length(ri_candidates) >= 2L) {
    ri_names <- names(ri_candidates); within_names <- unique(unlist(ri_candidates))
    mixed_cov <- Filter(function(e) e$type == "covariance" &&
      ((e$from %in% paste0("node:", ri_names) && e$to %in% paste0("node:", within_names)) ||
       (e$to %in% paste0("node:", ri_names) && e$from %in% paste0("node:", within_names))) &&
       !(isTRUE(e$fixed) && identical(e$fixed_value, 0)), edges)
    if (!length(mixed_cov)) {
      inferred_roles[ri_names] <- "random_intercept"
      inferred_roles[within_names] <- "within"
    }
  }
  # Semantic mappings follow the verified decomposition, not variable names.
  # Users may annotate the observed measurement or its corresponding within
  # component; both describe the same measurement occasion and construct.
  for (within in names(inferred_roles)[inferred_roles == "within"]) {
    links <- Filter(function(e) e$type == "loading" && e$from == paste0("node:", within) &&
                      sub("^node:", "", e$to) %in% observed && isTRUE(e$fixed) && identical(e$fixed_value, 1), edges)
    if (length(links) != 1L) next
    measurement <- sub("^node:", "", links[[1L]]$to)
    for (key in c("waves", "constructs")) {
      values <- mapping[[key]]
      if (is.null(values)) next
      has_within <- within %in% names(values); has_measurement <- measurement %in% names(values)
      if (has_within && has_measurement && !identical(unname(values[[within]]), unname(values[[measurement]])))
        fail(paste("Mapping contradicts the within/observed decomposition:", within, measurement), "input")
      if (has_within && !has_measurement) values[measurement] <- values[[within]]
      if (has_measurement && !has_within) values[within] <- values[[measurement]]
      mapping[[key]] <- values
    }
  }
  for (factor in names(inferred_roles)[inferred_roles == "random_intercept"]) {
    loadings <- Filter(function(e) e$type == "loading" && e$from == paste0("node:", factor), edges)
    targets <- vapply(loadings, function(e) sub("^node:", "", e$to), character(1))
    if (!is.null(mapping$constructs) && all(targets %in% names(mapping$constructs))) {
      construct <- unique(unname(mapping$constructs[targets]))
      if (length(construct) != 1L) fail(paste("Random-intercept indicators have contradictory construct mappings:", factor), "input")
      if (factor %in% names(mapping$constructs) && mapping$constructs[[factor]] != construct)
        fail(paste("Random-intercept construct mapping contradicts its indicators:", factor), "input")
      mapping$constructs[factor] <- construct
    }
  }
  roles <- c("observed", "latent", "random_intercept", "within", "growth_intercept", "growth_slope")
  if (any(!mapping$roles %in% roles)) fail("Unknown mapping role.", "input")
  nodes <- lapply(sort(active), function(name) {
    role <- if (name %in% latent) "latent" else "observed"
    if (name %in% names(inferred_roles)) role <- inferred_roles[[name]]
    if (name %in% names(mapping$roles)) {
      requested <- mapping$roles[[name]]
      if ((role == "observed" && requested != "observed") || (role != "observed" && requested == "observed") ||
          (role %in% c("growth_intercept", "growth_slope", "within", "random_intercept") && requested != role)) fail(paste("Role mapping contradicts MODEL declaration for", name), "input")
      role <- requested
    }
    list(id = paste0("node:", name), name = name, role = role,
         wave = if (name %in% names(mapping$waves)) unname(mapping$waves[[name]]) else if (name %in% names(growth_time)) unname(growth_time[[name]]) else NULL,
         construct = if (name %in% names(mapping$constructs)) unname(mapping$constructs[[name]]) else if (name %in% names(growth_construct)) unname(growth_construct[[name]]) else NULL)
  })
  family <- if (any(vapply(edges, function(e) e$type == "regression", logical(1)))) "sem" else "cfa"
  if (length(growth_roles)) family <- "growth"
  if (any(inferred_roles == "random_intercept")) family <- "ri_clpm"
  if (family == "sem" && !is.null(mapping$waves) && !is.null(mapping$constructs) && !length(latent)) family <- "clpm"
  if (family %in% c("sem", "ri_clpm") && is.null(mapping$waves))
    note("unmapped_time", "No explicit wave/construct mapping is available; longitudinal order is not inferred from variable names.", "info")
  if (!is.null(mapping$family)) {
    if (length(mapping$family) != 1L || !mapping$family %in% c("cfa", "sem", "clpm", "ri_clpm", "growth")) fail("Invalid mapping family.", "input")
    if (family %in% c("ri_clpm", "growth") && mapping$family != family)
      fail("Family mapping contradicts the model family established by its structural definition.", "input")
    if (mapping$family == "cfa" && any(vapply(edges, function(e) e$type == "regression", logical(1))))
      fail("CFA mapping contradicts structural regression paths.", "input")
    if (mapping$family %in% c("clpm", "ri_clpm") && (is.null(mapping$waves) || is.null(mapping$constructs))) fail("Longitudinal mapping requires explicit waves and constructs.", "input")
    if (mapping$family == "growth" && !length(growth_roles)) fail("Growth mapping requires an explicit linear growth statement.")
    if (mapping$family == "ri_clpm" && !all(c("random_intercept", "within") %in% c(mapping$roles, inferred_roles))) fail("RI-CLPM mapping requires explicit random_intercept and within roles.", "input")
    family <- mapping$family
  }
  # Residual display is derived from directed model relationships, not from
  # result-table headings alone. In a hat model, X1^ is the substantive within
  # component: its initial variance is not a new error for that component. X2^
  # becomes endogenous only when it has an incoming dynamic regression. The
  # observed measurement node retains its separate (often fixed-zero) error.
  # These presentation helpers never change statistical identities/fingerprints.
  endogenous <- unique(vapply(Filter(function(e) e$type %in% c("loading", "regression"), edges),
                              `[[`, character(1), "to"))
  nodes <- lapply(nodes, function(node) {
    node$endogenous <- node$id %in% endogenous
    node
  })
  edges <- lapply(edges, function(edge) {
    if (edge$type == "variance") edge$residual <- edge$from %in% endogenous
    if (edge$type == "covariance") {
      edge$residual_from <- edge$from %in% endogenous
      edge$residual_to <- edge$to %in% endogenous
    }
    edge
  })
  path <- attr(model, "filename", exact = TRUE)
  if (is.null(path) || length(path) != 1L || !file.exists(path)) path <- NULL else path <- normalizePath(path, winslash = "/")
  list(structure = list(family = family, nodes = nodes, edges = unname(edges[sort(names(edges))]), constraints = constraints, mapping = mapping),
       results = results[sort(names(results))], diagnostics = diagnostics,
       provenance = list(path = path, title = field("Title"), estimator = estimator, engine = field("Mplus.version")))
}
