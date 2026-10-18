# Presentation persistence: never serialise result values into configuration.
.mplus_diagram_object <- function() stats::setNames(list(), character())

.mplus_diagram_fingerprint <- function(structure) {
  # Canonical order excludes provenance/result values and result table row order.
  canonical <- function(x) {
    if (is.list(x)) {
      if (!is.null(names(x))) x <- x[order(names(x))]
      return(lapply(x, canonical))
    }
    if (!is.null(names(x))) x <- x[order(names(x))]
    x
  }
  nodes <- lapply(structure$nodes, function(n) n[intersect(
    c("id", "name", "role", "wave", "construct", "time"), names(n))])
  edges <- lapply(structure$edges, function(e) e[intersect(
    c("id", "from", "to", "type", "fixed", "fixed_value", "label"), names(e))])
  sort_records <- function(x) {
    if (!length(x)) return(list())
    x[order(vapply(x, function(y) y$id, character(1)))]
  }
  constraints <- lapply(structure$constraints, function(x) {
    if (identical(x$type, "mean")) x$source <- NULL
    if (identical(x$type, "model_constraint") && is.character(x$source)) {
      statements <- strsplit(toupper(gsub("[[:space:]]", "", x$source)), ";", fixed = TRUE)[[1L]]
      x$source <- paste(sort(statements[nzchar(statements)]), collapse = ";")
    }
    x
  })
  if (length(constraints)) {
    keys <- vapply(constraints, function(x) as.character(jsonlite::toJSON(
      canonical(x), auto_unbox = TRUE, null = "null", digits = NA)), character(1))
    constraints <- constraints[order(keys)]
  }
  data <- canonical(list(family = structure$family, nodes = sort_records(nodes),
    edges = sort_records(edges), constraints = constraints, mapping = structure$mapping))
  digest::digest(jsonlite::toJSON(data, auto_unbox = TRUE, null = "null",
    na = "null", digits = NA), algo = "sha256", serialize = FALSE)
}

# Snapshot content ID: raw imported output plus current graph results. Raw text
# order is retained; parameter-table/list order is not part of result identity.
.mplus_diagram_result_id <- function(output, results) {
  canonical <- function(x) {
    if (is.list(x)) {
      if (!is.null(names(x))) x <- x[order(names(x))]
      return(lapply(x, canonical))
    }
    if (!is.null(names(x))) x <- x[order(names(x))]
    x
  }
  snapshot <- list(output = unname(as.list(output)), results = canonical(results))
  digest::digest(jsonlite::toJSON(snapshot, auto_unbox = TRUE, null = "null",
    na = "null", digits = NA), algo = "sha256", serialize = FALSE)
}

.mplus_diagram_source <- function(graph) {
  keys <- c("title", "output", "estimator", "engine", "result_id")
  stats::setNames(lapply(keys, function(key) {
    value <- graph$provenance[[key]]
    if (is.atomic(value) && length(value) == 1L && !is.na(value)) as.character(value) else NULL
  }), keys)
}

.mplus_diagram_config <- function(widget) {
  list(schema_version = 1L, fingerprint = widget$x$model$fingerprint,
       layout_version = 1L, state = widget$x$state,
       source = .mplus_diagram_source(widget$x$model))
}

.mplus_diagram_validate_config <- function(config, graph) {
  fail <- function(message) .mplus_diagram_abort(message, "config")
  fields <- function(x, allowed, required = allowed) {
    if (!is.list(x) || (length(x) && (is.null(names(x)) || anyDuplicated(names(x)) ||
        any(!names(x) %in% allowed))) || !all(required %in% names(x)))
      fail("Configuration has missing, duplicate, or unknown fields.")
  }
  number <- function(x, lower = -1e6, upper = 1e6) {
    is.numeric(x) && length(x) == 1L && is.finite(x) && x >= lower && x <= upper
  }
  dictionary <- function(x, ids, all = FALSE) {
    if (!is.list(x) || (length(x) && (is.null(names(x)) || anyDuplicated(names(x)) ||
        any(!names(x) %in% ids))) || (all && !setequal(names(x), ids)))
      fail("Configuration contains missing or unknown model identities.")
  }
  required <- c("schema_version", "fingerprint", "layout_version", "state")
  fields(config, c(required, "source"), required)
  if ("source" %in% names(config)) {
    fields(config$source, c("title", "output", "estimator", "engine", "result_id"))
    if (any(!vapply(config$source, function(value) is.null(value) ||
        (is.character(value) && length(value) == 1L && !is.na(value)), logical(1))))
      fail("Configuration source metadata must contain only strings or null values.")
    if (!is.null(config$source$result_id) && !grepl("^[A-Fa-f0-9]{64}$", config$source$result_id))
      fail("Configuration source result_id must be a SHA256 hexadecimal content identifier.")
  }
  if (!number(config$schema_version, 1, 1) ||
      !number(config$layout_version, 1, 1)) fail("Unsupported configuration or layout version.")
  if (!identical(config$fingerprint, graph$fingerprint)) fail("Configuration structure does not match this model.")
  state <- config$state
  fields(state, c("options", "aliases", "hidden_edges", "hidden_nodes", "views"),
    c("options", "aliases", "hidden_edges", "views"))
  if (!"hidden_nodes" %in% names(state)) state$hidden_nodes <- list()
  # Schema 1 predates these display-only flags. Missing additions receive
  # their defaults, while all original options remain mandatory and typos still
  # fail validation. Explicit public options are applied after this migration.
  .mplus_diagram_validate_options(state$options)
  additions <- c("stars", "nonsignificant_dotted", "nonsignificant_hide_values")
  original <- setdiff(names(.mplus_diagram_defaults()), additions)
  if (!all(original %in% names(state$options))) fail("Configuration is missing required display options.")
  for (key in setdiff(additions, names(state$options)))
    state$options[[key]] <- .mplus_diagram_defaults()[[key]]
  .mplus_diagram_validate_options(state$options, complete = TRUE)
  nodes <- vapply(graph$structure$nodes, `[[`, character(1), "id")
  edges <- vapply(graph$structure$edges, `[[`, character(1), "id")
  edge_types <- stats::setNames(vapply(graph$structure$edges, `[[`, character(1), "type"), edges)
  dictionary(state$aliases, nodes)
  for (alias in state$aliases) {
    if (!is.character(alias) || length(alias) != 1L || is.na(alias) ||
        nchar(alias, type = "chars") > 500L) fail("Aliases must contain at most 500 characters.")
  }
  # simplifyVector=FALSE keeps JSON arrays as lists; accept only string elements.
  for (key in c("hidden_edges", "hidden_nodes")) {
    hidden <- state[[key]]
    ids <- if (key == "hidden_edges") edges else nodes
    if (is.list(hidden)) {
      if (!is.null(names(hidden)) || any(!vapply(hidden, function(x)
          is.character(x) && length(x) == 1L && !is.na(x), logical(1))))
        fail(paste(key, "must be an array of IDs."))
      hidden <- unlist(hidden, use.names = FALSE)
      if (!length(hidden)) hidden <- character()
    }
    if (!is.character(hidden) || anyNA(hidden) || anyDuplicated(hidden) || any(!hidden %in% ids))
      fail(paste(key, "must contain unique existing IDs."))
    state[[key]] <- unname(as.list(hidden))
  }
  fields(state$views, c("full", "simplified"))
  for (view in c("full", "simplified")) {
    v <- state$views[[view]]
    fields(v, c("nodes", "edges", "viewport"))
    dictionary(v$nodes, nodes, all = TRUE)
    for (p in v$nodes) {
      fields(p, c("x", "y"))
      if (!number(p$x) || !number(p$y)) fail("Node positions must be finite coordinates within +/- 1000000.")
    }
    dictionary(v$edges, edges)
    for (edge_id in names(v$edges)) {
      e <- v$edges[[edge_id]]
      legacy <- c("curve", "label_dx", "label_dy")
      fields(e, c(legacy, "residual_angle", "residual_offset", "covariance_side",
        "line_type", "bend_offset"), character())
      if ("line_type" %in% names(e) &&
          (edge_types[[edge_id]] == "variance" || !is.character(e$line_type) ||
           length(e$line_type) != 1L || is.na(e$line_type) ||
           !e$line_type %in% c("straight", "curve", "polyline")))
        fail("line_type must be straight/curve/polyline on a non-variance edge.")
      if ("bend_offset" %in% names(e) &&
          (!identical(e$line_type, "polyline") || !number(e$bend_offset)))
        fail("bend_offset must be a finite coordinate within +/- 1000000 on a polyline.")
      if ("covariance_side" %in% names(e) &&
          (edge_types[[edge_id]] != "covariance" || !is.character(e$covariance_side) ||
           length(e$covariance_side) != 1L || is.na(e$covariance_side) ||
           !e$covariance_side %in% c("left", "right", "top", "bottom")))
        fail("covariance_side must be left/right/top/bottom on a covariance edge.")
      if (any(!vapply(e[intersect(names(e), legacy)], number, logical(1))))
        fail("Invalid curve or label position.")
      orbit <- intersect(names(e), c("residual_angle", "residual_offset"))
      if (length(orbit) && edge_types[[edge_id]] != "variance")
        fail("Residual orbit fields are only valid for variance edges.")
      if ("residual_offset" %in% names(e) && !"residual_angle" %in% names(e))
        fail("residual_offset requires residual_angle.")
      if ("residual_angle" %in% names(e)) {
        if (!number(e$residual_angle, -180, 180))
          fail("residual_angle must be a finite numeric angle between -180 and 180 degrees.")
        if (!"residual_offset" %in% names(e)) e$residual_offset <- 0
        if (!number(e$residual_offset, -.95, .95))
          fail("residual_offset must be a finite numeric fraction between -0.95 and 0.95.")
      }
      v$edges[[edge_id]] <- e
    }
    fields(v$viewport, c("x", "y", "k"))
    if (!number(v$viewport$x) || !number(v$viewport$y) || !number(v$viewport$k, 0.1, 10))
      fail("Invalid viewport (zoom must be between 0.1 and 10).")
    if (!length(v$edges)) v$edges <- .mplus_diagram_object()
    state$views[[view]] <- v
  }
  if (!length(state$aliases)) state$aliases <- .mplus_diagram_object()
  config$state <- state
  config
}

.mplus_diagram_read_config <- function(path, graph, defaults = list()) {
  if (is.na(file.info(path)$size) || file.info(path)$size > 5 * 1024^2)
    .mplus_diagram_abort("Configuration is missing or exceeds 5 MB.", "config")
  .mplus_diagram_validate_options(defaults)
  json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  # Capture actual saved keys before migration. A newly introduced option absent
  # from an old file must not masquerade as a saved preference above the user's
  # project defaults. Validation still fills all keys for editor/write callers.
  config <- .mplus_diagram_validate_config(json, graph)
  absent <- setdiff(c("stars", "nonsignificant_dotted", "nonsignificant_hide_values"), names(json$state$options))
  for (key in intersect(absent, names(defaults))) config$state$options[[key]] <- defaults[[key]]
  config
}

.mplus_diagram_write_config <- function(config, path, graph) {
  config <- .mplus_diagram_validate_config(config, graph)
  # Source describes this import; imported metadata is never an authority for
  # result values, the destination filename, or the model's current provenance.
  config$source <- .mplus_diagram_source(graph)
  if (is.null(path) || !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path))
    .mplus_diagram_abort("Supply an explicit configuration filename before saving an object without a reliable .out source.", "config")
  if (!grepl("\\.json$", path, ignore.case = TRUE))
    .mplus_diagram_abort("Configuration targets must end in .json; never overwrite a Mplus output.", "config")
  source <- graph$provenance$path
  if (!is.null(source)) {
    resolved <- normalizePath(path, winslash = "/", mustWork = FALSE)
    original <- normalizePath(source, winslash = "/", mustWork = FALSE)
    if (.Platform$OS.type == "windows") { resolved <- tolower(resolved); original <- tolower(original) }
    if (identical(resolved, original)) .mplus_diagram_abort("Configuration target resolves to the original Mplus output; choose a separate JSON file.", "config")
  }
  parent <- dirname(path)
  if (!dir.exists(parent) || dir.exists(path)) .mplus_diagram_abort("Configuration target must be a file in an existing directory.", "config")
  temporary <- tempfile(pattern = ".mplus-diagram-", tmpdir = parent, fileext = ".json")
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(config, temporary, auto_unbox = TRUE, pretty = TRUE,
    null = "null", na = "null", digits = NA)
  .mplus_diagram_read_config(temporary, graph)
  if (!file.rename(temporary, path)) .mplus_diagram_abort(
    "Could not replace configuration. The previous file is unchanged; your edits remain available.", "config")
  invisible(config)
}
