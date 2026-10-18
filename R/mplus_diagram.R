#' Interactive diagrams of existing Mplus results
#'
#' Build a self-contained htmlwidget from one continuous, single-group,
#' single-level ML/MLR analysis. Rendering never starts an editing service or
#' writes configuration. The diagram uses complete MplusAutomation results,
#' not formatted coefficient tables.
#'
#' @param model An existing .out filename, a single MplusAutomation model,
#'   or a directory/list with explicit \code{select}.
#' @param action \code{"view"} (default) or interactive \code{"edit"}.
#' @param select One exact name or positive integer selecting a model from a
#'   directory/list. Required even when that container has only one model.
#' @param mapping Optional named list with \code{family}, \code{waves},
#'   \code{constructs}, and/or \code{roles}; mappings use original variable names.
#' @param config Configuration JSON filename. Defaults to
#'   \code{<output-stem>.diagram.json} when a reliable source file is available.
#' @param options Explicit display options overriding saved configuration.
#'   Options: \code{scale} (\code{"stdyx"}/\code{"unstandardized"}),
#'   \code{digits} (0--6), \code{view} (\code{"full"}/\code{"simplified"}),
#'   and logical \code{residuals}, \code{default_covariances},
#'   \code{parameter_labels}, \code{values}, \code{fixed_values},
#'   \code{constraints}, \code{stars}, \code{nonsignificant_dotted}, and
#'   \code{nonsignificant_hide_values}. The last three default to true.
#'   Nonsignificant paths default to dotted lines without labels. Set
#'   \code{nonsignificant_hide_values = FALSE} to retain labels on dotted paths,
#'   or \code{nonsignificant_dotted = FALSE} for solid paths with labels.
#'   The toolbar checkbox cycles through these three states; significance
#'   display never removes paths. Stars and line style use the displayed scale's p value;
#'   original-scale fixed parameters are not tested on the original scale.
#' @param defaults Explicit project default options, used below saved options.
#' @param width,height Widget dimensions passed to htmlwidgets.
#'
#' @return An htmlwidget. In interactive edit mode, saving validates and writes
#'   presentation state before returning; cancelling returns the initial widget.
#' @details Display names never change statistical identities. Configuration
#'   stores presentation only; new estimates always come from \code{model}.
#'   In edit mode, Remove object removes selected nodes and all incident paths
#'   and labels, or a selected path, from both views. Undo restores the last
#'   change; Restore automatic layout restores removed objects in both views
#'   and resets the current view's layout. Removals are saved in configuration.
#'   The Path selector can select overlapping relationships. Line type switches
#'   non-variance paths between Straight, Curve, and Polyline. Drag a curve's
#'   handle to bend it, or a polyline's handle/middle segment to move its two-corner
#'   routing lane. Changing type recenters the label. Types and routing offsets
#'   are saved independently for each view; old saved curves remain compatible.
#'   Routing is manual; it does not automatically avoid all intervening nodes.
#'   Missing STDYX triggers a visible whole-diagram fallback. Offline HTML
#'   downloads do not overwrite the R project; use the downloaded file as
#'   \code{config} on a subsequent call. Unsupported or ambiguous structures
#'   raise an error rather than showing a partial fitted model. The model object
#'   must retain the echoed input, complete parameter tables, scope summary and
#'   raw output needed to verify estimation termination. Re-read a selectively
#'   extracted object with \code{readModels(out)}. A STDYX table
#'   without any finite coefficient is treated as unavailable. A stored relative
#'   filename in a model object does not determine a configuration location;
#'   pass the output filename directly, read it with \code{normalizePath()},
#'   or provide an explicit \code{config} filename.
#'   Saved configurations include source metadata and an SHA256 content ID of
#'   imported output/results, not an engine-issued run UUID. Source metadata
#'   never replaces current estimates or chooses the save destination. Older
#'   version-1 configurations without this metadata remain compatible.
#' @author Chaopai Lin
#' @examples
#' if (requireNamespace("htmlwidgets", quietly = TRUE) &&
#'     requireNamespace("jsonlite", quietly = TRUE)) {
#'   out <- system.file("extdata", "mplus-diagram", "cfa-ml.out",
#'                      package = "MplusAutomation")
#'   diagram <- mplus_diagram(out)
#'   diagram
#' }
#' \dontrun{
#' htmlwidgets::saveWidget(diagram, "model.html", selfcontained = TRUE)
#' # Explicit interactive edit (requires shiny):
#' mplus_diagram(out, action = "edit", config = "my-model.diagram.json")
#' }
#' @export
mplus_diagram <- function(model, action = c("view", "edit"), select = NULL,
                          mapping = NULL, config = NULL, options = list(),
                          defaults = list(), width = NULL, height = NULL) {
  for (pkg in c("htmlwidgets", "jsonlite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Install the optional '%s' package to use mplus_diagram().", pkg), call. = FALSE)
    }
  }
  action <- match.arg(action)
  if (action == "edit" && (!interactive() || isTRUE(getOption("knitr.in.progress")))) {
    .mplus_diagram_abort(paste("Editing requires an interactive R session.",
      "Save configuration there first, then render with action = 'view'."), "edit")
  }
  .mplus_diagram_validate_options(defaults)
  .mplus_diagram_validate_options(options)
  if (!is.null(config) && (!is.character(config) || length(config) != 1L ||
                          is.na(config) || !nzchar(config) || !grepl("\\.json$", config, ignore.case = TRUE))) {
    .mplus_diagram_abort("config must be one nonempty filename ending in .json.", "config")
  }
  input <- .mplus_diagram_input(model, select)
  graph <- .mplus_diagram_parse(input$model, mapping)
  graph$provenance$path <- input$path
  for (key in c("title", "estimator", "engine"))
    graph$provenance[key] <- .mplus_diagram_source(graph)[key]
  # This content identifier describes the imported output/results snapshot. It
  # is not a Mplus engine UUID and is independent of presentation or disk path.
  graph$provenance$result_id <- .mplus_diagram_result_id(input$model[["output"]], graph$results)
  source_name <- input$path
  if (is.null(source_name)) source_name <- attr(input$model, "filename", exact = TRUE)
  if (!is.character(source_name) || length(source_name) != 1L || is.na(source_name) || !nzchar(source_name))
    source_name <- input$model$summaries$Filename
  graph$provenance$output <- if (is.character(source_name) && length(source_name) == 1L &&
    !is.na(source_name) && nzchar(source_name)) basename(gsub("\\", "/", source_name, fixed = TRUE)) else NULL
  if (length(input$read_warnings)) {
    graph$diagnostics <- c(graph$diagnostics, lapply(unique(input$read_warnings),
      function(msg) list(severity = "warning", code = "reader_warning",
                         message = paste("MplusAutomation reader:", msg))))
  }
  graph$fingerprint <- .mplus_diagram_fingerprint(graph$structure)
  automatic <- .mplus_diagram_layout(graph)
  state <- list(options = utils::modifyList(.mplus_diagram_defaults(), defaults),
    aliases = .mplus_diagram_object(), hidden_edges = list(), hidden_nodes = list(), views = automatic)
  target <- config
  if (is.null(target) && !is.null(input$path)) {
    target <- paste0(tools::file_path_sans_ext(input$path), ".diagram.json")
  }
  if (!is.null(target) && file.exists(target)) {
    saved <- tryCatch(.mplus_diagram_read_config(target, graph, defaults = defaults), error = identity)
    if (inherits(saved, "error")) {
      msg <- paste("Saved diagram configuration was not applied:", conditionMessage(saved),
                   "The original file is unchanged; using automatic layout.")
      .mplus_diagram_warn(msg, "config")
      graph$diagnostics <- c(graph$diagnostics,
        list(list(severity = "warning", code = "config", message = msg)))
    } else {
      state <- saved$state
      state$options <- utils::modifyList(
        utils::modifyList(.mplus_diagram_defaults(), defaults), state$options)
    }
  }
  state$options <- utils::modifyList(state$options, options)
  available <- any(vapply(graph$results, function(r) {
    estimate <- r$stdyx$estimate
    is.numeric(estimate) && length(estimate) == 1L && is.finite(estimate)
  }, logical(1)))
  if (state$options$scale == "stdyx" && !available) {
    state$options$scale <- "unstandardized"
    msg <- "STDYX is unavailable; displaying unstandardized estimates."
    .mplus_diagram_warn(msg, "results")
    if (!any(vapply(graph$diagnostics, function(d) identical(d$code, "missing_stdyx"), logical(1)))) {
      graph$diagnostics <- c(graph$diagnostics,
        list(list(severity = "warning", code = "missing_stdyx", message = msg)))
    }
  }
  widget <- htmlwidgets::createWidget("mplus_diagram",
    x = list(schema_version = 1L, model = graph, state = state,
             automatic = automatic, editor = NULL),
    width = width, height = height, package = "MplusAutomation",
    sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth = 960,
      defaultHeight = 680, browser.fill = TRUE, viewer.fill = TRUE, padding = 0))
  if (action == "edit") return(.mplus_diagram_edit(widget, target))
  widget
}

.mplus_diagram_abort <- function(message, code = "input") {
  stop(structure(list(message = message, call = NULL),
    class = c(paste0("mplus_diagram_", code, "_error"), "mplus_diagram_error", "error", "condition")))
}

.mplus_diagram_warn <- function(message, code = "input") {
  warning(structure(list(message = message, call = NULL),
    class = c(paste0("mplus_diagram_", code, "_warning"), "mplus_diagram_warning", "warning", "condition")))
}

.mplus_diagram_input <- function(model, select = NULL) {
  container <- is.list(model) && !inherits(model, "mplus.model")
  directory <- is.character(model) && length(model) == 1L && !is.na(model) && dir.exists(model)
  if (container || directory) {
    if (is.null(select)) .mplus_diagram_abort("A directory/model list requires explicit select; no model is chosen automatically.")
    if (directory) {
      paths <- sort(list.files(model, pattern = "\\.out$", full.names = TRUE, ignore.case = TRUE))
      model <- stats::setNames(as.list(paths), basename(paths))
    }
    if (is.character(select) && length(select) == 1L && !is.na(select)) {
      idx <- which(names(model) == select)
      if (length(idx) != 1L) .mplus_diagram_abort("select must match exactly one model name.")
    } else if (is.numeric(select) && length(select) == 1L && is.finite(select) &&
               select == floor(select) && select >= 1 && select <= length(model)) {
      idx <- as.integer(select)
    } else .mplus_diagram_abort("select must be one existing model name or positive index.")
    return(.mplus_diagram_input(model[[idx]]))
  }
  if (!is.null(select)) .mplus_diagram_abort("select is only used for a directory or model list.")
  path <- NULL
  read_warnings <- character()
  if (is.character(model) && length(model) == 1L && !is.na(model)) {
    if (!file.exists(model) || !grepl("\\.out$", model, ignore.case = TRUE)) {
      .mplus_diagram_abort("model must name an existing .out file.")
    }
    path <- normalizePath(model, winslash = "/", mustWork = TRUE)
    model <- withCallingHandlers(readModels(path, quiet = TRUE),
      warning = function(w) {
        read_warnings <<- c(read_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
  }
  if (!inherits(model, "mplus.model")) .mplus_diagram_abort("Supply a single MplusAutomation mplus.model object or .out filename.")
  if (is.null(path)) {
    source <- attr(model, "filename", exact = TRUE)
    if (is.character(source) && length(source) == 1L && !is.na(source)) {
      source <- path.expand(source)
      # A stored relative filename has no record of the directory in which
      # readModels ran. A later working directory may contain another model
      # with the same name; it must not supply that project's configuration.
      absolute <- startsWith(source, "/") || (.Platform$OS.type == "windows" &&
        (grepl("^[A-Za-z]:[/\\\\]", source, perl = TRUE) || startsWith(source, "\\\\")))
      if (absolute && grepl("\\.out$", source, ignore.case = TRUE) && file.exists(source))
        path <- normalizePath(source, winslash = "/", mustWork = TRUE)
    }
  }
  list(model = model, path = path, read_warnings = read_warnings)
}

.mplus_diagram_defaults <- function() {
  list(scale = "stdyx", digits = 2L, residuals = FALSE,
       default_covariances = FALSE, parameter_labels = FALSE, values = TRUE,
       fixed_values = TRUE, constraints = FALSE, view = "full",
       stars = TRUE, nonsignificant_dotted = TRUE, nonsignificant_hide_values = TRUE)
}

.mplus_diagram_validate_options <- function(x, complete = FALSE) {
  expected <- names(.mplus_diagram_defaults())
  if (!is.list(x) || (length(x) && (is.null(names(x)) || anyNA(names(x)) ||
      anyDuplicated(names(x)) || any(!names(x) %in% expected))) ||
      (complete && !setequal(names(x), expected))) {
    .mplus_diagram_abort("Display options must be a named list of supported option names.", "config")
  }
  for (key in names(x)) {
    val <- x[[key]]
    valid <- if (key == "scale") is.character(val) && length(val) == 1L && !is.na(val) && val %in% c("stdyx", "unstandardized")
    else if (key == "view") is.character(val) && length(val) == 1L && !is.na(val) && val %in% c("full", "simplified")
    else if (key == "digits") is.numeric(val) && length(val) == 1L && is.finite(val) && val == floor(val) && val >= 0 && val <= 6
    else is.logical(val) && length(val) == 1L && !is.na(val)
    if (!valid) .mplus_diagram_abort(paste("Invalid display option:", key), "config")
  }
  invisible(x)
}
