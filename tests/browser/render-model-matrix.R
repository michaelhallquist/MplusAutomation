#!/usr/bin/env Rscript
# Generate the complete fixed positive corpus using the installed public API.
# Usage: Rscript render-model-matrix.R REPO OUTPUT [EXISTING_ISOLATED_LIBRARY]
# Without the third argument installs only to OUTPUT/library, never the default.
args <- commandArgs(TRUE)
worker <- length(args) && identical(args[[1]], "--worker")
if (worker) args <- args[-1L]
if (length(args) < 2L) stop("Supply repository and output directory")
repo <- normalizePath(args[[1]], mustWork = TRUE)
dir.create(args[[2]], recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(args[[2]], mustWork = TRUE)
lib <- if (length(args) >= 3L) normalizePath(args[[3]], mustWork = TRUE) else file.path(out, "library")
if (!worker) {
  runtime_files <- c(list.files(file.path(repo, "R"), pattern = "^mplus_diagram.*[.]R$", full.names = TRUE),
    list.files(file.path(repo, "inst/htmlwidgets"), full.names = TRUE, recursive = TRUE))
  source_records <- lapply(runtime_files, function(file) list(
    path = substring(file, nchar(repo) + 2L), sha256 = digest::digest(file = file, algo = "sha256")))
  provenance <- list(commit = system2("git", c("-C", shQuote(repo), "rev-parse", "HEAD"), stdout = TRUE),
    origin = if (length(args) < 3L) "isolated_install_from_checkout" else "existing_isolated_library",
    checkout_runtime_files = source_records,
    note = "Both modes verify installed diagram function bodies and widget assets against the checkout")
  jsonlite::write_json(provenance, file.path(out, "runtime-source.json"), auto_unbox = TRUE, pretty = TRUE)
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  if (length(args) < 3L) {
    log <- file.path(out, "install.log")
    code <- system2(file.path(R.home("bin"), "R"),
      c("CMD", "INSTALL", "--no-multiarch", shQuote(paste0("--library=", lib)), shQuote(repo)),
      stdout = log, stderr = log)
    if (code != 0L) stop(paste(readLines(log, warn = FALSE), collapse = "\n"))
    stopifnot(identical(source_records, lapply(runtime_files, function(file) list(
      path = substring(file, nchar(repo) + 2L), sha256 = digest::digest(file = file, algo = "sha256")))))
  }
  script <- file.path(repo, "tests", "browser", "render-model-matrix.R")
  code <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script), "--worker", shQuote(repo), shQuote(out), shQuote(lib)),
    stdout = file.path(out, "render.log"), stderr = file.path(out, "render.log"))
  if (code != 0L) stop(paste(readLines(file.path(out, "render.log"), warn = FALSE), collapse = "\n"))
  cat(file.path(out, "manifest.json"), "\n")
  quit(status = 0L)
}
stopifnot(!interactive(), !"MplusAutomation" %in% loadedNamespaces())
.libPaths(c(lib, .libPaths()))
library(MplusAutomation)
stopifnot(normalizePath(find.package("MplusAutomation")) == normalizePath(file.path(lib, "MplusAutomation")))
# A reused library must not silently validate a stale implementation.
reference <- new.env(parent = baseenv())
for (file in list.files(file.path(repo, "R"), pattern = "^mplus_diagram.*[.]R$", full.names = TRUE))
  sys.source(file, envir = reference)
functions <- ls(reference, all.names = TRUE)
functions <- functions[vapply(functions, function(id) is.function(reference[[id]]), logical(1))]
if (!all(vapply(functions, function(id)
    identical(body(reference[[id]]), body(get(id, envir = asNamespace("MplusAutomation")))), logical(1))))
  stop("Installed MplusAutomation diagram functions differ from the checkout; rebuild the isolated library.")
for (relative in list.files(file.path(repo, "inst/htmlwidgets"), recursive = TRUE)) {
  installed <- system.file("htmlwidgets", relative, package = "MplusAutomation")
  if (!nzchar(installed) || !identical(
      digest::digest(file = installed, algo = "sha256"),
      digest::digest(file = file.path(repo, "inst/htmlwidgets", relative), algo = "sha256")))
    stop("Installed widget assets differ from the checkout; rebuild the isolated library.")
}

options(viewer = function(...) stop("Unexpected Viewer"), browser = function(...) stop("Unexpected browser"))
corpus <- jsonlite::read_json(file.path(repo, "inst/extdata/mplus-diagram/manifest.json"))
positive <- Filter(function(f) !is.null(f$expected_path), corpus$fixtures)
expected <- lapply(positive, function(f) jsonlite::read_json(file.path(repo, f$expected_path)))
names(expected) <- vapply(positive, `[[`, character(1), "id")
records <- list()
for (fixture in positive) {
  want <- expected[[fixture$id]]
  mapping_source <- fixture$id
  waves <- want$waves; constructs <- want$constructs
  # Sharing a mapping is allowed only with an independently annotated fixture
  # of the identical synthetic dataset and identical observed identities.
  if (is.null(waves) && !is.null(fixture$data_sha256)) {
    candidates <- Filter(function(f) identical(f$data_sha256, fixture$data_sha256) &&
      !is.null(expected[[f$id]]$waves) &&
      setequal(unlist(expected[[f$id]]$observed), unlist(want$observed)), positive)
    if (length(candidates)) {
      mapping_source <- candidates[[1]]$id
      waves <- expected[[mapping_source]]$waves
      constructs <- expected[[mapping_source]]$constructs
    }
  }
  mapping <- if (!is.null(waves)) list(waves = unlist(waves), constructs = unlist(constructs)) else NULL
  # Read installed resources, not a checkout widget or source-loaded namespace.
  relative <- sub("^inst/", "", fixture$path)
  file <- system.file(relative, package = "MplusAutomation")
  stopifnot(nzchar(file), identical(digest::digest(file = file, algo = "sha256"), fixture$sha256))
  warnings <- character()
  widget <- withCallingHandlers(mplus_diagram(file, mapping = mapping,
    options = list(scale = "unstandardized", digits = 3L,
                   default_covariances = TRUE, residuals = FALSE),
    width = 1440, height = 1000), warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  html <- file.path(out, paste0(fixture$id, ".html"))
  htmlwidgets::saveWidget(widget, html, selfcontained = TRUE)
  results <- widget$x$model$results
  records[[length(records) + 1L]] <- list(id = fixture$id, html = html,
    expected = normalizePath(file.path(repo, fixture$expected_path)),
    expected_sha256 = digest::digest(file = file.path(repo, fixture$expected_path), algo = "sha256"),
    source_sha256 = fixture$sha256, estimator = fixture$estimator,
    family = widget$x$model$structure$family,
    nodes = length(widget$x$model$structure$nodes), edges = length(widget$x$model$structure$edges),
    fingerprint = widget$x$model$fingerprint, initial_scale = widget$x$state$options$scale,
    stdyx_available = any(vapply(results, function(r) !is.null(r$stdyx$estimate), logical(1))),
    unstandardized_parameters = sum(vapply(results, function(r) !is.null(r$unstandardized$estimate), logical(1))),
    stdyx_parameters = sum(vapply(results, function(r) !is.null(r$stdyx$estimate), logical(1))),
    mapping_expected = if (is.null(mapping)) NULL else mapping_source,
    source_warnings = warnings)
}
installed_files <- c(list.files(system.file("R", package = "MplusAutomation"), full.names = TRUE),
  list.files(system.file("htmlwidgets", package = "MplusAutomation"), full.names = TRUE, recursive = TRUE))
provenance <- jsonlite::read_json(file.path(out, "runtime-source.json"))
provenance$installed_runtime_files <- lapply(installed_files, function(file) list(
  path = substring(file, nchar(find.package("MplusAutomation")) + 2L), sha256 = digest::digest(file = file, algo = "sha256")))
jsonlite::write_json(list(schema_version = 1L, generated = as.character(Sys.time()),
  runtime_provenance = provenance,
  package_path = find.package("MplusAutomation"), interactive = interactive(),
  shiny_loaded = "shiny" %in% loadedNamespaces(), models = records),
  file.path(out, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))
stopifnot(!"shiny" %in% loadedNamespaces(), length(records) == length(positive))
