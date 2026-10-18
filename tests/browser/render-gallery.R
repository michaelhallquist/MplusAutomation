# Generate real-model browser fixtures in a fresh R process from the repo root.
# Uses the installed package; never installs or source-loads a replacement.
library(MplusAutomation)
repo <- normalizePath('.', mustWork = TRUE)
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else file.path(tempdir(), 'mplus-diagram-gallery')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out, mustWork = TRUE)
commit <- system2('git', c('rev-parse', 'HEAD'), stdout = TRUE)
# Prove the installed implementation corresponds to the checkout used for review.
reference <- new.env(parent = baseenv())
runtime_files <- c(list.files('R', pattern = '^mplus_diagram.*[.]R$', full.names = TRUE),
  list.files('inst/htmlwidgets', recursive = TRUE, full.names = TRUE))
for (file in list.files('R', pattern = '^mplus_diagram.*[.]R$', full.names = TRUE))
  sys.source(file, envir = reference)
functions <- ls(reference, all.names = TRUE)
functions <- functions[vapply(functions, function(id) is.function(reference[[id]]), logical(1))]
stopifnot(all(vapply(functions, function(id)
  identical(body(reference[[id]]), body(get(id, envir = asNamespace('MplusAutomation')))), logical(1))))
files <- c(cfa = 'cfa-ml.out', sem = 'sem-ml.out', path = 'path-ml.out',
           mediation = 'mediation-ml.out', clpm = 'clpm-ml.out',
           ri_explicit = 'ri-explicit-ml.out', ri_hat = 'ri-hat-ml.out', growth = 'growth-ml.out',
           cfa_residuals = 'cfa-ml.out', clpm_residuals = 'clpm-ml.out')
expected <- jsonlite::read_json('tests/fixtures/mplus-diagram/clpm-ml.expected.json')
panel_map <- list(waves = unlist(expected$waves), constructs = unlist(expected$constructs))
records <- list()
widgets <- list()
for (name in names(files)) {
  mapping <- if (name %in% c('clpm', 'clpm_residuals', 'ri_explicit', 'ri_hat')) panel_map else NULL
  file <- system.file('extdata', 'mplus-diagram', files[[name]], package = 'MplusAutomation')
  stopifnot(nzchar(file))
  residual_comparison <- name %in% c('cfa_residuals', 'clpm_residuals')
  widget <- mplus_diagram(file, mapping = mapping, options = list(residuals = residual_comparison, default_covariances = name == "sem"),
    width = 1200, height = 800)
  widgets[[name]] <- widget
  html <- file.path(out, paste0(name, '.html'))
  htmlwidgets::saveWidget(widget, html, selfcontained = TRUE)
  records[[name]] <- list(html = html, fixture = files[[name]], residuals = residual_comparison,
    engine = readLines(file, n = 1L, warn = FALSE),
    fixture_sha256 = digest::digest(file = file, algo = 'sha256'),
    fingerprint = widget$x$model$fingerprint,
    nodes = length(widget$x$model$structure$nodes), edges = length(widget$x$model$structure$edges))
}
# The gallery uses only local files; its dependencies are generated beside it.
htmltools::save_html(htmltools::tagList(lapply(widgets, htmltools::as.tags)),
  file = file.path(out, 'gallery.html'), libdir = file.path(out, 'gallery_files'))
asset_files <- list.files(system.file('htmlwidgets', package = 'MplusAutomation'), recursive = TRUE, full.names = TRUE)
for (source in runtime_files[startsWith(runtime_files, 'inst/')]) {
  installed <- system.file(sub('^inst/', '', source), package = 'MplusAutomation')
  stopifnot(nzchar(installed), identical(digest::digest(file = source, algo = 'sha256'),
    digest::digest(file = installed, algo = 'sha256')))
}
jsonlite::write_json(list(commit = commit,
  dirty_runtime_files = system2('git', c('diff', '--name-only', 'HEAD', '--', shQuote(runtime_files)), stdout = TRUE),
  runtime_source_hashes = lapply(runtime_files, function(file) list(file = file,
    sha256 = digest::digest(file = file, algo = 'sha256'))), package_path = find.package('MplusAutomation'),
  package_version = as.character(packageVersion('MplusAutomation')),
  mplus_automation_version = as.character(packageVersion('MplusAutomation')), verified_function_bodies = length(functions),
  interactive = interactive(), models = records,
  installed_widget_hashes = lapply(asset_files, function(file) list(file = file,
    sha256 = digest::digest(file = file, algo = 'sha256')))),
  file.path(out, 'review-manifest.json'), auto_unbox = TRUE, pretty = TRUE)
cat(out, '\n')
