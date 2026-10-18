#!/usr/bin/env Rscript
# Actual installed-package fresh-R-process integration check. No engine/network.
# Rscript tests/browser/workflow-check.R [repository] [artifact-directory]
# This script must never intentionally install into or clean the default library.
args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(if (length(args)) args[[1]] else ".", mustWork = TRUE)
artifacts <- if (length(args) > 1L) args[[2]] else tempfile("mplus-diagram-workflow-")
dir.create(artifacts, recursive = TRUE, showWarnings = FALSE)
artifacts <- normalizePath(artifacts, mustWork = TRUE)
library_dir <- file.path(artifacts, "library")
dir.create(library_dir, showWarnings = FALSE)
install_log <- file.path(artifacts, "install.log")
status <- system2(file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-multiarch",
    shQuote(paste0("--library=", library_dir)), shQuote(repo)),
  stdout = install_log, stderr = install_log)
if (status != 0L) stop(paste(readLines(install_log, warn = FALSE), collapse = "\n"))
# Preparation is a separate process, so the render process cannot reuse a
# widget, an open Viewer, parser state, or a previously loaded namespace.
prepare <- file.path(artifacts, "prepare.R")
writeLines(c(
  "args <- commandArgs(TRUE)",
  ".libPaths(c(args[[1]], .libPaths())); library(MplusAutomation)",
  "stopifnot(normalizePath(find.package('MplusAutomation')) == normalizePath(file.path(args[[1]], 'MplusAutomation')))",
  "out <- system.file('extdata', 'mplus-diagram', 'cfa-ml.out', package='MplusAutomation')",
  "w <- mplus_diagram(out)",
  "cfg <- list(schema_version=1L, fingerprint=w$x$model$fingerprint, layout_version=1L, state=w$x$state)",
  "cfg$state$aliases[['node:F1']] <- '研究构念甲'",
  "cfg$state$views$full$nodes[['node:F1']]$x <- 631",
  "cfg$state$options$digits <- 3L",
  "jsonlite::write_json(cfg, args[[2]], auto_unbox=TRUE, pretty=TRUE, null='null', digits=NA)"
), prepare, useBytes = TRUE)
config <- file.path(artifacts, "saved.diagram.json")
rscript <- file.path(R.home("bin"), "Rscript")
run <- function(script, values, log) {
  result <- system2(rscript, c("--vanilla", shQuote(script), vapply(values, shQuote, character(1))),
                    stdout = log, stderr = log)
  if (result != 0L) stop(paste(readLines(log, warn = FALSE), collapse = "\n"))
}
run(prepare, c(library_dir, config), file.path(artifacts, "prepare.log"))
before <- readBin(config, "raw", file.info(config)$size)
render <- file.path(artifacts, "render.R")
writeLines(c(
  "args <- commandArgs(TRUE)",
  ".libPaths(c(args[[1]], .libPaths()))",
  "stopifnot(!interactive(), !'MplusAutomation' %in% loadedNamespaces())",
  "options(viewer=function(...) stop('Unexpected Viewer'), browser=function(...) stop('Unexpected browser'))",
  "rmarkdown::render(args[[2]], output_file='workflow.html', output_dir=args[[3]],",
  "  params=list(config_file=args[[4]]), envir=new.env(parent=globalenv()), quiet=TRUE)",
  "stopifnot(normalizePath(find.package('MplusAutomation')) == normalizePath(file.path(args[[1]], 'MplusAutomation')))",
  "writeLines(capture.output(sessionInfo()), file.path(args[[3]], 'session-info.txt'))",
  "jsonlite::write_json(list(interactive=interactive(), shiny_loaded='shiny' %in% loadedNamespaces(),",
  "  loaded_package=find.package('MplusAutomation'), model_widgets=2), file.path(args[[3]], 'render-process.json'), auto_unbox=TRUE)"
), render)
input <- file.path(artifacts, "workflow.Rmd")
file.copy(file.path(repo, "tests", "browser", "workflow.Rmd"), input, overwrite = TRUE)
run(render, c(library_dir, input, artifacts, config), file.path(artifacts, "render.log"))
stopifnot(identical(before, readBin(config, "raw", file.info(config)$size)))
html <- paste(readLines(file.path(artifacts, "workflow.html"), warn = FALSE), collapse = "\n")
stopifnot(lengths(regmatches(html, gregexpr('class="mplus_diagram html-widget', html, fixed = TRUE))) == 2L)
# Self-contained assets have no external src/href for scripts or stylesheets.
stopifnot(!grepl('<script[^>]+src=["\x27](https?:)?//', html, perl = TRUE),
          !grepl('<link[^>]+href=["\x27](https?:)?//', html, perl = TRUE))
cat("Installed-package fresh-process render passed; saved configuration unchanged.\n")
cat(file.path(artifacts, "workflow.html"), "\n")
