# Fresh installed public-API read of a configuration downloaded by the browser.
# Args: isolated library, downloaded CFA configuration, restored HTML output.
args <- commandArgs(TRUE)
stopifnot(length(args) == 3L, !interactive())
.libPaths(c(normalizePath(args[[1]]), .libPaths()))
library(MplusAutomation)
stopifnot(normalizePath(find.package("MplusAutomation")) == normalizePath(file.path(args[[1]], "MplusAutomation")))
options(viewer = function(...) stop("Unexpected Viewer"),
        browser = function(...) stop("Unexpected browser"))
source <- system.file("extdata/mplus-diagram/cfa-ml.out", package = "MplusAutomation")
bytes <- readBin(args[[2]], "raw", file.info(args[[2]])$size)
baseline <- mplus_diagram(source)
restored <- mplus_diagram(source, config = args[[2]])
stopifnot(
  identical(restored$x$model, baseline$x$model),
  identical(restored$x$state$views$full$edges[["covariance:F1:F2"]]$covariance_side, "right"),
  isTRUE(all.equal(restored$x$state$views$simplified, baseline$x$state$views$simplified, tolerance = 0)),
  identical(bytes, readBin(args[[2]], "raw", file.info(args[[2]])$size)),
  !"shiny" %in% loadedNamespaces()
)
htmlwidgets::saveWidget(restored, args[[3]], selfcontained = TRUE)
jsonlite::write_json(list(status = "passed", side = "right",
  fingerprint = restored$x$model$fingerprint, model_unchanged = TRUE,
  other_view_unchanged = TRUE, config_bytes_unchanged = TRUE,
  package_path = find.package("MplusAutomation")), paste0(args[[3]], ".json"),
  auto_unbox = TRUE, pretty = TRUE)
cat("Browser covariance-side configuration restored through public R.\n")
