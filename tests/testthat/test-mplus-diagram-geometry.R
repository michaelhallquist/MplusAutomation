skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

# Geometry acceptance for the real six-observed-variable path fixture. All
# nodes in this fixture have equal rectangle dimensions, so straight-path
# endpoint trimming preserves the midpoint used by the SVG coefficient label.
test_that("crossing path coefficients have distinct deterministic readable positions", {
  path <- system.file("extdata", "mplus-diagram", "path-ml.out", package = "MplusAutomation")
  if (!nzchar(path)) path <- test_path("..", "..", "inst", "extdata", "mplus-diagram", "path-ml.out")
  widget <- mplus_diagram(path)
  graph <- widget$x$model
  layout <- widget$x$automatic$full
  regressions <- Filter(function(edge) edge$type == "regression", graph$structure$edges)
  centers <- lapply(regressions, function(edge) {
    from <- unlist(layout$nodes[[edge$from]], use.names = FALSE)
    to <- unlist(layout$nodes[[edge$to]], use.names = FALSE)
    offset <- layout$edges[[edge$id]]
    c((from[1] + to[1]) / 2 + if (is.null(offset$label_dx)) 0 else offset$label_dx,
      (from[2] + to[2]) / 2 + if (is.null(offset$label_dy)) 0 else offset$label_dy)
  })
  # Nine regression coefficients should be separately readable, including the
  # two former exact collisions. Leave room for signs and two decimal places.
  expect_length(centers, 9)
  pairs <- utils::combn(seq_along(centers), 2, simplify = FALSE)
  for (pair in pairs) {
    separation <- abs(centers[[pair[1]]] - centers[[pair[2]]])
    expect_true(separation[1] >= 64 || separation[2] >= 26)
  }
  for (id in c("regression:X1:Y2", "regression:X2:Y1", "regression:X2:Y2", "regression:X3:Y1"))
    expect_true(length(layout$edges[[id]]) > 0)
  model <- MplusAutomation::readModels(path, quiet = TRUE)
  model$parameters$unstandardized <- model$parameters$unstandardized[rev(seq_len(nrow(model$parameters$unstandardized))), ]
  reordered <- mplus_diagram(model)
  expect_identical(reordered$x$automatic, widget$x$automatic)
  expect_identical(reordered$x$model$results, graph$results)
})

test_that("collision avoidance never replaces deliberately saved label positions", {
  path <- system.file("extdata", "mplus-diagram", "path-ml.out", package = "MplusAutomation")
  if (!nzchar(path)) path <- test_path("..", "..", "inst", "extdata", "mplus-diagram", "path-ml.out")
  baseline <- mplus_diagram(path)
  saved <- .mplus_diagram_config(baseline)
  saved$state$views$full$edges[["regression:X1:Y2"]] <- list(label_dx = 0, label_dy = 0)
  target <- tempfile(fileext = ".json")
  on.exit(unlink(target), add = TRUE)
  .mplus_diagram_write_config(saved, target, baseline$x$model)
  current <- mplus_diagram(path, config = target)
  expect_equal(current$x$state$views$full$edges[["regression:X1:Y2"]], list(label_dx = 0, label_dy = 0))
  expect_identical(current$x$model$results, baseline$x$model$results)
  expect_identical(current$x$model$fingerprint, baseline$x$model$fingerprint)
})
