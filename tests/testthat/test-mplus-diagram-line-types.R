skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

test_that("line types round-trip independently in each view without changing results", {
  model <- system.file("extdata/mplus-diagram/mediation-ml.out", package = "MplusAutomation")
  if (!nzchar(model)) model <- system.file("extdata/mplus-diagram/path-ml.out", package = "MplusAutomation")
  baseline <- mplus_diagram(model)
  id <- Filter(function(e) e$type == "regression", baseline$x$model$structure$edges)[[1]]$id
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  for (override in list(list(line_type = "straight"), list(line_type = "curve", curve = -160),
                       list(line_type = "polyline", bend_offset = -110), list(curve = 75))) {
    config <- .mplus_diagram_config(baseline)
    config$state$views$full$edges[[id]] <- override
    .mplus_diagram_write_config(config, path, baseline$x$model)
    restored <- mplus_diagram(model, config = path)
    expect_equal(restored$x$state$views$full$edges[[id]], override, tolerance = 0)
    expect_equal(restored$x$state$views$simplified, baseline$x$state$views$simplified, tolerance = 0)
    expect_identical(restored$x$model, baseline$x$model)
  }
  before <- readBin(path, "raw", file.info(path)$size)
  for (override in list(list(line_type = "spline"), list(line_type = NULL), list(line_type = 1),
                       list(line_type = NA_character_), list(line_type = c("curve", "straight")),
                       list(bend_offset = 100), list(line_type = "curve", bend_offset = 100),
                       list(line_type = "polyline", bend_offset = Inf),
                       list(line_type = "polyline", bend_offset = 1e7),
                       list(line_type = "polyline", bend_offset = "100"))) {
    config <- .mplus_diagram_config(baseline)
    config$state$views$full$edges[[id]] <- override
    expect_error(.mplus_diagram_write_config(config, path, baseline$x$model), "line_type|bend_offset")
    expect_identical(readBin(path, "raw", file.info(path)$size), before)
  }
  variance <- Filter(function(e) e$type == "variance", baseline$x$model$structure$edges)[[1]]$id
  config <- .mplus_diagram_config(baseline)
  config$state$views$full$edges[[variance]] <- list(line_type = "straight")
  expect_error(.mplus_diagram_validate_config(config, baseline$x$model), "non-variance")
})
