skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

test_that("covariance connection sides persist without changing the model", {
  baseline <- mplus_diagram(system.file("extdata/mplus-diagram/cfa-ml.out", package = "MplusAutomation"))
  id <- "covariance:F1:F2"
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  for (side in c("left", "right", "top", "bottom")) {
    config <- .mplus_diagram_config(baseline)
    config$state$views$full$edges[[id]] <- list(curve = 73, covariance_side = side)
    .mplus_diagram_write_config(config, path, baseline$x$model)
    restored <- mplus_diagram(baseline$x$model$provenance$path, config = path)
    expect_equal(restored$x$state$views$full$edges[[id]], list(curve = 73, covariance_side = side))
    expect_equal(restored$x$state$views$simplified, baseline$x$state$views$simplified, tolerance = 0)
    expect_identical(restored$x$model, baseline$x$model)
  }
  previous <- readBin(path, "raw", file.info(path)$size)
  for (side in list("center", "LEFT", 1, TRUE, NA_character_, c("left", "right"), NULL)) {
    config <- .mplus_diagram_config(baseline)
    config$state$views$full$edges[[id]] <- list(covariance_side = side)
    expect_error(.mplus_diagram_write_config(config, path, baseline$x$model), "covariance_side")
    expect_identical(readBin(path, "raw", file.info(path)$size), previous)
  }
  for (other in c("variance:F1:F1", "loading:F1:Y1")) {
    config <- .mplus_diagram_config(baseline)
    config$state$views$full$edges[[other]] <- list(covariance_side = "left")
    expect_error(.mplus_diagram_write_config(config, path, baseline$x$model), "covariance_side")
  }
})
