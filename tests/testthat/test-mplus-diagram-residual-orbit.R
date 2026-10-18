skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

residual_orbit_widget <- function() {
  path <- system.file("extdata", "mplus-diagram", "cfa-ml.out", package = "MplusAutomation")
  if (!nzchar(path)) path <- test_path("..", "..", "inst", "extdata", "mplus-diagram", "cfa-ml.out")
  mplus_diagram(path)
}

test_that("residual orbit state imports and roundtrips independently in each view", {
  baseline <- residual_orbit_widget()
  source <- baseline$x$model$provenance$path
  config <- .mplus_diagram_config(baseline)
  id <- "variance:Y1:Y1"
  config$state$views$full$edges[[id]] <- list(residual_angle = 137.5, residual_offset = -.4)
  config$state$views$simplified$edges[[id]] <- list(residual_angle = -42)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(config, path, auto_unbox = TRUE, null = "null")
  imported <- mplus_diagram(source, config = path)
  expect_equal(imported$x$state$views$full$edges[[id]], list(residual_angle = 137.5, residual_offset = -.4))
  expect_equal(imported$x$state$views$simplified$edges[[id]], list(residual_angle = -42, residual_offset = 0))
  expect_identical(imported$x$model, baseline$x$model)
  expect_identical(imported$x$model$fingerprint, baseline$x$model$fingerprint)
  expect_identical(imported$x$automatic, baseline$x$automatic)
  .mplus_diagram_write_config(.mplus_diagram_config(imported), path, imported$x$model)
  restored <- mplus_diagram(source, config = path)
  expect_equal(restored$x$state, imported$x$state)
  expect_identical(restored$x$model$provenance, baseline$x$model$provenance)
  stored <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(stored$source, .mplus_diagram_config(baseline)$source)
  expect_false(any(c("results", "estimates", "residual_length") %in% names(stored)))
})

test_that("orbit bounds are inclusive and variance identity is checked", {
  baseline <- residual_orbit_widget()
  config <- .mplus_diagram_config(baseline)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  # Both a true residual variance and an exogenous factor variance are valid
  # variance identities; the renderer retains their different statistical roles.
  for (id in c("variance:Y1:Y1", "variance:F1:F1")) {
    for (angle in c(-180, 0, 180)) {
      for (offset in c(-.95, 0, .95)) {
        config$state$views$full$edges[[id]] <- list(residual_angle = angle, residual_offset = offset)
        jsonlite::write_json(config, path, auto_unbox = TRUE, null = "null")
        widget <- mplus_diagram(baseline$x$model$provenance$path, config = path)
        expect_equal(widget$x$state$views$full$edges[[id]], list(residual_angle = angle, residual_offset = offset))
      }
    }
  }
  for (id in c("loading:F1:Y1", "covariance:F1:F2")) {
    invalid <- .mplus_diagram_config(baseline)
    invalid$state$views$full$edges[[id]] <- list(residual_angle = 45, residual_offset = .2)
    jsonlite::write_json(invalid, path, auto_unbox = TRUE, null = "null")
    expect_warning(widget <- mplus_diagram(baseline$x$model$provenance$path, config = path), "only valid for variance")
    expect_identical(widget$x$state$views, baseline$x$automatic)
  }
})

test_that("invalid orbit values and unknown fields never replace the previous configuration", {
  baseline <- residual_orbit_widget()
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  valid <- .mplus_diagram_config(baseline)
  .mplus_diagram_write_config(valid, path, baseline$x$model)
  original <- readBin(path, "raw", file.info(path)$size)
  bad_overrides <- c(
    lapply(list(-180.1, 180.1, NA_real_, Inf, "90", TRUE, c(0, 1), NULL),
           function(value) list(residual_angle = value)),
    lapply(list(-.951, .951, NA_real_, Inf, "0.5", TRUE, c(0, .5), NULL),
           function(value) list(residual_angle = 90, residual_offset = value)),
    list(list(residual_offset = 0), list(residual_angle = 90, residual_length = 100)))
  for (override in bad_overrides) {
    config <- valid
    config$state$views$full$edges[["variance:Y1:Y1"]] <- override
    expect_error(.mplus_diagram_write_config(config, path, baseline$x$model), class = "mplus_diagram_config_error")
    expect_identical(readBin(path, "raw", file.info(path)$size), original)
  }
  # The public import must reject the same invalid data and preserve its file.
  invalid <- valid
  invalid$state$views$full$edges[["variance:Y1:Y1"]] <- list(residual_offset = .2)
  jsonlite::write_json(invalid, path, auto_unbox = TRUE, null = "null")
  bytes <- readBin(path, "raw", file.info(path)$size)
  expect_warning(mplus_diagram(baseline$x$model$provenance$path, config = path), "requires residual_angle")
  expect_identical(readBin(path, "raw", file.info(path)$size), bytes)
})

test_that("legacy curve and label fields remain readable without inventing orbit state", {
  baseline <- residual_orbit_widget()
  legacy <- .mplus_diagram_config(baseline)
  legacy$source <- NULL
  legacy$state$options$stars <- NULL
  legacy$state$options$nonsignificant_dotted <- NULL
  id <- "variance:Y1:Y1"
  override <- list(curve = 500, label_dx = -20, label_dy = 80)
  legacy$state$views$full$edges[[id]] <- override
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(legacy, path, auto_unbox = TRUE, null = "null")
  imported <- mplus_diagram(baseline$x$model$provenance$path, config = path)
  expect_equal(imported$x$state$views$full$edges[[id]], override)
  expect_null(imported$x$state$views$full$edges[[id]]$residual_angle)
  expect_true(imported$x$state$options$stars)
  expect_identical(imported$x$model, baseline$x$model)
  # Legacy and new fields may coexist in imported data. First orbit editing in
  # the renderer removes legacy offsets; the R reader does not rewrite them.
  legacy$state$views$full$edges[[id]] <- c(override, list(residual_angle = 30, residual_offset = .5))
  jsonlite::write_json(legacy, path, auto_unbox = TRUE, null = "null")
  imported <- mplus_diagram(baseline$x$model$provenance$path, config = path)
  expect_equal(imported$x$state$views$full$edges[[id]], c(override, list(residual_angle = 30, residual_offset = .5)))
})
