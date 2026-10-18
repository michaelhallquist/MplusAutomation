skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

display_model <- function(name) {
  path <- system.file("extdata", "mplus-diagram", paste0(name, ".out"), package = "MplusAutomation")
  if (!nzchar(path)) path <- test_path("..", "..", "inst", "extdata", "mplus-diagram", paste0(name, ".out"))
  MplusAutomation::readModels(path, quiet = TRUE)
}
display_edge <- function(widget, id) {
  edges <- widget$x$model$structure$edges
  edges[[match(id, vapply(edges, `[[`, character(1), "id"))]]
}

test_that("significance display defaults migrate old configs without loosening validation", {
  model <- display_model("cfa-ml")
  baseline <- mplus_diagram(model)
  expect_true(baseline$x$state$options$stars)
  expect_true(baseline$x$state$options$nonsignificant_dotted)
  expect_true(baseline$x$state$options$nonsignificant_hide_values)
  cfg <- .mplus_diagram_config(baseline)
  cfg$state$options$stars <- NULL
  cfg$state$options$nonsignificant_dotted <- NULL
  cfg$state$options$nonsignificant_hide_values <- NULL
  cfg$state$options$digits <- 4L
  cfg$state$aliases[["node:F1"]] <- "原布局"
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write <- function(x) jsonlite::write_json(x, path, auto_unbox = TRUE, null = "null")
  write(cfg)
  migrated <- mplus_diagram(model, config = path)
  expect_true(migrated$x$state$options$stars)
  expect_true(migrated$x$state$options$nonsignificant_dotted)
  expect_true(migrated$x$state$options$nonsignificant_hide_values)
  expect_equal(migrated$x$state$options$digits, 4)
  expect_equal(migrated$x$state$aliases[["node:F1"]], "原布局")
  explicit <- mplus_diagram(model, config = path,
    options = list(stars = FALSE, nonsignificant_dotted = FALSE))
  expect_false(explicit$x$state$options$stars)
  expect_false(explicit$x$state$options$nonsignificant_dotted)
  expect_identical(explicit$x$model$results, baseline$x$model$results)
  expect_identical(explicit$x$model$fingerprint, baseline$x$model$fingerprint)
  for (bad in list("TRUE", 1, NA, list(TRUE))) {
    expect_error(mplus_diagram(model, options = list(stars = bad)), "stars", class = "mplus_diagram_config_error")
    expect_error(mplus_diagram(model, options = list(nonsignificant_hide_values = bad)),
      "nonsignificant_hide_values", class = "mplus_diagram_config_error")
    invalid <- cfg; invalid$state$options$nonsignificant_dotted <- bad
    write(invalid)
    expect_warning(mplus_diagram(model, config = path), "configuration was not applied")
  }
  invalid <- cfg; invalid$state$options$digits <- NULL
  write(invalid)
  expect_warning(mplus_diagram(model, config = path), "missing required display options")
  invalid <- cfg; invalid$state$options$nonsignificant_dot <- TRUE
  write(invalid)
  expect_warning(mplus_diagram(model, config = path), "supported option")
})

test_that("residual endpoints follow actual endogenous structure across ML and MLR", {
  for (estimator in c("ml", "mlr")) {
    cfa <- mplus_diagram(display_model(paste0("cfa-", estimator)))
    expect_false(display_edge(cfa, "variance:F1:F1")$residual)
    expect_true(display_edge(cfa, "variance:Y1:Y1")$residual)
    expect_false(display_edge(cfa, "covariance:F1:F2")$residual_from)
    expect_false(display_edge(cfa, "covariance:F1:F2")$residual_to)
    expect_false(Filter(function(n) n$id == "node:F1", cfa$x$model$structure$nodes)[[1]]$endogenous)
    expect_true(Filter(function(n) n$id == "node:Y1", cfa$x$model$structure$nodes)[[1]]$endogenous)
    sem <- mplus_diagram(display_model(paste0("sem-", estimator)))
    expect_false(display_edge(sem, "variance:F1:F1")$residual)
    expect_true(display_edge(sem, "variance:F3:F3")$residual)
    clpm <- mplus_diagram(display_model(paste0("clpm-", estimator)))
    expect_false(display_edge(clpm, "covariance:X1:Y1")$residual_from)
    expect_false(display_edge(clpm, "covariance:X1:Y1")$residual_to)
    expect_true(display_edge(clpm, "covariance:X2:Y2")$residual_from)
    expect_true(display_edge(clpm, "covariance:X2:Y2")$residual_to)
  }
})

test_that("RI within components remain substantive nodes when residual display changes", {
  for (estimator in c("ml", "mlr")) {
    for (kind in c("ri-explicit", "ri-hat")) {
      model <- display_model(paste(kind, estimator, sep = "-"))
      hidden <- mplus_diagram(model, options = list(residuals = FALSE))
      shown <- mplus_diagram(model, options = list(residuals = TRUE))
      expect_identical(hidden$x$model, shown$x$model)
      suffix <- if (kind == "ri-hat") "^" else ""
      prefix <- if (kind == "ri-explicit") "W" else ""
      x1 <- paste0(prefix, "X1", suffix); x2 <- paste0(prefix, "X2", suffix)
      y1 <- paste0(prefix, "Y1", suffix); y2 <- paste0(prefix, "Y2", suffix)
      expect_false(display_edge(shown, paste("variance", x1, x1, sep = ":"))$residual)
      expect_true(display_edge(shown, paste("variance", x2, x2, sep = ":"))$residual)
      expect_true(display_edge(shown, "variance:X1:X1")$residual)
      expect_equal(display_edge(shown, "variance:X1:X1")$fixed_value, 0)
      expect_false(display_edge(shown, paste("covariance", x1, y1, sep = ":"))$residual_from)
      expect_true(display_edge(shown, paste("covariance", x2, y2, sep = ":"))$residual_from)
      dynamic <- display_edge(shown, paste("regression", x1, x2, sep = ":"))
      expect_equal(dynamic$from, paste0("node:", x1))
      expect_equal(dynamic$to, paste0("node:", x2))
    }
  }
})

test_that("derived residual metadata does not invalidate structural fingerprints", {
  widget <- mplus_diagram(display_model("ri-hat-ml"))
  structure <- widget$x$model$structure
  structure$nodes <- lapply(structure$nodes, function(node) { node$endogenous <- NULL; node })
  structure$edges <- lapply(structure$edges, function(edge) {
    edge$residual <- NULL; edge$residual_from <- NULL; edge$residual_to <- NULL
    edge
  })
  expect_identical(.mplus_diagram_fingerprint(structure), widget$x$model$fingerprint)
})

test_that("p values stay scale-specific and unknown values cannot imply significance", {
  # Deliberately synthetic numerical probes; original engine fixtures unchanged.
  model <- display_model("path-ml")
  raw <- which(model$parameters$unstandardized$paramHeader == "Y1.ON" & model$parameters$unstandardized$param == "X1")
  std <- which(model$parameters$stdyx.standardized$paramHeader == "Y1.ON" & model$parameters$stdyx.standardized$param == "X1")
  model$parameters$unstandardized$pval[raw] <- .001
  model$parameters$stdyx.standardized$pval[std] <- .8
  widget <- mplus_diagram(model)
  result <- widget$x$model$results[["regression:X1:Y1"]]
  expect_equal(result$unstandardized$p, .001)
  expect_equal(result$stdyx$p, .8)
  for (p in c(NA_real_, NaN, Inf, -1, 1.1, 999)) {
    model$parameters$stdyx.standardized$pval[std] <- p
    result <- mplus_diagram(model)$x$model$results[["regression:X1:Y1"]]
    expect_null(result$stdyx$p)
    expect_equal(result$unstandardized$p, .001)
  }
  for (p in c(0, 1)) {
    model$parameters$stdyx.standardized$pval[std] <- p
    expect_equal(mplus_diagram(model)$x$model$results[["regression:X1:Y1"]]$stdyx$p, p)
  }
  cfa <- mplus_diagram(display_model("cfa-ml"))
  expect_true(display_edge(cfa, "loading:F1:Y1")$fixed)
  expect_null(cfa$x$model$results[["loading:F1:Y1"]]$unstandardized$p)
  # Actual standardized p may exist for transformed fixed loadings. It stays
  # source data. The renderer may use this real STDYX p value, while original-
  # scale fixed parameters remain untested on the original scale.
  expect_equal(cfa$x$model$results[["loading:F1:Y1"]]$stdyx$p, 0)
})

test_that("migration defaults cannot outrank project defaults as invented saved preferences", {
  model <- display_model("cfa-ml")
  widget <- mplus_diagram(model)
  legacy <- .mplus_diagram_config(widget)
  legacy$state$options$stars <- NULL
  legacy$state$options$nonsignificant_dotted <- NULL
  legacy$state$options$nonsignificant_hide_values <- NULL
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write <- function(config) jsonlite::write_json(config, path, auto_unbox = TRUE, null = "null")
  write(legacy)
  defaults <- list(stars = FALSE, nonsignificant_dotted = FALSE, nonsignificant_hide_values = FALSE)
  restored <- mplus_diagram(model, config = path, defaults = defaults)
  expect_false(restored$x$state$options$stars)
  expect_false(restored$x$state$options$nonsignificant_dotted)
  expect_false(restored$x$state$options$nonsignificant_hide_values)
  explicit <- mplus_diagram(model, config = path, defaults = defaults, options = list(stars = TRUE))
  expect_true(explicit$x$state$options$stars)
  expect_false(explicit$x$state$options$nonsignificant_dotted)
  # One genuinely stored new preference wins; only its absent sibling migrates
  # using the project default rather than an invented saved TRUE.
  legacy$state$options$stars <- TRUE
  write(legacy)
  restored <- mplus_diagram(model, config = path, defaults = defaults)
  expect_true(restored$x$state$options$stars)
  expect_false(restored$x$state$options$nonsignificant_dotted)
  expect_false(restored$x$state$options$nonsignificant_hide_values)
  # Editor/write normalization still yields a complete config, with no internal
  # migration flags or attributes persisted to the JSON document.
  .mplus_diagram_write_config(legacy, path, widget$x$model)
  stored <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_true(stored$state$options$stars)
  expect_true(stored$state$options$nonsignificant_dotted)
  expect_setequal(names(stored), c("schema_version", "fingerprint", "layout_version", "state", "source"))
  expect_setequal(names(stored$state$options), names(.mplus_diagram_defaults()))
})
