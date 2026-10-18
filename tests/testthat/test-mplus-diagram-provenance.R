skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

# Snapshot IDs describe imported content, not an engine-issued run UUID.
# Modified result objects/raw text below are explicit synthetic provenance tests.
provenance_fixture <- function() {
  path <- system.file("extdata", "mplus-diagram", "cfa-ml.out", package = "MplusAutomation")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "extdata", "mplus-diagram", "cfa-ml.out")
  path
}
provenance_model <- function() MplusAutomation::readModels(provenance_fixture(), quiet = TRUE)
provenance_write <- function(config, path) {
  jsonlite::write_json(config, path, auto_unbox = TRUE, null = "null", digits = NA)
}

test_that("snapshot identity is stable across source paths, table order and presentation", {
  model <- provenance_model()
  baseline <- mplus_diagram(model)
  id <- baseline$x$model$provenance$result_id
  expect_match(id, "^[a-f0-9]{64}$")
  for (scale in c("unstandardized", "stdyx.standardized")) {
    table <- model$parameters[[scale]]
    model$parameters[[scale]] <- table[rev(seq_len(nrow(table))), , drop = FALSE]
  }
  attr(model, "filename") <- "a-moved-result.out"
  moved <- mplus_diagram(model, options = list(digits = 5, scale = "unstandardized", residuals = TRUE))
  expect_identical(moved$x$model$provenance$result_id, id)
  expect_identical(moved$x$model$provenance$output, "a-moved-result.out")
  expect_null(moved$x$model$provenance$path)
  copied <- tempfile(fileext = ".out")
  on.exit(unlink(copied), add = TRUE)
  expect_true(file.copy(provenance_fixture(), copied))
  expect_identical(mplus_diagram(copied)$x$model$provenance$result_id, id)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  cfg <- .mplus_diagram_config(baseline)
  cfg$state$aliases[["node:F1"]] <- "新显示名"
  cfg$state$views$full$nodes[["node:F1"]]$x <- 901
  provenance_write(cfg, path)
  shown <- mplus_diagram(model, config = path)
  expect_identical(shown$x$model$provenance$result_id, id)
  expect_equal(shown$x$state$views$full$nodes[["node:F1"]]$x, 901)
})

test_that("new imported values or raw output change the snapshot but retain compatible layout", {
  original <- provenance_model()
  baseline <- mplus_diagram(original)
  modified <- original
  # Synthetic new value, not a claim of a second Mplus estimation.
  row <- which(modified$parameters$unstandardized$paramHeader == "F1.BY" & modified$parameters$unstandardized$param == "Y2")
  modified$parameters$unstandardized$est[row] <- 1.234
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  cfg <- .mplus_diagram_config(baseline)
  cfg$state$views$full$nodes[["node:F1"]]$x <- 987
  provenance_write(cfg, path)
  current <- mplus_diagram(modified, config = path)
  expect_false(identical(current$x$model$provenance$result_id, baseline$x$model$provenance$result_id))
  expect_identical(current$x$model$fingerprint, baseline$x$model$fingerprint)
  expect_equal(current$x$state$views$full$nodes[["node:F1"]]$x, 987)
  expect_equal(current$x$model$results[["loading:F1:Y2"]]$unstandardized$estimate, 1.234)
  .mplus_diagram_write_config(cfg, path, current$x$model)
  saved <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(saved$source$result_id, current$x$model$provenance$result_id)
  changed_text <- original
  changed_text$output <- c(changed_text$output, "Synthetic audit marker: raw imported text changed.")
  expect_false(identical(mplus_diagram(changed_text)$x$model$provenance$result_id,
                         baseline$x$model$provenance$result_id))
})

test_that("schema one legacy configs and nullable metadata remain compatible", {
  model <- provenance_model()
  baseline <- mplus_diagram(model)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  legacy <- .mplus_diagram_config(baseline)
  legacy$source <- NULL
  legacy$state$aliases[["node:F1"]] <- "旧配置"
  provenance_write(legacy, path)
  expect_equal(mplus_diagram(model, config = path)$x$state$aliases[["node:F1"]], "旧配置")
  nullable <- legacy
  nullable$source <- stats::setNames(rep(list(NULL), 5), c("title", "output", "estimator", "engine", "result_id"))
  provenance_write(nullable, path)
  expect_equal(mplus_diagram(model, config = path)$x$state$aliases[["node:F1"]], "旧配置")
  .mplus_diagram_write_config(legacy, path, baseline$x$model)
  saved <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_equal(saved$schema_version, 1)
  expect_setequal(names(saved$source), c("title", "output", "estimator", "engine", "result_id"))
  expect_true(all(vapply(saved$source, function(value) is.null(value) || is.character(value), logical(1))))
  expect_false(any(c("results", "estimate", "se", "p", "path") %in% names(saved$source)))
})

test_that("imported source metadata cannot replace current results or redirect persistence", {
  model <- provenance_model()
  baseline <- mplus_diagram(model)
  path <- tempfile(fileext = ".json")
  unintended <- tempfile(fileext = ".json")
  on.exit(unlink(c(path, unintended)), add = TRUE)
  cfg <- .mplus_diagram_config(baseline)
  cfg$source <- list(title = "Another model", output = unintended, estimator = "BAYES",
                     engine = "999", result_id = paste(rep("0", 64), collapse = ""))
  cfg$state$aliases[["node:F1"]] <- "仅显示状态"
  provenance_write(cfg, path)
  current <- mplus_diagram(model, config = path)
  expect_identical(current$x$model$provenance, baseline$x$model$provenance)
  expect_identical(current$x$model$results, baseline$x$model$results)
  expect_equal(current$x$state$aliases[["node:F1"]], "仅显示状态")
  expect_false(file.exists(unintended))
  .mplus_diagram_write_config(cfg, path, current$x$model)
  expect_false(file.exists(unintended))
  saved <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(saved$source, .mplus_diagram_config(current)$source)
})

test_that("source metadata validates strictly without consuming result-like fields", {
  model <- provenance_model()
  baseline <- mplus_diagram(model)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  cfg <- .mplus_diagram_config(baseline)
  invalid <- list(cfg, cfg, cfg, cfg, cfg)
  invalid[[1]]$source$result_id <- "not-a-content-id"
  invalid[[2]]$source$estimate <- 123
  invalid[[3]]$source$title <- 123
  invalid[[4]]$source$engine <- NULL # missing key, unlike an explicit JSON null
  invalid[[5]]$unexpected <- "unknown top-level metadata"
  for (bad in invalid) {
    provenance_write(bad, path)
    before <- readBin(path, "raw", file.info(path)$size)
    expect_warning(result <- mplus_diagram(model, config = path), "configuration was not applied")
    expect_identical(result$x$model$results, baseline$x$model$results)
    expect_identical(readBin(path, "raw", file.info(path)$size), before)
  }
})
