skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

# Public workflow tests use real fixed Mplus results. No engine is run by CI.
diagram_fixture <- function() system.file("extdata", "mplus-diagram", "ex5.11.out", package = "MplusAutomation")

test_that("a fresh read reuses compatible state but always uses current estimates", {
  path <- tempfile(fileext = ".diagram.json")
  on.exit(unlink(path))
  model <- MplusAutomation::readModels(diagram_fixture(), quiet = TRUE)
  w <- suppressWarnings(mplus_diagram(model))
  config <- .mplus_diagram_config(w)
  id <- w$x$model$structure$nodes[[1]]$id
  config$state$aliases[[id]] <- "中文研究变量"
  config$state$views$full$nodes[[id]]$x <- 777
  config$state$options$digits <- 4L
  .mplus_diagram_write_config(config, path, w$x$model)
  before <- readBin(path, "raw", file.info(path)$size)
  model$parameters$unstandardized$est[2] <- 1.234
  model$parameters$unstandardized <- model$parameters$unstandardized[
    rev(seq_len(nrow(model$parameters$unstandardized))), ]
  updated <- suppressWarnings(mplus_diagram(model, config = path, defaults = list(digits = 1)))
  expect_identical(updated$x$model$fingerprint, w$x$model$fingerprint)
  expect_equal(updated$x$state$views$full$nodes[[id]]$x, 777)
  expect_identical(updated$x$state$aliases[[id]], "中文研究变量")
  expect_equal(updated$x$state$options$digits, 4)
  expect_equal(updated$x$model$results[['loading:F1:Y2']]$unstandardized$estimate, 1.234)
  explicit <- suppressWarnings(mplus_diagram(model, config = path, options = list(digits = 3)))
  expect_equal(explicit$x$state$options$digits, 3)
  expect_identical(readBin(path, "raw", file.info(path)$size), before)
})

test_that("invalid and incompatible files remain unchanged and yield visible diagnostics", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  model <- MplusAutomation::readModels(diagram_fixture(), quiet = TRUE)
  w <- suppressWarnings(mplus_diagram(model))
  config <- .mplus_diagram_config(w)
  for (bad in list("{broken", '{"schema_version":99}',
                   as.character(jsonlite::toJSON(within(config, fingerprint <- "other"),
                     auto_unbox = TRUE, null = "null")))) {
    writeLines(bad, path)
    before <- readBin(path, "raw", file.info(path)$size)
    result <- suppressWarnings(mplus_diagram(model, config = path))
    expect_true(any(vapply(result$x$model$diagnostics, function(d) d$code == "config", logical(1))))
    expect_identical(result$x$state$views, result$x$automatic)
    expect_identical(readBin(path, "raw", file.info(path)$size), before)
  }
})

test_that("failed validation cannot replace the previous configuration", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  w <- suppressWarnings(mplus_diagram(diagram_fixture()))
  cfg <- .mplus_diagram_config(w)
  .mplus_diagram_write_config(cfg, path, w$x$model)
  before <- readBin(path, "raw", file.info(path)$size)
  bad <- cfg
  bad$state$views$full$nodes[[1]]$x <- Inf
  expect_error(.mplus_diagram_write_config(bad, path, w$x$model), "finite coordinates")
  expect_identical(readBin(path, "raw", file.info(path)$size), before)
  bad <- cfg
  bad$state$results <- list(fake = 999)
  expect_error(.mplus_diagram_write_config(bad, path, w$x$model), "unknown fields")
  expect_error(.mplus_diagram_write_config(cfg, NULL, w$x$model), "explicit configuration")
  expect_error(.mplus_diagram_write_config(cfg, file.path(tempfile(), 'x.json'), w$x$model), "existing directory")
  expect_identical(readBin(path, "raw", file.info(path)$size), before)
})

test_that("directory and list selection identify a single intended analysis", {
  w <- suppressWarnings(mplus_diagram(dirname(diagram_fixture()), select = "ex5.11.out"))
  model <- MplusAutomation::readModels(diagram_fixture(), quiet = TRUE)
  selected <- suppressWarnings(mplus_diagram(list(first = model), select = "first"))
  expect_equal(w$x$model$fingerprint, selected$x$model$fingerprint)
  expect_s3_class(w, "htmlwidget")
})

test_that("config versions are numeric and import cannot carry analysis values", {
  w <- suppressWarnings(mplus_diagram(diagram_fixture()))
  cfg <- .mplus_diagram_config(w)
  cfg$schema_version <- "1"
  expect_error(.mplus_diagram_validate_config(cfg, w$x$model), "Unsupported configuration")
  cfg <- .mplus_diagram_config(w)
  cfg$results <- w$x$model$results
  expect_error(.mplus_diagram_validate_config(cfg, w$x$model), "unknown fields")
})

test_that("single hidden edge remains a JSON array across local save and render", {
  w <- suppressWarnings(mplus_diagram(diagram_fixture()))
  cfg <- .mplus_diagram_config(w)
  cfg$state$hidden_edges <- list(w$x$model$structure$edges[[1]]$id)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  .mplus_diagram_write_config(cfg, path, w$x$model)
  json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_type(json$state$hidden_edges, "list")
  expect_length(json$state$hidden_edges, 1L)
  reloaded <- suppressWarnings(mplus_diagram(diagram_fixture(), config = path))
  encoded <- jsonlite::fromJSON(jsonlite::toJSON(reloaded$x, auto_unbox = TRUE,
    null = "null", na = "null"), simplifyVector = FALSE)
  expect_type(encoded$state$hidden_edges, "list")
  expect_identical(encoded$state$hidden_edges, cfg$state$hidden_edges)
})

test_that("configuration saving cannot overwrite its Mplus source", {
  path <- tempfile(fileext = ".out")
  alias <- tempfile(fileext = ".json")
  on.exit(unlink(c(path, alias)))
  file.copy(diagram_fixture(), path)
  before <- readBin(path, "raw", file.info(path)$size)
  expect_error(mplus_diagram(path, config = path), "ending in .json")
  w <- suppressWarnings(mplus_diagram(path))
  expect_error(.mplus_diagram_write_config(.mplus_diagram_config(w), path, w$x$model), "end in .json")
  if (.Platform$OS.type != "windows" && file.symlink(path, alias)) {
    expect_error(.mplus_diagram_write_config(.mplus_diagram_config(w), alias, w$x$model), "original Mplus output")
  }
  expect_identical(readBin(path, "raw", file.info(path)$size), before)
})

test_that("removed nodes and three-state display survive saving without changing results", {
  w <- suppressWarnings(mplus_diagram(diagram_fixture()))
  cfg <- .mplus_diagram_config(w)
  id <- w$x$model$structure$nodes[[1]]$id
  cfg$state$hidden_nodes <- list(id)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  for (flags in list(c(TRUE, TRUE), c(TRUE, FALSE), c(FALSE, FALSE))) {
    cfg$state$options$nonsignificant_dotted <- flags[1]
    cfg$state$options$nonsignificant_hide_values <- flags[2]
    .mplus_diagram_write_config(cfg, path, w$x$model)
    stored <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    expect_identical(stored$state$hidden_nodes, list(id))
    restored <- suppressWarnings(mplus_diagram(diagram_fixture(), config = path))
    expect_identical(restored$x$state$hidden_nodes, list(id))
    expect_identical(restored$x$state$options$nonsignificant_dotted, flags[1])
    expect_identical(restored$x$state$options$nonsignificant_hide_values, flags[2])
    expect_identical(restored$x$model, w$x$model)
    expect_equal(restored$x$state$views, w$x$state$views, tolerance = 0)
  }
  legacy <- cfg
  legacy$state$hidden_nodes <- NULL
  expect_identical(.mplus_diagram_validate_config(legacy, w$x$model)$state$hidden_nodes, list())
  for (bad in list(NULL, list("node:missing"), list(id, id), list(1),
                   list(named = id), NA_character_)) {
    invalid <- cfg
    invalid$state["hidden_nodes"] <- list(bad)
    expect_error(.mplus_diagram_validate_config(invalid, w$x$model), "hidden_nodes")
  }
})

test_that("adjacent configuration is discovered without an explicit path", {
  folder <- tempfile("diagram-discovery-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE))
  out <- file.path(folder, "analysis.out")
  file.copy(diagram_fixture(), out)
  w <- suppressWarnings(mplus_diagram(out))
  cfg <- .mplus_diagram_config(w)
  cfg$state$options$digits <- 5L
  cfg$state$aliases[[w$x$model$structure$nodes[[1]]$id]] <- "Discovered configuration"
  target <- file.path(folder, "analysis.diagram.json")
  .mplus_diagram_write_config(cfg, target, w$x$model)
  before <- readBin(target, "raw", file.info(target)$size)
  discovered <- suppressWarnings(mplus_diagram(out))
  expect_equal(discovered$x$state$options$digits, 5)
  expect_identical(discovered$x$state$aliases, cfg$state$aliases)
  expect_identical(readBin(target, "raw", file.info(target)$size), before)
})

test_that("constraint changes reject saved layouts even with the same filename", {
  # A deliberate object mutation tests the identity boundary; it is not a new
  # fitted Mplus analysis. The complete real output remains unchanged on disk.
  model <- MplusAutomation::readModels(system.file("extdata", "mplus-diagram", "ex5.20.out", package = "MplusAutomation"), quiet = TRUE)
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  w <- mplus_diagram(model)
  cfg <- .mplus_diagram_config(w)
  cfg$state$views$full$nodes[[1]]$x <- 987654
  .mplus_diagram_write_config(cfg, path, w$x$model)
  before <- readBin(path, "raw", file.info(path)$size)
  model$input$model.constraint <- c(model$input$model.constraint, "p1 = p2;")
  changed <- suppressWarnings(mplus_diagram(model, config = path))
  expect_false(identical(changed$x$model$fingerprint, w$x$model$fingerprint))
  expect_identical(changed$x$state$views, changed$x$automatic)
  expect_true(any(vapply(changed$x$model$diagnostics, function(d) d$code == "config", logical(1))))
  expect_identical(readBin(path, "raw", file.info(path)$size), before)
})

test_that("relative object provenance cannot select another project's sidecar", {
  original_directory <- getwd()
  root <- tempfile("diagram-projects-")
  dir.create(root)
  on.exit({ setwd(original_directory); unlink(root, recursive = TRUE) })
  a <- file.path(root, "a"); b <- file.path(root, "b")
  dir.create(a); dir.create(b)
  file.copy(diagram_fixture(), file.path(a, "analysis.out"))
  file.copy(diagram_fixture(), file.path(b, "analysis.out"))
  setwd(a)
  object <- MplusAutomation::readModels("analysis.out", quiet = TRUE)
  reference <- suppressWarnings(mplus_diagram("analysis.out"))
  cfg <- .mplus_diagram_config(reference)
  cfg$state$aliases[[reference$x$model$structure$nodes[[1]]$id]] <- "Other project"
  target <- file.path(b, "analysis.diagram.json")
  .mplus_diagram_write_config(cfg, target, reference$x$model)
  setwd(b)
  from_object <- suppressWarnings(mplus_diagram(object))
  expect_null(from_object$x$model$provenance$path)
  expect_length(from_object$x$state$aliases, 0L)
  # The caller can still deliberately choose this directory or this JSON file.
  from_file <- suppressWarnings(mplus_diagram("analysis.out"))
  explicit <- suppressWarnings(mplus_diagram(object, config = target))
  expect_identical(from_file$x$state$aliases, cfg$state$aliases)
  expect_identical(explicit$x$state$aliases, cfg$state$aliases)
})
