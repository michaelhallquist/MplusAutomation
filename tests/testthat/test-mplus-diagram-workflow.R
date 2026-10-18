skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

# Independent source-table expectations, through the public API.
workflow_fixture <- function(name) {
  candidate <- system.file("extdata", "mplus-diagram", paste0(name, ".out"), package = "MplusAutomation")
  if (!nzchar(candidate)) candidate <- system.file("extdata", paste0(name, ".out"), package = "MplusAutomation")
  if (!nzchar(candidate)) {
    candidate <- test_path("..", "..", "inst", "extdata", "mplus-diagram", paste0(name, ".out"))
    if (!file.exists(candidate)) candidate <- test_path("..", "..", "inst", "extdata", paste0(name, ".out"))
  }
  candidate
}
workflow_expected <- function(name) jsonlite::read_json(
  test_path("..", "fixtures", "mplus-diagram", paste0(name, ".expected.json")), simplifyVector = FALSE)
workflow_identity <- function(edge) {
  if (edge$operator == "ON") return(paste("regression", edge$rhs, edge$lhs, sep = ":"))
  if (edge$operator == "WITH") return(paste(c("covariance", sort(c(edge$lhs, edge$rhs))), collapse = ":"))
  paste("loading", edge$lhs, edge$rhs, sep = ":")
}
workflow_config <- function(widget) list(schema_version = 1L,
  fingerprint = widget$x$model$fingerprint, layout_version = 1L, state = widget$x$state)
workflow_write_config <- function(config, path) jsonlite::write_json(config, path,
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)

workflow_cases <- sub("\\.expected\\.json$", "", list.files(
  test_path("..", "fixtures", "mplus-diagram"), pattern = "\\.expected\\.json$"))
test_that("the complete independent expected-value corpus is present", {
  manifest <- jsonlite::read_json(system.file("extdata", "mplus-diagram", "manifest.json", package = "MplusAutomation"))
  positive <- Filter(function(f) !is.null(f$expected_path), manifest$fixtures)
  expected_cases <- vapply(positive, `[[`, character(1), "id")
  expect_length(expected_cases, 24L)
  expect_setequal(workflow_cases, expected_cases)
})

for (case in workflow_cases) local({
  name <- case
  test_that(paste("public diagram matches every independently transcribed relationship:", name), {
    expected <- workflow_expected(name)
    source_warnings <- character()
    widget <- withCallingHandlers(
      mplus_diagram(workflow_fixture(name), options = list(scale = "unstandardized")),
      warning = function(w) {
        source_warnings <<- c(source_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    # MplusAutomation 1.3 warns while reading an ancillary Web Talk hat table;
    # all graph coefficients below still must match the independently read source.
    expect_true(all(grepl("number of columns of result is not a multiple of vector length", source_warnings, fixed = TRUE)))
    expect_s3_class(widget, "htmlwidget")
    nodes <- widget$x$model$structure$nodes
    node_names <- vapply(nodes, `[[`, character(1), "name")
    roles <- stats::setNames(vapply(nodes, `[[`, character(1), "role"), node_names)
    expect_setequal(node_names, c(unlist(expected$observed), names(expected$latent_roles)))
    expect_true(all(roles[unlist(expected$observed)] == "observed"))
    for (id in names(expected$latent_roles)) {
      wanted <- expected$latent_roles[[id]]
      if (wanted == "factor") wanted <- "latent"
      expect_equal(unname(roles[id]), wanted, info = id)
    }
    edges <- widget$x$model$structure$edges
    edge_ids <- vapply(edges, `[[`, character(1), "id")
    relational_ids <- edge_ids[vapply(edges, function(e) e$type != "variance", logical(1))]
    wanted_ids <- vapply(expected$relationships, workflow_identity, character(1))
    # Hat residual syntax has an implicit measurement connection for each
    # within component; it is present semantically but absent in MODEL RESULTS.
    extra <- setdiff(relational_ids, wanted_ids)
    hat <- any(grepl("\\^$", names(expected$latent_roles)))
    if (hat) {
      implicit <- paste("loading", names(expected$latent_roles)[grepl("\\^$", names(expected$latent_roles))],
        sub("\\^$", "", names(expected$latent_roles)[grepl("\\^$", names(expected$latent_roles))]), sep = ":")
      expect_true(all(extra %in% implicit))
    } else expect_setequal(relational_ids, wanted_ids)
    expect_true(all(wanted_ids %in% edge_ids))
    for (e in expected$relationships) {
      id <- workflow_identity(e)
      actual <- edges[[match(id, edge_ids)]]
      result <- widget$x$model$results[[id]]
      expect_equal(actual$fixed, e$fixed, info = id)
      expect_equal(actual$fixed_value, e$fixed_value, info = id)
      expect_equal(toupper(actual$label), toupper(e$label), info = id)
      if (!is.null(e$covariance_source)) expect_equal(actual$origin, e$covariance_source, info = id)
      expect_equal(result$unstandardized$estimate, e$unstandardized, tolerance = 1e-12, info = id)
      expect_equal(result$stdyx$estimate, e$stdyx, tolerance = 1e-12, info = id)
      if (isTRUE(e$fixed)) {
        expect_null(result$unstandardized$z, info = id)
        expect_null(result$unstandardized$p, info = id)
      }
    }
    for (id in unlist(expected$zero_observed_residuals)) {
      index <- match(paste("variance", id, id, sep = ":"), edge_ids)
      expect_false(is.na(index))
      expect_true(edges[[index]]$fixed)
      expect_equal(edges[[index]]$fixed_value, 0)
    }
    for (pair in expected$forbidden_covariances) {
      expect_false(paste(c("covariance", sort(unlist(pair))), collapse = ":") %in% edge_ids)
    }
  })
})

test_that("public object input joins reordered and partial standardized tables by identity", {
  model <- MplusAutomation::readModels(workflow_fixture("cfa-ml"), quiet = TRUE)
  original <- mplus_diagram(model)
  table <- model$parameters$stdyx.standardized
  model$parameters$stdyx.standardized <- table[rev(seq_len(nrow(table))), ]
  reordered <- mplus_diagram(model)
  expect_identical(original$x$model$results, reordered$x$model$results)
  model$parameters$stdyx.standardized <- table[!(table$paramHeader == "F1.BY" & table$param == "Y2"), ]
  partial <- mplus_diagram(model)
  expect_equal(partial$x$state$options$scale, "stdyx")
  expect_null(partial$x$model$results[["loading:F1:Y2"]]$stdyx)
  expect_equal(partial$x$model$results[["loading:F1:Y2"]]$unstandardized$estimate, 1.065)
  model$parameters$stdyx.standardized <- NULL
  expect_warning(absent <- mplus_diagram(model), "STDYX is unavailable")
  expect_equal(absent$x$state$options$scale, "unstandardized")
})

test_that("a public saved-state round trip refreshes estimates and preserves the file", {
  model <- MplusAutomation::readModels(workflow_fixture("cfa-ml"), quiet = TRUE)
  original <- mplus_diagram(model)
  config <- workflow_config(original)
  id <- "node:F1"
  config$state$aliases[[id]] <- "研究构念甲"
  config$state$views$full$nodes[[id]]$x <- 631
  config$state$options$digits <- 4L
  target <- tempfile(fileext = ".diagram.json")
  on.exit(unlink(target), add = TRUE)
  workflow_write_config(config, target)
  before <- readBin(target, "raw", n = file.info(target)$size)
  row <- model$parameters$unstandardized$paramHeader == "F1.BY" & model$parameters$unstandardized$param == "Y2"
  model$parameters$unstandardized$est[row] <- 1.234
  updated <- mplus_diagram(model, config = target, defaults = list(digits = 1L))
  expect_identical(updated$x$model$fingerprint, original$x$model$fingerprint)
  expect_equal(updated$x$state$aliases[[id]], "研究构念甲")
  expect_equal(updated$x$state$views$full$nodes[[id]]$x, 631)
  expect_equal(updated$x$state$options$digits, 4)
  expect_equal(updated$x$model$results[["loading:F1:Y2"]]$unstandardized$estimate, 1.234)
  overridden <- mplus_diagram(model, config = target, options = list(digits = 3L))
  expect_equal(overridden$x$state$options$digits, 3)
  expect_identical(readBin(target, "raw", n = file.info(target)$size), before)
  expect_error(mplus_diagram(model, action = "edit", config = target), "interactive R session")
})

test_that("actual failures and unsupported engine outputs cannot become valid diagrams", {
  expect_error(mplus_diagram(workflow_fixture("failed-convergence")), class = "mplus_diagram_results_error")
  expect_error(mplus_diagram(workflow_fixture("failed-label-count")), class = "mplus_diagram_error")
  expect_error(mplus_diagram(workflow_fixture("ex3.12")), class = "mplus_diagram_unsupported_error")
  expect_error(mplus_diagram(workflow_fixture("ex5.13")), class = "mplus_diagram_unsupported_error")
})

test_that("random-starts success requires replicated maxima and complete quality output", {
  model <- MplusAutomation::readModels(workflow_fixture("webtalk4-clpm-mlr"), quiet = TRUE)
  w <- mplus_diagram(model)
  expect_true(any(vapply(w$x$model$diagnostics,
    function(d) d$code == "random_starts_termination", logical(1))))
  truncated <- model
  truncated$output <- truncated$output[trimws(truncated$output) != "QUALITY OF NUMERICAL RESULTS"]
  expect_error(mplus_diagram(truncated), "does not confirm normal")
  unreplicated <- model
  lines <- trimws(unreplicated$output)
  start <- which(lines == "Fit function values at local maxima and random start numbers:")
  finish <- which(lines == "MODEL FIT INFORMATION")
  rows <- seq.int(start + 1L, finish - 1L)
  rows <- rows[grepl("^[+-]?[0-9]+\\.[0-9]+[[:space:]]+([0-9]+|unperturbed)$", lines[rows])]
  unreplicated$output[rows] <- paste(seq_along(rows) + 200, ".1234  ", seq_along(rows), sep = "")
  expect_error(mplus_diagram(unreplicated), "does not confirm normal")
})
