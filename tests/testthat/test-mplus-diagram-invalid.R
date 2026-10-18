skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

# Deliberately incomplete/contradictory mplus.model objects test fail-closed
# behavior. These mutations are synthetic; the committed engine outputs remain
# unchanged and their numerical acceptance is tested independently.
invalid_diagram_model <- function(name = "cfa-ml") {
  path <- system.file("extdata", "mplus-diagram", paste0(name, ".out"), package = "MplusAutomation")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "extdata", "mplus-diagram", paste0(name, ".out"))
  MplusAutomation::readModels(path, quiet = TRUE)
}
invalid_diagram_remove <- function(model, header) {
  for (scale in c("unstandardized", "stdyx.standardized")) {
    table <- model$parameters[[scale]]
    model$parameters[[scale]] <- table[!table$paramHeader %in% header, , drop = FALSE]
  }
  model
}

test_that("single-group scope must be known rather than assumed from partial summaries", {
  for (field in c("NGroups", "AnalysisType", "Estimator")) {
    for (value in list(NULL, NA)) {
      model <- invalid_diagram_model(); model$summaries[[field]] <- value
      expect_error(mplus_diagram(model), class = "mplus_diagram_results_error")
    }
  }
  model <- invalid_diagram_model(); model$input$analysis$type <- "TWO"
  expect_error(mplus_diagram(model), class = "mplus_diagram_unsupported_error")
})

test_that("an object without termination evidence does not silently borrow another run", {
  model <- invalid_diagram_model()
  expect_true(file.exists(attr(model, "filename")))
  model$output <- NULL; model$errors <- NULL; model$warnings <- NULL
  expect_error(mplus_diagram(model), "complete.*object|termination", class = "mplus_diagram_results_error")
})

test_that("missing mandatory residuals and default covariances cannot produce a partial graph", {
  model <- invalid_diagram_remove(invalid_diagram_model(), "Residual.Variances")
  expect_error(mplus_diagram(model), "variance|residual", class = "mplus_diagram_results_error")
  model <- invalid_diagram_model()
  model$input$model <- model$input$model[!grepl("WITH", model$input$model, ignore.case = TRUE)]
  model <- invalid_diagram_remove(model, "F1.WITH")
  expect_error(mplus_diagram(model), "covariance", class = "mplus_diagram_results_error")
  # Conditioned observed covariates have no estimated variance rows by design.
  expect_s3_class(mplus_diagram(invalid_diagram_model("path-ml")), "htmlwidget")
  # Explicit zero measurement variances and ^ decomposition remain accepted.
  for (name in c("ri-explicit-ml", "ri-hat-ml"))
    expect_s3_class(mplus_diagram(invalid_diagram_model(name)), "htmlwidget")
})

test_that("parameter context cannot be erased while joining result scales", {
  for (scale in c("unstandardized", "stdyx.standardized")) {
    model <- invalid_diagram_model()
    model$parameters[[scale]]$Group <- rep(c("A", "B"), length.out = nrow(model$parameters[[scale]]))
    expect_error(mplus_diagram(model), "context|group", class = "mplus_diagram_unsupported_error")
    for (field in c("BetweenWithin", "LatentClass")) {
      model <- invalid_diagram_model(); model$parameters[[scale]][[field]] <- "1"
      expect_error(mplus_diagram(model), "context|level|class", class = "mplus_diagram_unsupported_error")
    }
  }
  model <- invalid_diagram_model()
  model$parameters$unstandardized$Group <- NA_character_
  model$parameters$unstandardized$BetweenWithin <- NA_character_
  expect_s3_class(mplus_diagram(model), "htmlwidget")
})

test_that("fixed syntax and current estimates must agree within output precision", {
  model <- invalid_diagram_model()
  row <- which(model$parameters$unstandardized$paramHeader == "F1.BY" & model$parameters$unstandardized$param == "Y1")
  model$parameters$unstandardized$est[row] <- 42
  expect_error(mplus_diagram(model), "fixed", class = "mplus_diagram_results_error")
  model$parameters$unstandardized$est[row] <- 1.0004
  expect_s3_class(mplus_diagram(model), "htmlwidget")
})

test_that("result data types cannot silently recode coefficients as factor levels", {
  model <- invalid_diagram_model()
  model$parameters$unstandardized$est <- factor(model$parameters$unstandardized$est)
  expect_error(mplus_diagram(model), "numeric", class = "mplus_diagram_results_error")
  model <- invalid_diagram_model(); model$parameters$unstandardized$param[1] <- NA_character_
  expect_error(mplus_diagram(model), "identity|parameter", class = "mplus_diagram_results_error")
  model <- invalid_diagram_model(); model$parameters$stdyx.standardized <- list(est = 1)
  expect_error(mplus_diagram(model), "table", class = "mplus_diagram_results_error")
})

test_that("all unavailable STDYX estimates trigger a whole-graph public fallback", {
  for (empty in c(FALSE, TRUE)) {
    model <- invalid_diagram_model()
    if (empty) model$parameters$stdyx.standardized <- data.frame()
    else model$parameters$stdyx.standardized$est <- NA_real_
    expect_warning(widget <- mplus_diagram(model), "STDYX is unavailable")
    expect_identical(widget$x$state$options$scale, "unstandardized")
    expect_true(any(vapply(widget$x$model$diagnostics, function(d) d$code == "missing_stdyx", logical(1))))
  }
})

test_that("explicit mappings cannot contradict known model families or growth chronology", {
  for (name in c("ri-explicit-ml", "growth-ml")) {
    model <- invalid_diagram_model(name)
    for (family in c("cfa", "sem"))
      expect_error(mplus_diagram(model, mapping = list(family = family)), "contradict", class = "mplus_diagram_input_error")
  }
  growth <- invalid_diagram_model("growth-ml")
  expect_error(mplus_diagram(growth, mapping = list(waves = c(Y11 = 3, Y12 = 2, Y13 = 1, Y14 = 0))),
               "time|chronolog", class = "mplus_diagram_input_error")
  # A wave index with another origin still preserves actual model chronology.
  expect_s3_class(mplus_diagram(growth, mapping = list(waves = c(Y11 = 1, Y12 = 2, Y13 = 3, Y14 = 4))), "htmlwidget")
})

test_that("categorical and multilevel VARIABLE abbreviations stay outside scope", {
  for (key in c("categ", "nom", "cens", "clust", "with", "betw", "group", "knownc")) {
    model <- invalid_diagram_model(); model$input$variable[[key]] <- "y1"
    expect_error(mplus_diagram(model), class = "mplus_diagram_unsupported_error")
  }
})

test_that("a coherent large standard error of 999 is a value rather than a sentinel", {
  model <- invalid_diagram_model()
  row <- which(model$parameters$unstandardized$paramHeader == "F1.BY" & model$parameters$unstandardized$param == "Y2")
  model$parameters$unstandardized[row, c("est", "se", "est_se", "pval")] <- c(1998, 999, 2, .046)
  widget <- mplus_diagram(model)
  expect_equal(widget$x$model$results[["loading:F1:Y2"]]$unstandardized$se, 999)
})

test_that("an all-missing results object cannot masquerade as a fitted diagram", {
  # Deliberate incomplete-object mutation, not another Mplus engine run.
  model <- MplusAutomation::readModels(system.file("extdata", "mplus-diagram",
    "cfa-ml.out", package = "MplusAutomation"), quiet = TRUE)
  model$parameters$unstandardized$est[] <- NA_real_
  model$parameters$stdyx.standardized$est[] <- NA_real_
  expect_error(mplus_diagram(model), "no finite estimates", class = "mplus_diagram_results_error")
})

test_that("coherent free-parameter SE and z values of 999 both survive", {
  # Synthetic column boundary, not a claim of another fitted analysis.
  model <- MplusAutomation::readModels(system.file("extdata", "mplus-diagram",
    "cfa-ml.out", package = "MplusAutomation"), quiet = TRUE)
  row <- with(model$parameters$unstandardized, which(paramHeader == "F1.BY" & param == "Y2"))
  model$parameters$unstandardized[row, c("est", "se", "est_se", "pval")] <- list(998001, 999, 999, 0)
  widget <- mplus_diagram(model, options = list(scale = "unstandardized"))
  result <- widget$x$model$results[["loading:F1:Y2"]]$unstandardized
  expect_equal(result$estimate, 998001)
  expect_equal(result$se, 999)
  expect_equal(result$z, 999)
  expect_equal(result$p, 0)
  model$parameters$unstandardized$pval <- NULL
  partial <- mplus_diagram(model, options = list(scale = "unstandardized"))
  expect_equal(partial$x$model$results[["loading:F1:Y2"]]$unstandardized$se, 999)
  expect_null(partial$x$model$results[["loading:F1:Y2"]]$unstandardized$p)
})
