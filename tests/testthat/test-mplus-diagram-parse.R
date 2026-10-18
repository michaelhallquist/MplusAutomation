skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

# Real engine outputs exercise the public path. Mutated copies below are
# deliberately synthetic numerical/ambiguity probes, not Mplus validation data.
diagram_parse_fixture <- function(name = "ex5.20.out") {
  path <- system.file("extdata", "mplus-diagram", name, package = "MplusAutomation")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "extdata", "mplus-diagram", name)
  path
}
diagram_parse_model <- function(name = "ex5.20.out") {
  MplusAutomation::readModels(diagram_parse_fixture(name), quiet = TRUE)
}
diagram_edge <- function(widget, id) {
  edges <- widget$x$model$structure$edges
  edges[[match(id, vapply(edges, `[[`, character(1), "id"))]]
}
diagram_unstandardized <- function(model, ...) {
  mplus_diagram(model, options = list(scale = "unstandardized"), ...)
}

test_that("real SEM output yields audited topology through the public filename entry", {
  for (file in c("ex5.11.out", "ex5.12.out")) {
    w <- diagram_unstandardized(diagram_parse_fixture(file))
    expect_s3_class(w, "htmlwidget")
    nodes <- w$x$model$structure$nodes
    expect_length(nodes, 16)
    expect_equal(sum(vapply(nodes, function(n) n$role == "latent", logical(1))), 4)
    edges <- w$x$model$structure$edges
    expected_loadings <- paste("loading", rep(paste0("F", 1:4), each = 3), paste0("Y", 1:12), sep = ":")
    expected <- c(expected_loadings, "regression:F3:F4", "regression:F1:F3", "regression:F2:F3", "covariance:F1:F2",
                  paste("variance", c(paste0("F", 1:4), paste0("Y", 1:12)), c(paste0("F", 1:4), paste0("Y", 1:12)), sep = ":"))
    expect_setequal(vapply(edges, `[[`, character(1), "id"), expected)
    expect_true(diagram_edge(w, "loading:F1:Y1")$fixed)
    expect_equal(diagram_edge(w, "loading:F1:Y1")$fixed_value, 1)
    expect_false(diagram_edge(w, "loading:F1:Y2")$fixed)
    expect_equal(diagram_edge(w, "covariance:F1:F2")$origin, "default")
    expect_equal(w$x$model$results[["regression:F3:F4"]]$unstandardized$estimate, .473)
    expect_equal(w$x$model$results[["covariance:F1:F2"]]$unstandardized$estimate, -.030)
    expect_null(w$x$model$results[["loading:F1:Y1"]]$unstandardized$p)
    expect_null(w$x$model$results[["loading:F1:Y1"]]$unstandardized$z)
  }
})

test_that("real CFA constraints, original labels and actual scales survive adaptation", {
  w <- mplus_diagram(diagram_parse_fixture())
  expect_equal(w$x$model$structure$family, "cfa")
  expect_equal(diagram_edge(w, "loading:F1:Y2")$label, "lam2")
  expect_equal(diagram_edge(w, "loading:F1:Y3")$label, "lam3")
  expect_equal(diagram_edge(w, "variance:Y6:Y6")$label, "ve6")
  expect_true(any(vapply(w$x$model$structure$constraints, function(c) c$type == "model_constraint", logical(1))))
  expect_equal(w$x$model$results[["loading:F1:Y1"]]$unstandardized$estimate, 1)
  expect_equal(w$x$model$results[["loading:F1:Y1"]]$stdyx$estimate, .800)
  expect_equal(w$x$model$results[["loading:F1:Y1"]]$stdyx$se, .022)
  expect_equal(w$x$state$options$scale, "stdyx")
})

test_that("scale matching is independent of table order and WITH direction", {
  original <- diagram_parse_model()
  reordered <- original
  table <- reordered$parameters$stdyx.standardized
  i <- which(table$paramHeader == "F2.WITH" & table$param == "F1")
  table$paramHeader[i] <- "F1.WITH"; table$param[i] <- "F2"
  reordered$parameters$stdyx.standardized <- table[rev(seq_len(nrow(table))), ]
  expect_identical(mplus_diagram(original)$x$model$results, mplus_diagram(reordered)$x$model$results)
})

test_that("synthetic fixed zero and free start values cannot masquerade as estimates", {
  model <- diagram_parse_model()
  model$input$model <- sub("f1 BY y1", "f1 BY y1*7", model$input$model, fixed = TRUE)
  model$input$model <- c(model$input$model, "y1 WITH y2@0;")
  row <- which(model$parameters$unstandardized$paramHeader == "F1.BY" & model$parameters$unstandardized$param == "Y1")
  model$parameters$unstandardized$est[row] <- 1.25
  # A free parameter can have a reported zero SE; syntax, not SE, sets fixed.
  model$parameters$unstandardized$se[row] <- 0
  w <- diagram_unstandardized(model)
  edge <- diagram_edge(w, "loading:F1:Y1")
  expect_false(edge$fixed)
  expect_null(edge$fixed_value)
  expect_equal(w$x$model$results[[edge$id]]$unstandardized$estimate, 1.25)
  zero <- diagram_edge(w, "covariance:Y1:Y2")
  expect_true(zero$fixed)
  expect_equal(zero$fixed_value, 0)
  expect_equal(zero$origin, "explicit")
  expect_equal(w$x$model$results[[zero$id]]$unstandardized$estimate, 0)
})

test_that("synthetic true 999 and estimated zero are retained, sentinel inference is null", {
  model <- diagram_parse_model()
  row <- which(model$parameters$unstandardized$paramHeader == "F1.BY" & model$parameters$unstandardized$param == "Y2")
  model$parameters$unstandardized[row, c("est", "se", "est_se", "pval")] <- c(999, 1, 999, 0)
  w <- diagram_unstandardized(model)
  r <- w$x$model$results[["loading:F1:Y2"]]$unstandardized
  expect_equal(r$estimate, 999)
  expect_equal(r$z, 999) # This is a coherent statistic, not fixed-row 999.
  model$parameters$unstandardized[row, c("est", "se", "est_se", "pval")] <- c(0, .1, 0, 1)
  w <- diagram_unstandardized(model)
  expect_false(diagram_edge(w, "loading:F1:Y2")$fixed)
  expect_equal(w$x$model$results[["loading:F1:Y2"]]$unstandardized$estimate, 0)
  model$parameters$unstandardized[row, c("est", "se", "est_se", "pval")] <- c(NA, 999, 999, 999)
  r <- diagram_unstandardized(model)$x$model$results[["loading:F1:Y2"]]$unstandardized
  expect_null(r$estimate); expect_null(r$se); expect_null(r$z); expect_null(r$p)
})

test_that("synthetic explicit and unverified covariances retain different origins", {
  model <- diagram_parse_model()
  model$input$model <- c(model$input$model, "f1 WITH f2;")
  expect_equal(diagram_edge(mplus_diagram(model), "covariance:F1:F2")$origin, "explicit")
  model <- diagram_parse_model()
  row <- model$parameters$unstandardized[1, ]
  row$paramHeader <- "Y1.WITH"; row$param <- "Y2"
  row$est <- .12; row$se <- .05; row$est_se <- 2.4; row$pval <- .016
  model$parameters$unstandardized <- rbind(model$parameters$unstandardized, row)
  w <- mplus_diagram(model)
  expect_equal(diagram_edge(w, "covariance:Y1:Y2")$origin, "unknown")
  expect_null(diagram_edge(w, "covariance:Y1:Y2")$fixed)
  expect_true(any(vapply(w$x$model$diagnostics, function(d) d$code == "unknown_origin", logical(1))))
})

test_that("missing and partial STDYX never mix scales", {
  model <- diagram_parse_model()
  model$parameters$stdyx.standardized <- NULL
  expect_warning(w <- mplus_diagram(model), "STDYX is unavailable")
  expect_equal(w$x$state$options$scale, "unstandardized")
  model <- diagram_parse_model()
  model$parameters$stdyx.standardized <- model$parameters$stdyx.standardized[-1, ]
  w <- mplus_diagram(model)
  expect_equal(w$x$state$options$scale, "stdyx")
  expect_null(w$x$model$results[["loading:F1:Y1"]]$stdyx)
  expect_true(any(vapply(w$x$model$diagnostics, function(d) d$code == "partial_stdyx", logical(1))))
})

test_that("unsupported and ambiguous synthetic syntax fails closed", {
  model <- diagram_parse_model()
  for (syntax in c("i s q | y1@0 y2@1 y3@2;", "y2^^ ON y1^;", "y2 y3 PON y1;", "%WITHIN% y2 ON y1;", "f1 BY y2;")) {
    bad <- model; bad$input$model <- c(bad$input$model, syntax)
    expect_error(mplus_diagram(bad), class = "mplus_diagram_unsupported_error")
  }
  bad <- model; bad$input$model <- NULL
  expect_error(mplus_diagram(bad), "complete echoed MODEL")
  bad <- model; bad$parameters$unstandardized <- rbind(bad$parameters$unstandardized, bad$parameters$unstandardized[1, ])
  expect_error(mplus_diagram(bad), "Duplicate parameter identity")
  bad <- model; bad$input$variable$categorical <- "y1"
  expect_error(mplus_diagram(bad), "Unsupported VARIABLE")
  bad <- model; bad$summaries$Estimator <- "BAYES"
  expect_error(mplus_diagram(bad), "ML or MLR")
  bad <- model; bad$errors <- list("MODEL ESTIMATION DID NOT TERMINATE NORMALLY")
  expect_error(mplus_diagram(bad), class = "mplus_diagram_results_error")
})

test_that("ranges follow declared order, and line labels preserve their scope", {
  model <- diagram_parse_model()
  # Synthetic reordered declaration: Y1-Y3 includes Y2 and Y5 here.
  model$input$variable$names <- "y1 y2 y5 y3 y4 y6"
  model$input$model <- c("f1 BY y1\n y2 y3 (a);", "f2 BY y4 y5 y6;")
  model$input$model.constraint <- NULL
  w <- mplus_diagram(model)
  expect_null(diagram_edge(w, "loading:F1:Y1")$label)
  expect_equal(diagram_edge(w, "loading:F1:Y2")$label, "a")
  expect_equal(diagram_edge(w, "loading:F1:Y3")$label, "a")
  # Unmatched range generates a topology mismatch; it cannot silently infer
  # numerical suffix ordering or discard the extra Y5 relationship.
  model$input$model[1] <- "f1 BY y1-y3;"
  expect_error(mplus_diagram(model), "No unstandardized result.*loading:F1:Y5")
})

test_that("semantic mappings use declared identities and reject contradictions", {
  model <- diagram_parse_model()
  w <- mplus_diagram(model, mapping = list(waves = c(y1 = 1, y2 = 2), constructs = c(y1 = "评分", y2 = "评分")))
  node <- Filter(function(n) n$name == "Y2", w$x$model$structure$nodes)[[1]]
  expect_equal(node$wave, 2); expect_equal(node$construct, "评分")
  expect_error(mplus_diagram(model, mapping = list(roles = c(y1 = "within"))), "contradicts")
  expect_error(mplus_diagram(model, mapping = list(waves = c(unknown = 1))), "unknown variables")
  expect_error(mplus_diagram(model, mapping = list(family = "clpm")), "requires explicit")
})

diagram_longitudinal_fixture <- function(name) {
  path <- system.file("extdata", "mplus-diagram", paste0(name, ".out"), package = "MplusAutomation")
  if (!nzchar(path)) path <- testthat::test_path("..", "..", "inst", "extdata", "mplus-diagram", paste0(name, ".out"))
  path
}

test_that("verified RI decomposition carries mappings from observed to within nodes", {
  for (family in c("ri-explicit", "ri-hat")) {
    path <- diagram_longitudinal_fixture(paste0(family, "-ml"))
    mapping <- list(family = "ri_clpm", waves = c(x1 = 1, x2 = 2, x3 = 3, y1 = 1, y2 = 2, y3 = 3),
                    constructs = c(x1 = "X", x2 = "X", x3 = "X", y1 = "Y", y2 = "Y", y3 = "Y"))
    w <- mplus_diagram(path, mapping = mapping)
    within <- Filter(function(n) n$role == "within", w$x$model$structure$nodes)
    expect_length(within, 6)
    expect_setequal(vapply(within, function(n) paste(n$construct, n$wave), character(1)),
                    c("X 1", "X 2", "X 3", "Y 1", "Y 2", "Y 3"))
    # Each dynamic path retains its actual latent/hat endpoints.
    regressions <- Filter(function(e) e$type == "regression", w$x$model$structure$edges)
    expect_true(all(vapply(regressions, function(e) e$from %in% vapply(within, `[[`, character(1), "id") &&
                            e$to %in% vapply(within, `[[`, character(1), "id"), logical(1))))
    mapping$waves[if (family == "ri-hat") "X1^" else "WX1"] <- 99
    expect_error(mplus_diagram(path, mapping = mapping), "contradicts the within/observed")
  }
})

test_that("hat paired syntax expands ranges without mixing standardization identities", {
  model <- MplusAutomation::readModels(diagram_longitudinal_fixture("ri-hat-ml"), quiet = TRUE)
  baseline <- mplus_diagram(model)
  # Equivalent syntax against the same real engine results; original fixture
  # uses ON and WITH, the official 5-wave fixture also directly exercises PON.
  model$input$model <- c("RIx BY x1-x3@1;", "RIy BY y1-y3@1;",
                        "x2^-x3^ y2^-y3^ PON x1^-x2^ y1^-y2^;",
                        "x2^-x3^ y2^-y3^ PON y1^-y2^ x1^-x2^;",
                        "x1^-x3^ PWITH y1^-y3^;")
  w <- mplus_diagram(model)
  expect_identical(w$x$model$results, baseline$x$model$results)
  expect_setequal(vapply(w$x$model$structure$edges, `[[`, character(1), "id"),
                  vapply(baseline$x$model$structure$edges, `[[`, character(1), "id"))
  expect_equal(w$x$model$results[["regression:X1^:X2^"]]$stdyx$estimate, .547)
  expect_null(w$x$model$results[["loading:X1^:X1"]]$stdyx)
  expect_equal(diagram_edge(w, "loading:X1^:X1")$fixed_source,
               "Mplus residual-hat definition (not an estimated result row)")
})

test_that("growth zero is structural time information on both ML scales", {
  for (estimator in c("ml", "mlr")) {
    w <- mplus_diagram(diagram_longitudinal_fixture(paste0("growth-", estimator)))
    expect_equal(w$x$model$structure$family, "growth")
    expect_true(diagram_edge(w, "loading:S:Y11")$fixed)
    expect_equal(diagram_edge(w, "loading:S:Y11")$fixed_value, 0)
    expect_equal(w$x$model$results[["loading:S:Y11"]]$stdyx$estimate, 0)
    observed <- Filter(function(n) n$role == "observed", w$x$model$structure$nodes)
    expect_equal(vapply(observed, `[[`, numeric(1), "wave"), 0:3)
    expect_setequal(vapply(Filter(function(n) n$role != "observed", w$x$model$structure$nodes), `[[`, character(1), "role"),
                    c("growth_intercept", "growth_slope"))
  }
})

test_that("normal completion preserves numerical warnings while true failures stop", {
  w <- mplus_diagram(diagram_longitudinal_fixture("estimate-999"))
  expect_equal(w$x$model$results[["regression:X:Y"]]$unstandardized$estimate, 999)
  expect_equal(w$x$model$results[["regression:Z:Y"]]$unstandardized$estimate, 0)
  expect_true(any(vapply(w$x$model$diagnostics, function(d) grepl("TRUSTWORTHY", d$message), logical(1))))
  expect_error(mplus_diagram(diagram_longitudinal_fixture("failed-convergence")), class = "mplus_diagram_results_error")
  model <- diagram_parse_model()
  model$output <- sub("THE MODEL ESTIMATION TERMINATED NORMALLY", "MODEL RESULTS", model$output, fixed = TRUE)
  expect_error(mplus_diagram(model), "does not confirm normal")
})
