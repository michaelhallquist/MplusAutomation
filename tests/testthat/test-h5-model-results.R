test_that("readModels prefers supported H5 MODEL RESULTS tables", {
  result_headings <- c("Estimate", "S.E.", "Estimate/S.E.", "Two-Tailed P-Value")
  statement_headings <- c(
    "Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable"
  )
  model_results <- list(
    Results = matrix(c(
      1.23456789, 0.012345678, 99.999999, 1e-10,
      0.45678901, 0.023456789, 19.472, 2e-8,
      0.98765432, 0.034567891, 28.572, 3e-12
    ), ncol = 4L, byrow = TRUE),
    Statements = matrix(c(
      "", "Y1", "ON", "X1",
      "Intercepts", "Y1", "", "",
      "Residual Variances", "Y1", "", ""
    ), ncol = 4L, byrow = TRUE)
  )
  h5results <- list(
    "Model Results" = model_results,
    "Standardized Model Results" = list(
      "STDYX Standardization" = within(model_results, Results <- Results * 0.1),
      "STDY Standardization" = within(model_results, Results <- Results * 0.2),
      "STD Standardization" = within(model_results, Results <- Results * 0.3)
    ),
    "R-Square" = list(
      Results = matrix(c(0.87654321, 0.012345678, 70.999999, 1e-12), nrow = 1L),
      Statements = matrix(c("Observed Variable", "Y1", "", "Y1"), nrow = 1L)
    )
  )

  outfile <- tempfile(fileext = ".out")
  h5file <- tempfile(fileext = ".h5")
  file.create(h5file)
  on.exit(unlink(c(outfile, h5file)))
  writeLines(c(
    readLines(testthat::test_path("ex3.1.out")),
    "", "SAVEDATA INFORMATION", "", "  Results in H5 Format", "",
    "  Save file", paste0("    ", basename(h5file))
  ), outfile)

  h5_attributes <- function(file, dataset) {
    list(Headings = if (grepl("Results$", dataset)) result_headings else statement_headings)
  }
  h5_reader <- function(file) h5results

  preferred <- testthat::with_mocked_bindings(
    MplusAutomation::readModels(outfile, what = "parameters", quiet = TRUE),
    read_h5 = h5_reader,
    read_h5_attributes = h5_attributes
  )
  text <- MplusAutomation::readModels(
    outfile, what = "parameters", quiet = TRUE, preferH5File = FALSE
  )

  expect_s3_class(preferred$parameters$unstandardized, "mplus.params")
  expect_equal(preferred$parameters$unstandardized$est[1L], 1.23456789, tolerance = 1e-12)
  expect_equal(preferred$parameters$unstandardized$paramHeader, c("Y1.ON", "Intercepts", "Residual.Variances"))
  expect_equal(preferred$parameters$unstandardized$param, c("X1", "Y1", "Y1"))
  expect_length(preferred$h5results, 0L)
  expect_false(isTRUE(all.equal(preferred$parameters$unstandardized$est[1L], text$parameters$unstandardized$est[1L])))
})

test_that("H5 standardized sections preserve established parameter names", {
  h5results <- list(
    "Standardized Model Results" = list(
      "STDYX Standardization" = list(
        Results = matrix(c(0.123456789, 0.012345678, 9.999999, 1e-10), nrow = 1L),
        Statements = matrix(c("", "Y1", "ON", "X1"), nrow = 1L)
      ),
      "STDY Standardization" = list(
        Results = matrix(c(0.234567891, 0.023456789, 10.0, 2e-10), nrow = 1L),
        Statements = matrix(c("", "Y1", "ON", "X1"), nrow = 1L)
      ),
      "STD Standardization" = list(
        Results = matrix(c(0.345678912, 0.034567891, 10.0, 3e-10), nrow = 1L),
        Statements = matrix(c("", "Y1", "ON", "X1"), nrow = 1L)
      )
    ),
    "R-Square" = list(
      Results = matrix(c(0.87654321, 0.012345678, 70.999999, 1e-12), nrow = 1L),
      Statements = matrix(c("Observed Variable", "Y1", "", "Y1"), nrow = 1L)
    )
  )

  parsed <- MplusAutomation:::extractH5StandardizedModelResults(h5results, "model.out")

  expect_equal(names(parsed), c("stdyx.standardized", "stdy.standardized", "std.standardized", "r2"))
  expect_s3_class(parsed$stdyx.standardized, "mplus.params")
  expect_equal(parsed$stdyx.standardized$est, 0.123456789, tolerance = 1e-12)
  expect_equal(parsed$r2$param, "Y1")
  expect_equal(parsed$r2$est, 0.87654321, tolerance = 1e-12)
})

test_that("unsupported H5 MODEL RESULTS layouts fall back to text parsing", {
  unsupported <- list("Model Results" = list(
    Results = matrix(1, nrow = 1L, ncol = 2L),
    Statements = matrix("Y", nrow = 1L, ncol = 2L)
  ))

  expect_null(MplusAutomation:::extractH5ModelResults(unsupported, "model.out"))
})

test_that("H5 odds-ratio results agree with the text-parsed output", {
  skip_if_not_installed("rhdf5")
  outfile <- testthat::test_path("mplus_ug", "8.11", "ch3", "ex3.6.out")

  h5_parameters <- MplusAutomation::readModels(
    outfile, what = "parameters", quiet = TRUE
  )$parameters
  text_parameters <- MplusAutomation::readModels(
    outfile, what = "parameters", quiet = TRUE, preferH5File = FALSE
  )$parameters

  for (section in c("odds", "ci.odds")) {
    expect_equal(h5_parameters[[section]][, c("paramHeader", "param")],
                 text_parameters[[section]][, c("paramHeader", "param")])
    expect_equal(
      h5_parameters[[section]][, -(1:2), drop = FALSE],
      text_parameters[[section]][, -(1:2), drop = FALSE],
      tolerance = 5.1e-4
    )
  }

  # The H5 source is authoritative: it preserves precision not printed in .out.
  expect_equal(h5_parameters$odds$est[1L], 2.1568, tolerance = 1e-7)
  expect_false(isTRUE(all.equal(
    h5_parameters$odds$est[1L], text_parameters$odds$est[1L]
  )))
})

test_that("H5 credibility intervals retain the established Bayesian schema", {
  result_headings <- c(
    "Lower .5%", "Lower 2.5%", "Lower 5%", "Estimate",
    "Upper 5%", "Upper 2.5%", "Upper .5%"
  )
  statement_headings <- c(
    "Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable"
  )
  credibility_section <- list(
    Results = matrix(c(-2.753151, -1.814369, -1.393716, 1.014612,
                       3.403152, 3.916317, 5.045601), nrow = 1L),
    Statements = matrix(c("", "Y", "ON", "X"), nrow = 1L)
  )
  h5results <- list("Credibility Intervals of Model Results" = credibility_section)

  parsed <- MplusAutomation:::extractH5ConfidenceIntervals(
    h5results, "bayes.out", result_headings, statement_headings,
    interval_type = "Credibility"
  )$ci.unstandardized

  expect_equal(names(parsed), c(
    "paramHeader", "param", "low.5", "low2.5", "low5", "est", "up5", "up2.5", "up.5"
  ))
  expect_equal(parsed$paramHeader, "Y.ON")
  expect_equal(parsed$param, "X")
  expect_equal(parsed$est, 1.014612, tolerance = 1e-12)

  bayes_results <- list(
    Results = matrix(c(1.014612, 1.475553, 0.242, -1.814369, 3.916317, 0), nrow = 1L),
    Statements = matrix(c("", "Y", "ON", "X"), nrow = 1L)
  )
  bayes_table <- MplusAutomation:::extractH5ResultTable(
    bayes_results, "bayes.out",
    c("Estimate", "Posterior S.D.", "One-Tailed P-Value", "Lower 2.5% C.I.",
      "Upper 2.5% C.I.", "Significance"),
    statement_headings
  )
  expect_equal(names(bayes_table), c(
    "paramHeader", "param", "est", "posterior_sd", "pval", "lower_2.5ci", "upper_2.5ci", "sig"
  ))
  expect_false(bayes_table$sig)
})

test_that("H5 probability-scale and IRT tables preserve parameter fields", {
  result_headings <- c("Estimate", "S.E.", "Estimate/S.E.", "Two-Tailed P-Value")
  statement_headings <- c(
    "Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable"
  )
  probability_results <- list(
    Results = matrix(c(0.654321, 0.037654, 17.375, 1e-12,
                       0.345679, 0.037654, 9.180, 2e-8), ncol = 4L, byrow = TRUE),
    Statements = matrix(c("U1", "Category 1", "", "",
                          "U1", "Category 2", "", ""), ncol = 4L, byrow = TRUE)
  )
  probability <- MplusAutomation:::extractH5ProbabilityScaleResults(
    list("Results in Probability Scale" = probability_results), "model.out",
    result_headings, statement_headings
  )$probability.scale
  expect_equal(names(probability), c("param", "category", "est", "se", "est_se", "pval"))
  expect_equal(probability$param, c("U1", "U1"))
  expect_equal(probability$category, c("1", "2"))
  expect_equal(probability$est[1L], 0.654321, tolerance = 1e-12)

  irt_results <- list(
    Results = matrix(c(0.918273, 0.155432, 5.908, 4e-9,
                       0.208765, 0.276543, 0.755, 0.450,
                       1, 0, 0, 1), ncol = 4L, byrow = TRUE),
    Statements = matrix(c("Item Discriminations", "F", "BY", "U1",
                          "Item Difficulties", "U1", "", "",
                          "Variances", "F", "", ""), ncol = 4L, byrow = TRUE)
  )
  irt <- MplusAutomation:::extractH5IRTParameterization(
    list("IRT Parameterization" = irt_results), "model.out",
    result_headings, statement_headings
  )$irt.parameterization
  expect_equal(irt$paramHeader, c("F.BY", "Item.Difficulties", "Variances"))
  expect_equal(irt$param, c("U1", "U1", "FALSE"))
  expect_equal(irt$est[1L], 0.918273, tolerance = 1e-12)
})
