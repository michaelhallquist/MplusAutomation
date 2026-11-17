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

  root <- tempfile()
  dir.create(file.path(root, "results"), recursive = TRUE)
  outfile <- file.path(root, "model.out")
  h5file <- file.path(root, "results", "model.h5")
  file.create(h5file)
  on.exit(unlink(root, recursive = TRUE))
  writeLines(c(
    readLines(testthat::test_path("ex3.1.out")),
    "", "SAVEDATA INFORMATION", "", "  Results in H5 Format", "",
    "  Save file", "    results/model.h5"
  ), outfile)

  h5_attributes <- function(file, dataset) {
    list(Headings = if (grepl("Results$", dataset)) result_headings else statement_headings)
  }
  h5_reader <- function(file) {
    expect_identical(
      normalizePath(file, winslash = "/", mustWork = TRUE),
      normalizePath(h5file, winslash = "/", mustWork = TRUE)
    )
    h5results
  }

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
  expect_null(MplusAutomation:::extractH5ModelResults(
    unsupported, "model.out",
    result_headings = c("Estimate", "S.E.", "Two-Tailed P-Value"),
    statement_headings = c("Section", "Variable", "Keyword", "Right-hand variable")
  ))
})

test_that("H5 R-square retains the columns shown in text output", {
  filename <- "probit.out"
  section <- c(
    "    Observed                   Residual",
    "    Variable        Estimate   Variance",
    "    U1                 0.877      1.000",
    "    U2             Undefined"
  )
  h5 <- list("R-Square" = list(
    Results = rbind(c(0.8773808, 1), c(999, 999)),
    Statements = rbind(
      c("Observed Variable", "U1", "", ""),
      c("Observed Variable", "U2", "", "")
    )
  ))
  r2 <- testthat::with_mocked_bindings(
    MplusAutomation:::extractH5RSquare_1file(section, h5, "probit.h5", filename),
    h5_result_headings = function(...) c("Estimate", "Residual Variance"),
    h5_statement_headings = function(...) c("Section", "Variable", "Keyword", "Right-hand variable")
  )
  expect_named(r2, c("param", "est", "resid_var"))
  expect_equal(r2$param, c("U1", "U2"))
  expect_equal(r2$est, c(0.8773808, NA_real_))
  expect_equal(r2$resid_var, c(1, NA_real_))
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
  expect_equal(irt$param, c("U1", "U1", "F"))
  expect_equal(irt$est[1L], 0.918273, tolerance = 1e-12)
})

test_that("stratified H5 statement headings fall back instead of dropping Group/Class/Level", {
  result_headings <- c("Estimate", "S.E.", "Estimate/S.E.", "Two-Tailed P-Value")
  base_stmt <- c("Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable")
  stratified_section <- list(
    Results = matrix(c(-2.063, 0.373, -5.536, 0,
                       2.091, 0.182, 11.502, 0), ncol = 4L, byrow = TRUE),
    Statements = matrix(c("Latent Class 1", "Thresholds", "U1$1", "", "",
                          "Latent Class 2", "Thresholds", "U1$1", "", ""), ncol = 5L, byrow = TRUE)
  )

  for (col in c("Class", "Group", "Level")) {
    expect_null(MplusAutomation:::extractH5ModelResults(
      list("Model Results" = stratified_section),
      "stratified.out",
      result_headings = result_headings,
      statement_headings = c(col, base_stmt)
    ))
  }

  odds_section <- list(
    Results = matrix(c(2.15, 0.31, 1.62, 2.85), nrow = 1L),
    Statements = matrix(c("Group G1", "", "U1", "ON", "X1"), nrow = 1L)
  )
  expect_null(MplusAutomation:::extractH5OddsRatioResults(
    list("Logistic Regression Odds Ratio Results" = odds_section),
    "grouped_odds.out",
    result_headings = c("Estimate", "S.E.", "Lower 2.5%", "Upper 2.5%"),
    statement_headings = c("Group", base_stmt)
  ))

  prob_grouped <- list(
    Results = matrix(c(0.65, 0.04, 16.25, 0), nrow = 1L),
    Statements = matrix(c("Group G1", "U1", "Category 1", "", ""), nrow = 1L)
  )
  expect_null(MplusAutomation:::extractH5ProbabilityScaleResults(
    list("Results in Probability Scale" = prob_grouped),
    "grouped_prob.out",
    result_headings = result_headings,
    statement_headings = c("Group", base_stmt)
  ))
})

test_that("H5 R-square guards uninitialized THETA Scale Factors and parses Bayesian R-square headings", {
  theta_section <- c(
    "    Observed                                        Two-Tailed     Scale",
    "    Variable    Estimate       S.E.  Est./S.E.   P-Value        Factors",
    "    U1             0.800      0.046     17.436      0.000          0.447"
  )
  h5_theta <- list("R-Square" = list(
    Results = matrix(c(0.7999982, 0.04588321, 17.43553, 0, 1.356316e-19), nrow = 1L),
    Statements = matrix(c("Observed Variable", "U1", "", "U1"), nrow = 1L)
  ))
  r2_theta <- testthat::with_mocked_bindings(
    MplusAutomation:::extractH5RSquare_1file(theta_section, h5_theta, "theta.h5", "theta.out"),
    h5_result_headings = function(...) c("Estimate", "S.E.", "Estimate/S.E.", "Two-Tailed P-Value", "Scale Factors"),
    h5_statement_headings = function(...) c("Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable")
  )
  expect_equal(r2_theta$est, 0.7999982, tolerance = 1e-7)
  expect_equal(r2_theta$scale_f, 0.447)

  bayes_section <- c(
    "                                Posterior  One-Tailed         95% C.I.",
    "    Variable        Estimate       S.D.      P-Value   Lower 2.5%  Upper 2.5%",
    "    Y1                 0.457       0.044      0.000       0.374       0.553"
  )
  h5_bayes <- list("R-Square" = list(
    Results = matrix(c(0.4572245, 0.04361657, 0, 0.3735053, 0.5531552), nrow = 1L),
    Statements = matrix(c("Observed Variable", "Y1", "", "Y1"), nrow = 1L)
  ))
  r2_bayes <- testthat::with_mocked_bindings(
    MplusAutomation:::extractH5RSquare_1file(bayes_section, h5_bayes, "bayes.h5", "bayes.out"),
    h5_result_headings = function(...) c("Estimate", "Posterior S. D.", "One-Tailed P-Value", "Lower 2.5% CI", "Upper 2.5% CI"),
    h5_statement_headings = function(...) c("Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable")
  )
  expect_equal(r2_bayes$est, 0.4572245, tolerance = 1e-7)
  expect_equal(r2_bayes$posterior_sd, 0.04361657, tolerance = 1e-8)
  expect_equal(r2_bayes$lower_2.5ci, 0.3735053, tolerance = 1e-7)
  expect_equal(r2_bayes$upper_2.5ci, 0.5531552, tolerance = 1e-7)

  bayes_std <- MplusAutomation:::extractH5ResultTable(
    list(
      Results = matrix(c(0.6761838, 0.03223832, 0, 0.6111508, 0.7437440,
                         0.0500000, 0.08000000, 0.26, -0.1000000, 0.2000000), ncol = 5L, byrow = TRUE),
      Statements = matrix(c("", "F1", "BY", "Y1",
                            "", "F1", "BY", "Y2"), ncol = 4L, byrow = TRUE)
    ),
    "bayes_std.out",
    c("Estimate", "Posterior S.D.", "One-Tailed P-Value", "Lower 2.5% C.I.", "Upper 2.5% C.I."),
    c("Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable")
  )
  expect_equal(bayes_std$sig, c(TRUE, FALSE))
})
