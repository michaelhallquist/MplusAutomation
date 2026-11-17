additional_output_lines <- function() {
  c(
    "Mplus VERSION 9.1 (Mac)", "MUTHEN & MUTHEN", "",
    "INPUT INSTRUCTIONS",
    "  TITLE: additional output;",
    "  VARIABLE: NAMES = y;",
    "INPUT READING TERMINATED NORMALLY", "",
    "SUMMARY OF ANALYSIS",
    "Number of groups 1", "Number of observations 500",
    "Number of dependent variables 1", "Number of independent variables 0",
    "Number of continuous latent variables 0", "Estimator ML", "",
    "SUMMARY OF CENSORED LIMITS", "", "  Y 10.000", "",
    "COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES", "", "  U1 0.210 0 35", "",
    "THE MODEL ESTIMATION TERMINATED NORMALLY", "",
    "MODEL RESULTS", "",
    "                                                    Two-Tailed",
    "                    Estimate       S.E.  Est./S.E.    P-Value", "",
    " Means", "    Y 0.500 0.100 5.000 0.000", "",
    "QUALITY OF NUMERICAL RESULTS", "",
    "     Condition Number for the Information Matrix 0.200D-04", "",
    "CONFIDENCE INTERVALS OF MODEL RESULTS", "",
    "                  Lower .5%  Lower 2.5%    Lower 5%    Estimate    Upper 5%  Upper 2.5%   Upper .5%", "",
    " Means", "    Y 0.200 0.300 0.350 0.500 0.650 0.700 0.800", "",
    "CONFIDENCE INTERVALS IN PROBABILITY SCALE", "",
    "Latent Class 1", "",
    " U1", "    Category 1 0.700 0.750 0.780 0.821 0.850 0.880 0.900", "",
    "LATENT CLASS INDICATOR MEANS AND PROBABILITIES FOR EACH LATENT CLASS",
    "EVALUATED AT THE COVARIATE SAMPLE MEANS", "",
    "     MEAN/PROBABILITY PROFILES FOR C", "                   Latent class",
    "                     1      2",
    "     U1", "       Category 1 0.731 0.349", "       Category 2 0.269 0.651",
    "     F             2.518 -0.172", "",
    "LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES", "",
    "                                                95% C.I.",
    "                    Estimate       S.E.  Lower 2.5% Upper 2.5%", "",
    "Latent Class C1#1 Compared to Latent Class C1#2", "",
    " U1", "    Category > 1 4.100 0.200 3.700 4.500", "",
    " U2", "    Category = 2 2.100 0.100 1.900 2.300", "",
    "CONFIDENCE INTERVALS FOR THE LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES", "",
    "Latent Class C1#1 Compared to Latent Class C1#2", "",
    " U1", "    Category > 1 3.500 3.700 3.800 4.100 4.400 4.500 4.700", "",
    "BRANT WALD TEST FOR PROPORTIONAL ODDS", "",
    "                                   Degrees of",
    "                      Chi-Square     Freedom   P-Value", "",
    "  U1", "    Overall test 3.811 3 0.283", "    X 3.811 3 0.283", "",
    "LOGRANK OUTPUT", "",
    "     LOGRANK TEST FOR SURVIVAL VARIABLE T COMPARING CLASS 2 AGAINST CLASS 1", "",
    "          Chi-Square Value 64.136", "          Degrees of Freedom 1", "          P-value 0.000", "",
    "SUMMARIES OF PLAUSIBLE VALUES (N = NUMBER OF OBSERVATIONS * NUMBER OF IMPUTATIONS)", "",
    "     SAMPLE STATISTICS", "", "           Means",
    "              S             LOGV", "              ________      ________",
    "                0.286        -0.048", "", "           Covariances",
    "              S             LOGV", "              ________      ________",
    " S              0.034", " LOGV           0.042         0.102", "",
    "           Correlations", "              S             LOGV",
    "              ________      ________", " S              1.000", " LOGV           0.719         1.000", "",
    "SUMMARY OF PLAUSIBLE STANDARD DEVIATION (N = NUMBER OF OBSERVATIONS)", "",
    "     SAMPLE STATISTICS", "", "           Means",
    "              S_SD          LOGV_SD", "              ________      ________",
    "                0.095         0.045", "",
    "     Beginning Time:  10:00:00"
  )
}

test_that("additional sections are available through readModels and existing what selectors", {
  file <- tempfile(fileext = ".out")
  on.exit(unlink(file), add = TRUE)
  writeLines(additional_output_lines(), file)
  expect_silent(m <- readModels(file, what = c("summaries", "parameters", "sampstat", "fac_score_stats")))
  expect_equal(m$summaries$ConditionNumber, 2e-5)
  expect_equal(m$sampstat$censored.limits, data.frame(variable = "Y", limit = 10))
  expect_equal(m$sampstat$count.summary,
               data.frame(variable = "U1", zero_proportion = 0.210, minimum = 0, maximum = 35))
  p <- m$parameters
  expect_s3_class(p$ci.probability.scale, "mplus.params")
  expect_equal(p$ci.probability.scale$low2.5, 0.750)
  expect_equal(p$ci.probability.scale$LatentClass, "1")
  expect_equal(p$indicator.profiles$param, c(rep("U1", 4), "F", "F"))
  expect_equal(p$indicator.profiles$est, c(0.731, 0.349, 0.269, 0.651, 2.518, -0.172))
  expect_match(attr(p$indicator.profiles, "evaluated_at"), "COVARIATE SAMPLE MEANS")
  expect_equal(p$indicator.odds$category, c("> 1", "= 2"))
  expect_equal(p$indicator.odds$classA, rep("C1#1", 2))
  expect_equal(p$ci.indicator.odds$up2.5, 4.5)
  expect_equal(p$brant$test, c("Overall test", "X"))
  expect_equal(p$brant$chisq, c(3.811, 3.811))
  expect_equal(p$logrank$chisq, 64.136)
  expect_equal(p$logrank$classA, "2")
  expect_equal(m$fac_score_stats$plausible.values$Means[1, "S"], 0.286)
  expect_equal(m$fac_score_stats$plausible.values$Correlations["LOGV", "S"], 0.719)
  expect_equal(m$fac_score_stats$plausible.sd$Means[1, "S_SD"], 0.095)
})

test_that("supplemental H5 tables preserve keys and fall back when incomplete", {
  file <- tempfile(fileext = ".out")
  h5file <- tempfile(fileext = ".h5")
  on.exit(unlink(c(file, h5file)), add = TRUE)
  file.create(h5file)
  writeLines(c(additional_output_lines(), "", "SAVEDATA INFORMATION", "",
               "  Results in H5 Format", "", "  Save file", paste0("    ", basename(h5file))), file)
  statement_headings <- c("Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable")
  ci_headings <- c("Lower .5%", "Lower 2.5%", "Lower 5%", "Estimate", "Upper 5%", "Upper 2.5%", "Upper .5%")
  ci <- list(
    Results = matrix(c(0.7001, 0.7501, 0.7801, 0.8211, 0.8501, 0.8801, 0.9001), nrow = 1L),
    Statements = matrix(c("Latent Class 1", "U1", "Category 1", "", ""), nrow = 1L)
  )
  h5 <- list(
    "Model Results" = list(
      Results = matrix(c(0.5001, 0.1001, 5, 0), nrow = 1L),
      Statements = matrix(c("Means", "Y", "", ""), nrow = 1L)
    ),
    "Confidence Intervals of Model Results" = list(
      Results = matrix(c(0.2, 0.3, 0.35, 0.5001, 0.65, 0.7, 0.8), nrow = 1L),
      Statements = matrix(c("Means", "Y", "", ""), nrow = 1L)
    ),
    "Confidence Intervals in Probability Scale" = ci
  )
  attributes <- function(file, dataset) {
    headings <- if (grepl("Statements$", dataset)) {
      if (grepl("in Probability Scale", dataset)) c("Class", statement_headings) else statement_headings
    } else if (grepl("Intervals", dataset)) {
      ci_headings
    } else {
      c("Estimate", "S.E.", "Estimate/S.E.", "Two-Tailed P-Value")
    }
    list(Headings = headings)
  }
  for (core_supported in c(TRUE, FALSE)) {
    source <- h5
    if (!core_supported) source[["Model Results"]] <- NULL
    expect_silent(m <- testthat::with_mocked_bindings(
      readModels(file, what = "parameters"),
      read_h5 = function(...) source, read_h5_attributes = attributes
    ))
    expect_equal(m$parameters$ci.probability.scale$est, 0.8211)
    expect_true("indicator.profiles" %in% names(m$parameters))
  }
  source[["Confidence Intervals in Probability Scale"]]$Statements[1, 2] <- "OTHER"
  expect_silent(m <- testthat::with_mocked_bindings(
    readModels(file, what = "parameters"),
    read_h5 = function(...) source, read_h5_attributes = attributes
  ))
  expect_equal(m$parameters$ci.probability.scale$est, 0.821)
})

test_that("additional readers preserve missing values, wrapped profiles, and grouped counts", {
  lines <- c(
    "LATENT CLASS INDICATOR MEANS AND PROBABILITIES FOR EACH LATENT CLASS", "",
    "     MEAN/PROBABILITY PROFILES FOR C1", "          1 2",
    "     T", "       Category 1 ******** 0.400", "",
    "     MEAN/PROBABILITY PROFILES FOR C1", "          3",
    "     T", "       Category 1 0.600", "",
    "BRANT WALD TEST FOR PROPORTIONAL ODDS", "", "  T",
    "    Overall test ******** 1 0.000", "",
    "COUNT PROPORTION OF ZERO, MINIMUM AND MAXIMUM VALUES", "",
    "Group G1 (1)", "  U 0.100 0 10", "", "Group G2 (2)", "  U 0.200 0 20"
  )
  text <- MplusAutomation:::parse_into_sections(lines)
  p <- MplusAutomation:::extractAdditionalParameters(text, "wrapped.out")
  expect_equal(p$indicator.profiles$LatentClass, c("1", "2", "3"))
  expect_equal(p$indicator.profiles$est, c(NA_real_, 0.4, 0.6))
  expect_true(is.na(p$brant$chisq))
  sampstat <- MplusAutomation:::extractSampstat(text, "wrapped.out")
  expect_equal(sampstat$G1$count.summary$maximum, 10)
  expect_equal(sampstat$G2$count.summary$maximum, 20)
})

test_that("H5 indicator odds and Brant tables retain their identifying columns", {
  text <- MplusAutomation:::parse_into_sections(additional_output_lines())
  parsed <- MplusAutomation:::extractAdditionalParameters(text, "tables.out")
  comparison <- "Latent Class C1#1 Compared to Latent Class C1#2"
  odds_name <- "Latent Class Indicator Odds Ratios for the Latent Classes"
  brant_name <- "Brant Wald Test For Proportional Odds"
  h5 <- setNames(list(
    list(
      Results = rbind(c(4.10012, 0.20012, 3.70012, 4.50012), c(2.10012, 0.10012, 1.90012, 2.30012)),
      Statements = rbind(c(comparison, "U1", "Category > 1", "", ""),
                         c(comparison, "U2", "Category = 2", "", ""))
    ),
    list(
      Results = rbind(c(3.810545, 3, 0.2826619), c(3.810545, 3, 0.2826619)),
      Statements = rbind(c("U1", "Overall test"), c("U1", "X"))
    )
  ), c(odds_name, brant_name))
  result_headings <- setNames(list(c("Estimate", "S.E.", "Lower 2.5%", "Upper 2.5%"),
                                  c("Chi-Square", "Degrees of Freedom", "P-Value")),
                             c(odds_name, brant_name))
  statement_headings <- setNames(list(
    c("Comparison", "Section", "Left-hand variable/Variable/Parameter label", "Keyword", "Right-hand variable"),
    c("Variable", "Test/Variable")
  ), c(odds_name, brant_name))
  odds <- testthat::with_mocked_bindings(
    MplusAutomation:::preferH5AdditionalTable(parsed$indicator.odds, odds_name, h5, "tables.h5"),
    h5_result_headings = function(file, section) result_headings[[section]],
    h5_statement_headings = function(file, section) statement_headings[[section]]
  )
  expect_equal(odds$est, c(4.10012, 2.10012))
  expect_identical(odds$classA, parsed$indicator.odds$classA)
  expect_identical(odds$category, c("> 1", "= 2"))
  brant <- testthat::with_mocked_bindings(
    MplusAutomation:::preferH5AdditionalTable(parsed$brant, brant_name, h5, "tables.h5"),
    h5_result_headings = function(file, section) result_headings[[section]],
    h5_statement_headings = function(file, section) statement_headings[[section]]
  )
  expect_equal(brant$pval, rep(0.2826619, 2))
  expect_identical(brant$test, parsed$brant$test)
})

test_that("condition numbers follow their invariance models", {
  lines <- additional_output_lines()
  lines[lines == "QUALITY OF NUMERICAL RESULTS"] <- "QUALITY OF NUMERICAL RESULTS FOR THE CONFIGURAL MODEL"
  lines <- c(lines,
    "MODEL FIT INFORMATION FOR THE CONFIGURAL MODEL", "", "Number of Free Parameters 2", "",
    "MODEL FIT INFORMATION FOR THE METRIC MODEL", "", "Number of Free Parameters 1", "",
    "QUALITY OF NUMERICAL RESULTS FOR THE METRIC MODEL", "",
    "     Condition Number for the Information Matrix 0.453E-02"
  )
  file <- tempfile(fileext = ".out")
  on.exit(unlink(file), add = TRUE)
  writeLines(lines, file)
  expect_silent(m <- readModels(file, what = "summaries"))
  expect_equal(m$summaries$ConditionNumber, c(2e-5, 0.00453))
})

test_that("probability-scale credibility intervals retain the five-column layout", {
  lines <- additional_output_lines()
  lines <- sub("^CONFIDENCE INTERVALS OF MODEL RESULTS$", "CREDIBILITY INTERVALS OF MODEL RESULTS", lines)
  lines <- sub("^CONFIDENCE INTERVALS IN PROBABILITY SCALE$", "CREDIBILITY INTERVALS IN PROBABILITY SCALE", lines)
  lines[grepl("^ +Lower .5%", lines)] <- "                  Lower .5%  Lower 2.5%    Estimate    Upper 2.5%   Upper .5%"
  lines[lines == "    Y 0.200 0.300 0.350 0.500 0.650 0.700 0.800"] <- "    Y 0.200 0.300 0.500 0.700 0.800"
  lines[lines == "    Category 1 0.700 0.750 0.780 0.821 0.850 0.880 0.900"] <- "    Category 1 0.700 0.750 0.821 0.880 0.900"
  file <- tempfile(fileext = ".out")
  on.exit(unlink(file), add = TRUE)
  writeLines(lines, file)
  expect_silent(m <- readModels(file, what = "parameters"))
  ci <- m$parameters$ci.probability.scale
  expect_equal(ci$est, 0.821)
  expect_equal(ci$up2.5, 0.880)
  expect_false("low5" %in% names(ci))
})

test_that("installed examples retain nominal categories and plausible-value matrices", {
  root <- "/Applications/Mplus/Examples/usersguide/outputs"
  skip_if_not(dir.exists(root), "Mplus examples not installed")
  expect_silent(m <- readModels(file.path(root, "ex9.30.out"), what = "fac_score_stats"))
  expect_equal(m$fac_score_stats$plausible.values$Means[1, "S"], 0.286)
  expect_equal(m$fac_score_stats$plausible.sd$Means[1, "S_SD"], 0.095)
  expect_silent(m <- readModels(file.path(root, "ex7.12.out"), what = "parameters"))
  expect_equal(m$parameters$indicator.odds$est[1], 16.260)
  profiles <- m$parameters$indicator.profiles
  expect_equal(profiles$est[profiles$param == "U4" & profiles$category == "1" & profiles$LatentClass == "1"], 0.731)
  expect_silent(m <- readModels(file.path(root, "ex7.7.out"), what = "parameters"))
  expect_true(all(c("= 1", "= 2", "= 3") %in% m$parameters$indicator.odds$category))
})
