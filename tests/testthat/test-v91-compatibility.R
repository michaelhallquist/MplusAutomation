write_temp_out <- function(lines) {
  f <- tempfile(fileext = ".out")
  writeLines(lines, f)
  f
}

test_that("v9.1 ESEM, SEFA, and DSEFA STANDARDIZED SOLUTION parse without bleeding into R-SQUARE", {
  # 1. DSEFA with BI-FACTOR MODEL, prefixless ROTATED LOADINGS, FACTOR CORRELATIONS, and preceding R-SQUARE
  dsefa_lines <- c(
    "Mplus VERSION 9.1 (Mac)",
    "MUTHEN & MUTHEN",
    "",
    "INPUT INSTRUCTIONS",
    "  TITLE: DSEFA test;",
    "  DATA: FILE = ex.dat;",
    "  VARIABLE: NAMES = y1-y4;",
    "  ANALYSIS: ROTATION = DSEFA;",
    "  MODEL: f1-f2 BY y1-y4 (*1);",
    "INPUT READING TERMINATED NORMALLY",
    "",
    "SUMMARY OF ANALYSIS",
    "Number of groups                                                 1",
    "Number of observations                                         500",
    "Number of dependent variables                                    4",
    "Number of independent variables                                  0",
    "Number of continuous latent variables                            2",
    "Estimator                                                       ML",
    "",
    "THE MODEL ESTIMATION TERMINATED NORMALLY",
    "",
    "R-SQUARE",
    "",
    "    Observed                                        Two-Tailed",
    "    Variable        Estimate       S.E.  Est./S.E.    P-Value",
    "",
    "    Y1                 0.591      0.032     18.469      0.000",
    "    Y2                 0.742      0.028     26.500      0.000",
    "",
    "STANDARDIZED SOLUTION",
    "",
    "           BI-FACTOR MODEL",
    "               F1                F2",
    "              __________        __________",
    " Y1              0.557*            0.529*",
    " Y2              0.553*           -0.006",
    "",
    "           GEOMIN SIMPLICITY   0.63476",
    "",
    "           ROTATED LOADINGS (* significant at 5% level)",
    "               F1                F2",
    "              __________        __________",
    " Y1              0.019*            0.690*",
    " Y2             -0.005             0.862*",
    "",
    "           GEOMIN SIMPLICITY   0.36896",
    "",
    "           FACTOR CORRELATIONS (* significant at 5% level)",
    "               F1                F2",
    "              __________        __________",
    " F1              1.000",
    " F2              0.642*            1.000",
    "",
    "     Beginning Time:  10:00:00"
  )
  f_dsefa <- write_temp_out(dsefa_lines)
  on.exit(unlink(f_dsefa), add = TRUE)

  m <- MplusAutomation::readModels(f_dsefa, quiet = TRUE)
  expect_true(is.numeric(m$parameters$r2$est))
  expect_equal(m$parameters$r2$est, c(0.591, 0.742))

  esem <- m$parameters$esem.standardized
  expect_false(is.null(esem))
  expect_equal(dim(esem$bifactor$estimates), c(2L, 2L))
  expect_equal(esem$bifactor$estimates["Y1", "F1"], 0.557)
  expect_equal(esem$bifactor$sig["Y2", "F2"], FALSE)
  expect_equal(esem$bifactor$simplicity, 0.63476)
  expect_equal(esem$loadings$estimates["Y2", "F2"], 0.862)
  expect_equal(esem$corrs$estimates["F2", "F1"], 0.642)
  expect_equal(esem$simplicity, 0.36896)

  # 2. Multi-group ESEM STANDARDIZED SOLUTION (FOR G1 / FOR G2)
  mg_lines <- c(
    "STANDARDIZED SOLUTION",
    "",
    "           GEOMIN ROTATED LOADINGS (* significant at 5% level) FOR G1",
    "               F1                F2",
    "              __________        __________",
    " Y1              0.621*            0.003",
    " Y2             -0.026             0.649*",
    "",
    "           GEOMIN SIMPLICITY   0.69500",
    "",
    "           GEOMIN FACTOR CORRELATIONS (* significant at 5% level) FOR G1",
    "               F1                F2",
    "              __________        __________",
    " F1              1.000",
    " F2              0.507*            1.000",
    "",
    "           GEOMIN ROTATED LOADINGS (* significant at 5% level) FOR G2",
    "               F1                F2",
    "              __________        __________",
    " Y1              0.532*            0.008",
    " Y2             -0.021             0.707*",
    "",
    "           GEOMIN SIMPLICITY   0.64048",
    "",
    "           GEOMIN FACTOR CORRELATIONS (* significant at 5% level) FOR G2",
    "               F1                F2",
    "              __________        __________",
    " F1              1.000",
    " F2              0.542*            1.000",
    "",
    "     Beginning Time:  10:00:00"
  )
  parsed_mg <- MplusAutomation:::extractESEMStandardized(
    MplusAutomation:::parse_into_sections(mg_lines), "mg_esem.out"
  )
  expect_equal(names(parsed_mg), c("G1", "G2"))
  expect_equal(parsed_mg$G1$simplicity, 0.695)
  expect_equal(parsed_mg$G2$corrs$estimates["F2", "F1"], 0.542)
})

test_that("v9.1 R3STEP renamed headers and overflow asterisks parse cleanly", {
  r3_lines <- c(
    "CONFIDENCE INTERVALS IN PROBABILITY SCALE",
    "",
    "Latent Class 1",
    "",
    " U1",
    "    Category 1       0.017       0.040       0.051       0.113       0.174       0.186       0.209",
    "    Category 2       0.791       0.814       0.826       0.887       0.949       0.960       0.983",
    "",
    "CONFIDENCE INTERVALS FOR THE LATENT CLASS INDICATOR ODDS RATIOS FOR THE LATENT CLASSES",
    "",
    "Latent Class 1 Compared to Latent Class 2",
    " U1",
    "    Category > 1    22.352      28.708      32.629      63.673     124.251     141.221     181.380",
    "",
    "CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS USING",
    "THE 3-STEP (FIXED LOGITS) PROCEDURE",
    "",
    "                                                    Two-Tailed",
    "                    Estimate       S.E.  Est./S.E.    P-Value",
    "",
    " C#1        ON",
    "    X1                 1.250      0.210      5.952      0.000",
    "",
    " Intercepts",
    "    C#1               -0.500      0.150     -3.333      0.001",
    "",
    "Parameterization using Reference Class 1",
    "",
    " C#2        ON",
    "    X1                -1.250      0.210     -5.952      0.000",
    "",
    " Intercepts",
    "    C#2                0.500      0.150      3.333      0.001",
    "",
    "ODDS RATIOS FOR CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS",
    "USING THE 3-STEP (FIXED LOGITS) PROCEDURE",
    "",
    "                                                95% C.I.",
    "                    Estimate       S.E. Lower 2.5% Upper 2.5%",
    "",
    " C#1        ON",
    "    X1                 3.490      0.733      2.312      5.268",
    "",
    "Parameterization using Reference Class 1",
    "",
    " C#2        ON",
    "    X1                 0.287      0.060      0.190      0.433",
    "",
    "CONFIDENCE INTERVALS FOR CATEGORICAL LATENT VARIABLE MULTINOMIAL LOGISTIC REGRESSIONS",
    "USING THE 3-STEP (FIXED LOGITS) PROCEDURE",
    "",
    "                  Lower .5%  Lower 2.5%    Lower 5%    Estimate    Upper 5%  Upper 2.5%   Upper .5%",
    "",
    " C#1        ON",
    "    X1               0.709       0.838       0.905       1.250       1.595       1.662       1.791",
    "",
    "Parameterization using Reference Class 1",
    "",
    " C#2        ON",
    "    X1              -1.791      -1.662      -1.595      -1.250      -0.905      -0.838      -0.709",
    "",
    "CONFIDENCE INTERVALS OF ODDS RATIOS FOR CATEGORICAL LATENT VARIABLE MULTINOMIAL",
    "LOGISTIC REGRESSIONS USING THE 3-STEP (FIXED LOGITS) PROCEDURE",
    "",
    " C#1        ON",
    "    X1           *********  **********  **********  **********  **********  **********  **********",
    "",
    "Parameterization using Reference Class 1",
    "",
    " C#2        ON",
    "    X1               0.167       0.190       0.203       0.287       0.405       0.433       0.492",
    "",
    "     Beginning Time:  10:00:00"
  )

  sec <- MplusAutomation:::parse_into_sections(r3_lines)
  r3 <- MplusAutomation:::extractR3step(sec, "r3step91.out")

  expect_setequal(names(r3), c("multinomialTests", "multinomialOdds", "ci.unstandardized", "ci.odds"))
  expect_equal(nrow(r3$multinomialTests), 4L)
  expect_equal(r3$multinomialTests$ReferenceClass, c("2", "2", "1", "1"))
  expect_true(is.numeric(r3$ci.odds$est))
  expect_true(is.na(r3$ci.odds$est[1L]))
  expect_equal(r3$ci.odds$est[2L], 0.287)
})

test_that("v9.1 auxiliary D3STEP and D3STEPC step-shift tables do not corrupt equality tests", {
  aux_lines <- c(
    "EQUALITY TESTS OF MEANS/PROBABILITIES ACROSS CLASSES",
    "",
    "X1",
    "",
    "   NUMBER OF DELETED OBSERVATIONS FOR THE AUXILIARY VARIABLE:  0",
    "   NUMBER OF OBSERVATIONS USED FOR THE AUXILIARY VARIABLE:  500",
    "",
    "                          Mean       S.E.",
    "",
    "  Class 1                1.970      0.091",
    "  Class 2               -2.055      0.052",
    "",
    "                    Chi-Square     P-Value   Degrees of Freedom",
    "",
    "  Overall test        1513.545      0.000        1",
    "",
    "  Final Class Counts and Proportions for the Latent Class Patterns",
    "  Based On Estimated Posterior Probabilities for X1:  First Step vs. Last Step",
    "",
    "    Latent",
    "   Classes         First Step                  Last Step",
    "",
    "       1        132.04593   0.26409        131.94829   0.26390",
    "       2        367.95407   0.73591        368.05171   0.73610",
    "",
    "  Classification Probabilities for the Step 1 Most Likely Latent Class Membership (Row)",
    "  by Step 3 Most Likely Latent Class Membership (Column) for X1",
    "",
    "           1        2",
    "",
    "    1   0.901    0.099",
    "    2   0.014    0.986",
    "",
    "U4",
    "",
    "   NUMBER OF DELETED OBSERVATIONS FOR THE AUXILIARY VARIABLE:  0",
    "   NUMBER OF OBSERVATIONS USED FOR THE AUXILIARY VARIABLE:  500",
    "",
    "                          Prob       S.E.  Odds Ratio      S.E.   2.5% C.I.  97.5% C.I.",
    "",
    "  Class 1",
    "    Category 1           0.891      0.035      1.000      0.000      1.000      1.000",
    "    Category 2           0.109      0.035      0.019      0.008      0.008      0.042",
    "  Class 2",
    "    Category 1           0.134      0.019      1.000      0.000      1.000      1.000",
    "    Category 2           0.866      0.019      1.000      0.000      1.000      1.000",
    "",
    "                    Chi-Square     P-Value   Degrees of Freedom",
    "",
    "  Overall test          93.766      0.000        1",
    "",
    "  Final Class Counts and Proportions for the Latent Class Patterns",
    "  Based On Estimated Posterior Probabilities for U4:  First Step vs. Last Step",
    "",
    "    Latent",
    "   Classes         First Step                  Last Step",
    "",
    "       1        132.04593   0.26409        132.04614   0.26409",
    "       2        367.95407   0.73591        367.95386   0.73591",
    "",
    "  Classification Probabilities for the Step 1 Most Likely Latent Class Membership (Row)",
    "  by Step 3 Most Likely Latent Class Membership (Column) for U4",
    "",
    "           1        2",
    "",
    "    1   1.000    0.000",
    "    2   0.000    1.000",
    "",
    "     Beginning Time:  10:00:00"
  )

  sec <- MplusAutomation:::parse_into_sections(aux_lines)
  aux <- MplusAutomation:::extractAux(sec, "aux91.out")

  expect_s3_class(aux, "mplus.auxE")
  expect_equal(aux$overall$chisq, "1513.545")
  expect_equal(nrow(aux$U4$tests), 1L)
  expect_equal(aux$U4$tests$versus, "Overall")
  expect_equal(aux$U4$tests$chi_square, "93.766")
  expect_equal(aux$U4$tests$df, "1")
  expect_equal(aux$step_shift$X1$counts$last_step_count, c(131.94829, 368.05171))
  expect_equal(aux$step_shift$X1$classification[1, 2], 0.099)
  expect_equal(aux$step_shift$U4$classification[2, 2], 1.000)
})

test_that("v9.1 mixture class equality tests, LTA transition odds, Overall Part, and dual odds ratios parse cleanly", {
  mix_lines <- c(
    "MODEL RESULTS",
    "",
    "                                                    Two-Tailed",
    "                    Estimate       S.E.  Est./S.E.    P-Value",
    "",
    "Parameters in the Overall Part of the Model (Parameters Equal in All of the Classes)",
    "",
    " F        BY",
    "    Y1                 1.000      0.000    999.000    999.000",
    "",
    " Variances",
    "    F                  0.812      0.091      8.923      0.000",
    "",
    "Parameters for Class-specific Model Parts",
    "",
    "Latent Class Pattern 1 1",
    "",
    " Means",
    "    F                  1.200      0.100     12.000      0.000",
    "",
    "Latent Class Pattern 1 2",
    "",
    " Means",
    "    F                 -0.500      0.110     -4.545      0.000",
    "",
    "RESULTS IN PROBABILITY SCALE",
    "",
    "Latent Class 1",
    "",
    " U1",
    "    Category 1         0.850      0.025     34.000      0.000",
    "    Category 2         0.150      0.025      6.000      0.000",
    "",
    "EQUALITY TESTS OF MEANS ACROSS THE LATENT CLASSES",
    "",
    "                    Chi-Square     P-Value   Degrees of Freedom",
    "",
    "Y1",
    "  Overall test        797.523      0.000        2",
    "  Class 1 vs. 2       239.955      0.000        1",
    "  Class 1 vs. 3       795.906      0.000        1",
    "  Class 2 vs. 3       223.415      0.000        1",
    "",
    "TRANSITION PROBABILITY ODDS",
    "",
    "     EVALUATED AT THE SAMPLE MEAN FOR ALL COVARIATES",
    "",
    "     TRANSITION TABLE ODDS AND 95% CONFIDENCE INTERVALS FOR C1 TO C2",
    "        1.000(1.000,1.000)   1.209(0.895,1.633)",
    "        0.827(0.612,1.117)   1.000(1.000,1.000)",
    "",
    "COVARIATE EFFECTS ON TRANSITION PROBABILITY ODDS RATIOS",
    "",
    "     EFFECT OF F",
    "",
    "     TRANSITION TABLE ODDS RATIO AND 95% CONFIDENCE INTERVALS FOR C1 TO C2",
    "         1.000( 1.000, 1.000)   10.520( 2.498,44.298)",
    "         2.365( 1.855, 3.014)    1.000( 1.000, 1.000)",
    "",
    "LOGISTIC REGRESSION ODDS RATIO RESULTS FOR OBSERVED VARIABLES",
    "",
    "                                                95% C.I.",
    "                    Estimate       S.E.  Lower 2.5% Upper 2.5%",
    "",
    "Latent Class 1",
    "",
    " U4         ON",
    "    X                  1.784      0.229      1.387      2.293",
    "",
    "LOGISTIC REGRESSION ODDS RATIO RESULTS FOR LATENT CLASS VARIABLES",
    "",
    "                                                95% C.I.",
    "                    Estimate       S.E.  Lower 2.5% Upper 2.5%",
    "",
    " C1#1     ON",
    "    F                  0.359      0.054      0.267      0.482",
    "",
    "CONFIDENCE INTERVALS OF MODEL RESULTS",
    "",
    "                  Lower .5%  Lower 2.5%    Lower 5%    Estimate    Upper 5%  Upper 2.5%   Upper .5%",
    "",
    " Variances",
    "    F                 0.578       0.634       0.662       0.812       0.962       0.990       1.046",
    "",
    "CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS FOR LATENT CLASS VARIABLES",
    "",
    " C1#1     ON",
    "    F                 0.243       0.267       0.280       0.359       0.460       0.482       0.530",
    "",
    "CONFIDENCE INTERVALS FOR THE ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION",
    "",
    "Parameterization using Reference Class 1",
    "",
    " Intercepts",
    "    C1#2             -0.550      -0.402      -0.327       0.067       0.460       0.536       0.683",
    "",
    "     Beginning Time:  10:00:00"
  )

  sec <- MplusAutomation:::parse_into_sections(mix_lines)
  params <- MplusAutomation:::extractParameters_1file(sec, "mix91.out")

  # 1. Overall Part + "F" parameter preserved
  expect_equal(nrow(params$unstandardized), 4L)
  expect_equal(params$unstandardized$LatentClass[1:2], c("Overall", "Overall"))
  expect_equal(params$unstandardized$param, c("Y1", "F", "F", "F"))

  # 2. Probability scale not corrupted by EQUALITY TESTS
  expect_true(is.numeric(params$probability.scale$est))
  expect_equal(params$probability.scale$est, c(0.850, 0.150))

  # 3. Class equality tests (overall + pairwise)
  expect_equal(nrow(params$class.equality), 4L)
  expect_equal(params$class.equality$test_type, c("overall", "pairwise", "pairwise", "pairwise"))
  expect_equal(params$class.equality$chisq[1L], 797.523)
  expect_equal(attr(params$class.equality, "parameter_type"), "means")

  # 4. Transition odds & covariate effects (with spaces inside parentheses)
  expect_equal(nrow(params$transition.odds$overall), 4L)
  expect_equal(params$transition.odds$overall$odds[2L], 1.209)
  expect_equal(params$transition.odds$covariate.effects$covariate[1L], "F")
  expect_equal(params$transition.odds$covariate.effects$odds_ratio[2L], 10.520)
  expect_equal(params$transition.odds$covariate.effects$upper_2.5ci[2L], 44.298)

  # 5. Dual logistic odds ratios combined with preserved "F"
  expect_equal(nrow(params$odds), 2L)
  expect_equal(params$odds$param, c("X", "F"))
  expect_equal(params$odds$LatentClass, c("1", "Categorical.Latent.Variables"))
  expect_equal(params$ci.odds$paramHeader, "C1#1.ON")
  expect_equal(params$ci.odds$est, 0.359)
})

test_that("v9.1 Bayes TECH8 ESS, Penalized LL, VIF overflow, standalone R-SQUARE, and invariance (MODEL) syntax work", {
  out_lines <- c(
    "Mplus VERSION 9.1 (Mac)",
    "MUTHEN & MUTHEN",
    "",
    "INPUT INSTRUCTIONS",
    "  TITLE: Combined v9.1 edge cases;",
    "  DATA: FILE = ex.dat;",
    "  VARIABLE: NAMES = y1 x1;",
    "  ANALYSIS: ESTIMATOR = BAYES;",
    "  MODEL: y1 ON x1;",
    "INPUT READING TERMINATED NORMALLY",
    "",
    "SUMMARY OF ANALYSIS",
    "Number of groups                                                 1",
    "Number of observations                                         200",
    "Number of dependent variables                                    1",
    "Number of independent variables                                  1",
    "  Specifications for Data Imputation",
    "  Number of dependent variables                                  2",
    "  Number of independent variables                                0",
    "Estimator                                                      MLR",
    "",
    "UNIVARIATE SAMPLE STATISTICS",
    "",
    "     UNIVARIATE HIGHER-ORDER MOMENT DESCRIPTIVE STATISTICS",
    "",
    "         Variable/         Mean/     Skewness/   Minimum/ % with                Percentiles",
    "        Sample Size      Variance    Kurtosis    Maximum  Min/Max      20%/60%    40%/80%     Median",
    "",
    "     Y1                    0.012      -0.050      -2.500    0.50%      -0.800     -0.250      0.010",
    "             200.000       1.020      -0.120       2.600    0.50%       0.260      0.840",
    "",
    "     VIF (VARIANCE INFLATION FACTOR) FOR EXPLORING MULTICOLLINEARITY OF INDEPENDENT VARIABLES",
    "",
    "     Variable        VIF       Variable        VIF",
    "",
    "     TIMET       ********      X1            1.250",
    "",
    "WARNING:  CLUSTER HAS ZERO VARIANCE",
    "          18",
    "",
    "THE MODEL ESTIMATION TERMINATED NORMALLY",
    "",
    "     USE THE FBITERATIONS OPTION TO INCREASE THE NUMBER OF ITERATIONS BY A FACTOR",
    "     OF AT LEAST TWO TO CHECK CONVERGENCE AND THAT THE PSR VALUE DOES NOT INCREASE.",
    "",
    "MODEL FIT INFORMATION",
    "",
    "Number of Free Parameters                        3",
    "",
    "Penalized Loglikelihood (loglikelihood + penalty)",
    "",
    "          H0 Value                      -64395.226",
    "          H0 Scaling Correction Factor      0.9587",
    "            for MLR",
    "",
    "R-SQUARE",
    "",
    "    Observed                                        Two-Tailed",
    "    Variable        Estimate       S.E.  Est./S.E.    P-Value",
    "    Y1                 0.312      0.041      7.610      0.000",
    "",
    "TECHNICAL 8 OUTPUT",
    "",
    "     TECHNICAL 8 OUTPUT FOR BAYES ESTIMATION",
    "",
    "     CHAIN    ITERATION    POTENTIAL       PARAMETER WITH",
    "                         SCALE REDUCTION   HIGHEST PSR",
    "         1        100          1.012              2",
    "",
    "     MCMC EFFECTIVE SAMPLE SIZE (ESS) IN ASCENDING ORDER",
    "        LOWEST 10 PARAMETERS",
    "        PARAMETER    ESS",
    "             4       <20",
    "             2        41",
    "             3       195",
    "             1      1200",
    "",
    "     Beginning Time:  10:00:00"
  )

  f <- write_temp_out(out_lines)
  on.exit(unlink(f), add = TRUE)
  m <- MplusAutomation::readModels(f, quiet = TRUE)

  expect_equal(m$summaries$NDependentVars, 1L)
  expect_equal(m$summaries$NIndependentVars, 1L)
  expect_equal(m$summaries$LL, -64395.226)
  expect_equal(m$summaries$LLCorrectionFactor, 0.9587)
  expect_equal(nrow(m$sampstat$univariate.sample.statistics), 1L)
  expect_equal(m$sampstat$vif$variable, c("TIMET", "X1"))
  expect_true(is.na(m$sampstat$vif$vif[1L]))
  expect_equal(m$sampstat$vif$vif[2L], 1.250)
  expect_equal(m$parameters$r2$est, 0.312)
  expect_s3_class(m$tech8$ess, "mplus.ess.data.frame")
  expect_equal(m$tech8$ess$parameter, c(4L, 2L, 3L, 1L))
  expect_equal(m$tech8$ess$ess, c(NA_real_, 41, 195, 1200))
  expect_equal(m$tech8$ess$upper_bound, c(20, NA_real_, NA_real_, NA_real_))
  expect_length(m$errors, 0L)
  expect_gte(length(m$warnings), 1L)

  # Measurement invariance fit + (MODEL) syntax
  inv_lines <- c(
    "Mplus VERSION 9.1 (Mac)",
    "MUTHEN & MUTHEN",
    "",
    "INPUT INSTRUCTIONS",
    "  TITLE: Invariance model test;",
    "  DATA: FILE = ex.dat;",
    "  VARIABLE: NAMES = y1-y3 g; GROUPING = g (1=g1 2=g2);",
    "  ANALYSIS: MODEL = CONFIGURAL METRIC (MODEL);",
    "  MODEL: f1 BY y1-y3;",
    "INPUT READING TERMINATED NORMALLY",
    "",
    "SUMMARY OF ANALYSIS",
    "Number of groups                                                 2",
    "Total sample size                                              500",
    "Number of dependent variables                                    3",
    "Number of independent variables                                  0",
    "Estimator                                                       ML",
    "",
    "THE MODEL ESTIMATION TERMINATED NORMALLY",
    "",
    "Invariance Testing",
    "",
    "                             Number of     Chi-Square  Degrees of",
    "     Model                  Parameters          Value     Freedom     P-Value",
    "       Configural                   38         22.212          16      0.1365",
    "       Metric                       34         26.395          20      0.1532",
    "",
    "                                           Chi-Square  Degrees of",
    "     Models Compared                            Value     Freedom     P-Value",
    "       Metric against Configural                4.183           4      0.3818",
    "",
    "",
    "MODEL FIT INFORMATION FOR THE CONFIGURAL MODEL",
    "",
    "Number of Free Parameters                       38",
    "",
    "CFI/TLI",
    "",
    "          CFI                                0.989",
    "          TLI                                0.980",
    "",
    "MODEL FIT INFORMATION FOR THE METRIC MODEL",
    "",
    "Number of Free Parameters                       34",
    "",
    "CFI/TLI",
    "",
    "          CFI                                0.988",
    "          TLI                                0.982",
    "",
    "CONFIGURAL MODEL COMMAND FOR INVARIANCE ANALYSIS",
    "",
    "  MODEL:",
    "     f1 BY y1 y2 y3;",
    "",
    "METRIC MODEL COMMAND FOR INVARIANCE ANALYSIS",
    "",
    "  MODEL:",
    "     f1 BY y1 y2 y3;",
    "     [ f1@0 ];",
    "",
    "     Beginning Time:  10:00:00"
  )
  f_inv <- write_temp_out(inv_lines)
  on.exit(unlink(f_inv), add = TRUE)
  m_inv <- MplusAutomation::readModels(f_inv, quiet = TRUE)

  expect_equal(nrow(m_inv$summaries), 2L)
  expect_equal(m_inv$summaries$CFI, c(0.989, 0.988))
  expect_equal(names(m_inv$invariance_testing$model_syntax), c("CONFIGURAL", "METRIC"))
  expect_true(any(grepl("f1 BY y1 y2 y3;", m_inv$invariance_testing$model_syntax$CONFIGURAL, fixed = TRUE)))
})

test_that("installed Mplus 9.1 example outputs parse cleanly when available", {
  ug_dir <- "/Applications/Mplus/Examples/usersguide/outputs"
  mc_dir <- "/Applications/Mplus/Examples/montecarlo/outputs"
  skip_if_not(dir.exists(ug_dir) && dir.exists(mc_dir), "Mplus 9.1 Examples not installed")

  target_files <- c(
    file.path(ug_dir, c(
      "ex3.4.out", "ex3.18.out", "ex4.1part2.out", "ex4.4.out",
      "ex4.5.out", "ex5.5part3.out", "ex5.26.out", "ex5.27.out",
      "ex7.3.out", "ex7.12.out", "ex7.18.out", "ex7.22.out",
      "ex8.7.out", "ex8.8.out", "ex8.14.out",
      "ex9.32.out", "ex9.39b.out",
      "ex10.1.out", "ex11.5.out", "ex11.7.out", "ex11.8part2.out",
      "ex12.6step2.out", "ex12.12.out"
    )),
    file.path(mc_dir, c("mcex4.6.out", "mcex5.14.out", "mcex9.20.out"))
  )
  target_files <- target_files[file.exists(target_files)]

  for (f in target_files) {
    msgs <- character()
    expect_no_warning({
      m <- withCallingHandlers(
        MplusAutomation::readModels(f, quiet = TRUE),
        message = function(msg) {
          msgs <<- c(msgs, conditionMessage(msg))
          invokeRestart("muffleMessage")
        }
      )
    })
    expect_length(grep("^Error (extracting|parsing|reading)", msgs), 0L)
    expect_s3_class(m, "mplus.model")
  }
})

test_that("installed EFA examples retain the last rotation and each class", {
  ug_dir <- "/Applications/Mplus/Examples/usersguide/outputs"
  files <- file.path(ug_dir, c("ex4.4.out", "ex4.5.out", "ex8.15.out"))
  skip_if_not(all(file.exists(files)), "Mplus 9.1 Examples not installed")

  efa <- readModels(files[1L], what = "parameters")$parameters$efa
  expect_named(efa, c("f1_CLASS1", "f1_CLASS2", "f2_CLASS1", "f2_CLASS2"))
  expect_equal(efa$f2_CLASS2$loadings$estimates["Y1", "1"], 0.788)

  efa <- readModels(files[2L], what = "parameters")$parameters$efa
  expect_equal(efa$wi2_bw0_WITHIN$loadings$estimates["Y1", "1"], 0.798)

  lta <- readModels(files[3L], what = c("summaries", "class_counts"))
  expect_equal(lta$summaries$NLatentClasses, 54L)
  expect_equal(nrow(lta$class_counts$modelEstimated.patterns), 54L)
})

test_that("rotated TECH1 sections preserve the legacy top-level fields", {
  f <- test_path("mplus_ug", "8.11", "ch5", "ex5.26.out")
  tech1 <- readModels(f, what = "tech1")$tech1
  expect_s3_class(tech1$parameterSpecification, "mplus.parameterSpecification")
  expect_s3_class(tech1$startingValues, "mplus.startingValues")
  expect_identical(tech1$parameterSpecification$X, tech1$rotated$parameterSpecification)
  expect_s3_class(tech1$unrotated, "mplus.tech1")
})

test_that("mixtures without joint pattern tables retain the marginal class count", {
  f <- test_path("mplus_ug", "8.11", "ch7", "ex7.15.out")
  m <- readModels(f, what = c("summaries", "class_counts"))
  expect_equal(m$summaries$NLatentClasses, 6L)
})
