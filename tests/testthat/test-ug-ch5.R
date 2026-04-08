# ch5 - factor analysis, SEM, invariance, EFA, and IRT
mv <- "8.11"

read_ch5_model <- function(fname, ...) {
  MplusAutomation::readModels(
    target = get_mplus_file(file.path("ch5", fname), mplus_version = mv),
    quiet = TRUE,
    ...
  )
}

param_rows <- function(model, paramHeader, param) {
  p <- model$parameters$unstandardized
  p[p$paramHeader == paramHeader & p$param == param, , drop = FALSE]
}

test_that("Ch 5: Example 5.1 - Continuous CFA", {
  m <- read_ch5_model("ex5.1.out")

  expect_equal(m$summaries$Estimator, "ML")
  expect_equal(m$summaries$BIC, 9931.295)
  expect_equal(m$sampstat$univariate.sample.statistics["Y6", "Skewness"], 0.213)
  expect_equal(param_rows(m, "F1.BY", "Y2")$est, 1.126)
  expect_true(inherits(m$errors, "mplus.errors"))
})

test_that("Ch 5: Example 5.2 - CFA with categorical indicators", {
  m <- read_ch5_model("ex5.2.out")

  expect_equal(m$summaries$Estimator, "WLSMV")
  expect_equal(m$input$variable$categorical, "u1-u6")
  expect_equal(param_rows(m, "F2.WITH", "F1")$est, -0.021)
  expect_equal(param_rows(m, "Thresholds", "U4$1")$est, 0.060)
})

test_that("Ch 5: Example 5.3 - Mixed continuous and categorical indicators", {
  m <- read_ch5_model("ex5.3.out")

  expect_equal(m$summaries$Parameters, 16)
  expect_equal(m$input$variable$categorical, "u1 u2 u3")
  expect_equal(param_rows(m, "F2.BY", "Y5")$est, 1.051)
  expect_equal(param_rows(m, "Thresholds", "U3$1")$est, -0.010)
})

test_that("Ch 5: Example 5.4 - Censored and count indicators", {
  m <- read_ch5_model("ex5.4.out")

  expect_equal(m$summaries$Estimator, "MLR")
  expect_equal(m$input$variable$censored, "y1-y3 (a)")
  expect_equal(m$input$variable$count, "u4-u6")
  expect_equal(param_rows(m, "Intercepts", "Y2")$est, 8.046)
  expect_equal(param_rows(m, "F2.BY", "U6")$est, 1.048)
})

test_that("Ch 5: IRT examples capture model variants and gh5 output", {
  skip_if_not_installed("rhdf5")

  gpcm <- read_ch5_model("ex5.5.out")
  pcm <- read_ch5_model("ex5.5pcm.out")
  irt_2pl <- read_ch5_model("ex5.5part2.out")
  irt_3pl <- read_ch5_model("ex5.5part3.out")
  irt_4pl <- read_ch5_model("ex5.5part4.out")

  expect_equal(gpcm$input$variable$categorical, "u1-u20 (gpcm)")
  expect_equal(param_rows(gpcm, "F.BY", "U4")$est, 1.085)
  expect_true("irt_data" %in% names(gpcm$gh5))

  expect_equal(pcm$summaries$Parameters, 81)
  expect_equal(param_rows(pcm, "F.BY", "U1")$est, 1)

  expect_equal(irt_2pl$summaries$AIC, 12610.630)
  expect_equal(param_rows(irt_2pl, "F.BY", "U6")$est, 0.519)

  expect_equal(irt_3pl$input$variable$categorical, "u1-u20(3pl)")
  expect_equal(param_rows(irt_3pl, "F.BY", "U4")$est, 0.950)

  expect_equal(irt_4pl$input$variable$categorical, "u1-u20(4pl)")
  expect_equal(param_rows(irt_4pl, "F.BY", "U5")$est, 1.770)
})

test_that("Ch 5: Example 5.6 - Second-order factor analysis", {
  m <- read_ch5_model("ex5.6.out")

  expect_equal(m$summaries$Parameters, 40)
  expect_equal(m$summaries$ChiSqM_Value, 46.743)
  expect_equal(param_rows(m, "F5.BY", "F3")$est, 1.168)
  expect_equal(param_rows(m, "F5.BY", "F4")$se, 0.139)
})

test_that("Ch 5: Example 5.7 - Non-linear CFA", {
  m <- read_ch5_model("ex5.7.out")

  expect_equal(m$summaries$AnalysisType, "RANDOM")
  expect_equal(m$input$analysis$type, "RANDOM")
  expect_equal(m$input$analysis$algorithm, "INTEGRATION")
  expect_equal(param_rows(m, "Y1.ON", "FXF")$est, -0.253)
})

test_that("Ch 5: Example 5.8 - MIMIC CFA", {
  m <- read_ch5_model("ex5.8.out")

  expect_equal(m$summaries$AIC, 8051.235)
  expect_equal(param_rows(m, "F1.ON", "X3")$est, 0.691)
  expect_equal(param_rows(m, "F2.ON", "X1")$est, 0.678)
  expect_equal(m$sampstat$univariate.sample.statistics["X3", "Variance"], 1.042)
})

test_that("Ch 5: Example 5.9 - Mean structure CFA", {
  m <- read_ch5_model("ex5.9.out")

  expect_equal(m$summaries$Parameters, 11)
  expect_equal(param_rows(m, "F2.WITH", "F1")$est, 0.471)
  expect_equal(param_rows(m, "Intercepts", "Y1A")$est, 2.007)
  expect_equal(param_rows(m, "Intercepts", "Y2C")$est, 3.001)
})

test_that("Ch 5: Example 5.10 - Threshold structure CFA", {
  m <- read_ch5_model("ex5.10.out")

  expect_equal(m$summaries$Estimator, "WLSMV")
  expect_equal(m$input$variable$categorical, "u1a-u1c u2a-u2c")
  expect_equal(param_rows(m, "Thresholds", "U1A$1")$est, -0.515)
  expect_equal(param_rows(m, "Thresholds", "U2C$1")$est, 0.540)
})

test_that("Ch 5: Example 5.11 - SEM with continuous factor indicators", {
  m <- read_ch5_model("ex5.11.out")

  expect_equal(m$summaries$CFI, 0.997)
  expect_equal(param_rows(m, "F4.ON", "F3")$est, 0.473)
  expect_equal(param_rows(m, "F3.ON", "F2")$est, 0.790)
  expect_equal(m$sampstat$univariate.sample.statistics["Y10", "Variance"], 2.061)
})

test_that("Ch 5: Example 5.12 - Indirect effect for factors", {
  m <- read_ch5_model("ex5.12.out")

  expect_equal(m$indirect$unstandardized$overall$pred, "F1")
  expect_equal(m$indirect$unstandardized$overall$outcome, "F4")
  expect_equal(as.numeric(m$indirect$unstandardized$overall$est), 0.266)
  expect_equal(as.numeric(m$indirect$unstandardized$overall$se), 0.043)
})

test_that("Ch 5: Example 5.13 - Latent interaction", {
  m <- read_ch5_model("ex5.13.out")

  expect_equal(m$summaries$AnalysisType, "RANDOM")
  expect_equal(param_rows(m, "F3.ON", "F1XF2")$est, 0.397)
  expect_equal(param_rows(m, "F4.ON", "F3")$est, 0.585)
  expect_equal(m$input$analysis$type, "RANDOM")
})

test_that("Ch 5: Example 5.14 - Multiple-group MIMIC without means", {
  m <- read_ch5_model("ex5.14.out")

  expect_equal(m$summaries$NGroups, 2)
  expect_equal(m$input$analysis$model, "NOMEANSTRUCTURE")
  expect_equal(m$input$variable$grouping, "g (1=male 2=female)")
  expect_equal(param_rows(m, "F1.ON", "X1")$est, c(0.501, 0.440))
})

test_that("Ch 5: Example 5.15 - Multiple-group MIMIC with means", {
  m <- read_ch5_model("ex5.15.out")

  expect_equal(m$summaries$Parameters, 44)
  expect_equal(param_rows(m, "Intercepts", "F1")$est, c(0.000, 0.021))
  expect_equal(param_rows(m, "Intercepts", "F2")$est, c(0.000, -0.242))
  expect_equal(param_rows(m, "Intercepts", "Y3")$est, c(0.045, 1.072))
})

test_that("Ch 5: Example 5.16 - Multiple-group categorical CFA", {
  m <- read_ch5_model("ex5.16.out")

  expect_equal(m$summaries$Estimator, "WLSMV")
  expect_equal(m$input$variable$categorical, "u1-u6")
  expect_equal(param_rows(m, "Thresholds", "U3$1")$est, c(-0.230, 0.430))
  expect_equal(sort(names(m$tech4)), c("FEMALE", "MALE"))
})

test_that("Ch 5: Example 5.17 - Theta parameterization", {
  m <- read_ch5_model("ex5.17.out")

  expect_equal(m$input$analysis$parameterization, "THETA")
  expect_equal(param_rows(m, "F1.ON", "X1")$est, c(0.981, 0.785))
  expect_equal(param_rows(m, "Thresholds", "U3$1")$est, c(-0.425, 0.500))
  expect_equal(m$summaries$Parameters, 37)
})

test_that("Ch 5: Example 5.18 - Two-group continuous ACE twin model", {
  m <- read_ch5_model("ex5.18.out")

  expect_equal(m$summaries$NGroups, 2)
  expect_equal(m$input$variable$grouping, "g (1 = mz 2 = dz)")
  expect_equal(param_rows(m, "A1.WITH", "A2")$est, c(1.000, 0.500))
  expect_equal(param_rows(m, "E1.BY", "Y1")$est, c(0.515, 0.515))
})

test_that("Ch 5: Example 5.19 - Two-group categorical ACE twin model", {
  m <- read_ch5_model("ex5.19.out")

  expect_equal(m$summaries$Estimator, "WLSMV")
  expect_equal(m$input$variable$categorical, "u1-u2")
  expect_equal(param_rows(m, "A1.BY", "U1")$est, c(0.848, 0.848))
  expect_equal(param_rows(m, "Thresholds", "U1$1")$est, c(0.007, 0.007))
})

test_that("Ch 5: Example 5.20 - Parameter constraints", {
  m <- read_ch5_model("ex5.20.out")

  expect_equal(m$summaries$Parameters, 17)
  expect_equal(param_rows(m, "New.Additional.Parameters", "REL2")$est, 0.722)
  expect_equal(param_rows(m, "New.Additional.Parameters", "STAN6")$est, 0.805)
  expect_equal(param_rows(m, "F2.WITH", "F1")$est, -0.024)
})

test_that("Ch 5: Example 5.21 - Continuous twin model using constraints", {
  m <- read_ch5_model("ex5.21.out")

  expect_equal(param_rows(m, "Y1.WITH", "Y2")$est, c(0.753, 0.432))
  expect_equal(param_rows(m, "New.Additional.Parameters", "A")$est, 0.801)
  expect_equal(param_rows(m, "New.Additional.Parameters", "H")$est, 0.631)
  expect_equal(m$summaries$ChiSqM_DF, 6)
})

test_that("Ch 5: Example 5.22 - Categorical twin model using constraints", {
  m <- read_ch5_model("ex5.22.out")

  expect_equal(param_rows(m, "U1.WITH", "U2")$est, c(0.771, 0.411))
  expect_equal(param_rows(m, "New.Additional.Parameters", "C")$est, 0.228)
  expect_equal(param_rows(m, "New.Additional.Parameters", "H")$est, 0.719)
  expect_equal(m$input$variable$categorical, "u1 u2")
})

test_that("Ch 5: Example 5.23 - QTL sibling model with constraints", {
  m <- read_ch5_model("ex5.23.out")

  expect_equal(m$input$variable$constraint, "pihat")
  expect_equal(param_rows(m, "New.Additional.Parameters", "A")$est, 0.848)
  expect_equal(param_rows(m, "New.Additional.Parameters", "Q")$est, 0.649)
  expect_equal(param_rows(m, "Y1.WITH", "Y2")$est, 999)
})

test_that("Ch 5: Example 5.24 - EFA MIMIC model with direct effects", {
  m <- read_ch5_model("ex5.24.out")

  expect_equal(m$summaries$RMSEA_Estimate, 0.025)
  expect_equal(param_rows(m, "F2.BY", "Y6")$est, 0.861)
  expect_equal(param_rows(m, "Y1.ON", "X1")$est, 0.387)
  expect_equal(param_rows(m, "Y8.ON", "X2")$est, 0.577)
})

test_that("Ch 5: Example 5.25 - SEM with EFA and CFA factors", {
  m <- read_ch5_model("ex5.25.out")

  expect_equal(m$summaries$Parameters, 44)
  expect_equal(param_rows(m, "F1.BY", "Y1")$est, 0.751)
  expect_equal(param_rows(m, "F3.BY", "Y8")$est, 0.894)
  expect_equal(param_rows(m, "F4.ON", "F3")$est, 0.546)
})

test_that("Ch 5: Example 5.26 - Longitudinal EFA with correlated residuals", {
  m <- read_ch5_model("ex5.26.out")

  expect_equal(m$summaries$ChiSqM_Value, 43.990)
  expect_equal(param_rows(m, "F2.BY", "Y5")$est, 0.908)
  expect_equal(param_rows(m, "Y1.WITH", "Y7")$est, 0.397)
  expect_equal(m$summaries$CFI, 0.998)
})

test_that("Ch 5: Example 5.27 invariance sequence captures constraint changes", {
  ex527 <- read_ch5_model("ex5.27.out")
  ex527b <- read_ch5_model("ex5.27b.out")
  ex527c <- read_ch5_model("ex5.27c.out")
  ex527d <- read_ch5_model("ex5.27d.out")
  ex527e <- read_ch5_model("ex5.27e.out")

  expect_equal(ex527$summaries$Parameters, 78)
  expect_equal(ex527b$summaries$Parameters, 62)
  expect_equal(ex527c$summaries$Parameters, 54)
  expect_equal(ex527d$summaries$Parameters, 51)
  expect_equal(ex527e$summaries$Parameters, 49)

  expect_equal(param_rows(ex527, "F2.BY", "Y10")$est, c(0.722, 1.227))
  expect_equal(param_rows(ex527d, "Means", "F1")$est, c(0.000, 0.329))
  expect_equal(param_rows(ex527d, "Means", "F2")$est, c(0.000, 0.620))
  expect_equal(param_rows(ex527e, "Means", "F1")$est, c(0.000, 0.000))
  expect_equal(param_rows(ex527e, "Means", "F2")$est, c(0.000, 0.000))
})

test_that("Ch 5: Example 5.28 - EFA with residual variance lower bounds", {
  m <- read_ch5_model("ex5.28.out")

  expect_equal(m$input$analysis$rotation, "GEOMIN")
  expect_equal(param_rows(m, "Residual.Variances", "Y9")$est, 0.000)
  expect_equal(param_rows(m, "F2.BY", "Y9")$est, 0.991)
  expect_equal(m$summaries$Observations, 50)
})

test_that("Ch 5: Examples 5.29 and 5.30 - Bi-factor EFA models", {
  ex529 <- read_ch5_model("ex5.29.out")
  ex530 <- read_ch5_model("ex5.30.out")

  expect_equal(ex529$input$analysis$rotation, "BI-GEOMIN")
  expect_equal(param_rows(ex529, "F1.BY", "Y4")$est, 2.252)
  expect_equal(param_rows(ex529, "F2.BY", "Y10")$est, 1.308)

  expect_equal(ex530$input$analysis$rotation, "GEOMIN")
  expect_equal(param_rows(ex530, "FG.BY", "Y1")$est, 0.675)
  expect_equal(param_rows(ex530, "F2.BY", "Y8")$est, 0.369)
})

test_that("Ch 5: Bayesian examples load Bayes summaries and gh5 companions", {
  skip_if_not_installed("rhdf5")

  ex531 <- read_ch5_model("ex5.31.out")
  ex532 <- read_ch5_model("ex5.32.out")
  ex533 <- read_ch5_model("ex5.33.out")

  expect_equal(ex531$summaries$Estimator, "BAYES")
  expect_equal(param_rows(ex531, "Variances", "F1")$est, 0.211)
  expect_true("bayesian_data" %in% names(ex531$gh5))

  expect_equal(ex532$summaries$BIC, 8291.205)
  expect_equal(param_rows(ex532, "F1.ON", "X3")$est, 0.706)
  expect_true("bayesian_data" %in% names(ex532$gh5))

  expect_equal(ex533$summaries$AnalysisType, "MIXTURE")
  expect_equal(ex533$input$variable$knownclass, "c(group = 1-10)")
  expect_equal(length(param_rows(ex533, "Means", "F1")$est), 10)
  expect_equal(param_rows(ex533, "Means", "F1")$est[c(1, 10)], c(0.937, 0.000))
  expect_true(all(c("class_data", "measurement_parameters") %in% names(ex533$gh5)))
})
