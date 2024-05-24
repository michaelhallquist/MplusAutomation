# ch3 - regression and path analysis
mv <- "8.9"

test_that("Ch 3: Example 3.1 - Linear Regression", {
  m <- readModels(target = get_mplus_file("ch3/ex3.1.out", mplus_version=mv))
  b <- coef(m, params = "regression")
  expect_equal(b$est[1], 0.969)
  expect_equal(b$se[1], 0.042)
  expect_equal(m$summaries$BIC, 1413.526)
  expect_equal(m$sampstat$univariate.sample.statistics["Y1", "Kurtosis"], -0.136)
  expect_equal(m$summaries$Estimator, "ML")
  expect_true(inherits(m$errors, "mplus.errors"))
})


test_that("Ch 3: Example 3.2 - Censored Regression", {
  m <- readModels(target = get_mplus_file("ch3/ex3.2.out", mplus_version=mv))
  b <- coef(m, params = "regression")
  expect_equal(b$est[1], 1.075)
  expect_equal(b$se[1], 0.043)
  expect_equal(m$summaries$BIC, 2313.402)
  expect_equal(m$summaries$AIC, 2293.771)
  expect_equal(m$summaries$AICC, 2293.811, tolerance=1e-3)
  expect_equal(m$sampstat$univariate.sample.statistics["X1", "Skewness"], 0.041)
  expect_equal(m$summaries$Estimator, "MLR")
  expect_true(inherits(m$errors, "mplus.errors"))
  expect_equal(m$input$variable$names, "y1 x1 x3")
  expect_equal(m$input$variable$censored, "y1 (b)")
})

test_that("Ch 3: Example 3.3 - Censored-Inflated Regression", {
  m <- readModels(target = get_mplus_file("ch3/ex3.3.out", mplus_version=mv))
  b <- coef(m, params = "regression")
  expect_equal(trimws(b$Label[3]), "Y1#1<-X1")
  expect_equal(b$est[3], 0.338)
  expect_equal(b$se[2], 0.093)
  expect_equal(m$summaries$BIC, 1046.553)
  expect_equal(m$summaries$Parameters, 7)
  expect_equal(m$summaries$LLCorrectionFactor, 0.9922)
  expect_equal(m$sampstat$univariate.sample.statistics["X1", "Skewness"], -0.035)
  expect_equal(m$summaries$Estimator, "MLR")
  expect_equal(m$input$data$file, "ex3.3.dat")
})


test_that("Ch 3: Example 3.4 - Probit Regression", {
  m <- readModels(target = get_mplus_file("ch3/ex3.4.out", mplus_version=mv))
  b <- coef(m, params = "regression")
  expect_equal(trimws(b$Label[2]), "U1<-X3")
  expect_equal(b$est[2], 2.474)
  expect_equal(b$se[2], 0.224)
  expect_equal(m$parameters$unstandardized$est[3], 0.984)
  expect_equal(m$summaries$ChiSqBaseline_Value, 193.243)
  expect_equal(m$summaries$Parameters, 3)
  expect_equal(m$sampstat$univariate.sample.statistics["X1", "Skewness"], -0.133)
  expect_equal(m$summaries$Estimator, "WLSMV")
  expect_equal(m$input$data$file, "ex3.4.dat")
})

test_that("Ch 3: Example 3.5 - Logistic Regression", {
  m <- readModels(target = get_mplus_file("ch3/ex3.5.out", mplus_version=mv))
  b <- coef(m, params = "regression")
  expect_equal(trimws(b$Label[2]), "U1<-X3")
  expect_equal(b$est[2], 1.839)
  expect_equal(b$se[2], 0.179)
  expect_equal(m$parameters$unstandardized$est[3], 1.026)
  expect_equal(m$summaries$aBIC, 414.362)
  expect_equal(m$summaries$Parameters, 3)
  expect_equal(m$sampstat$univariate.sample.statistics["X1", "Skewness"], -0.133)
  expect_equal(m$summaries$Estimator, "ML")
  expect_equal(m$input$data$file, "ex3.5.dat")
  
  # odds ratios
  expect_equal(m$parameters$odds$est[1L], 2.921)
  expect_equal(m$parameters$odds$lower_2.5ci[2L], 4.423)
  
  # probability scale
  expect_equal(m$parameters$probability.scale$est[2L], 0.346)
})

# modified version of multinomial regression ex3.6 that includes odds ratios and confidence intervals
# only currently run on 8.11
test_that("Ch 3: Example 3.6 - Multinomial Regression", {
  m <- readModels(target = get_mplus_file("ch3/ex3.6.out", mplus_version="8.11"))
  b <- coef(m, params = "regression")
  expect_equal(trimws(b$Label[2]), "U1#1<-X3")
  expect_equal(b$est[2], 2.259)
  expect_equal(b$se[2], 0.203)
  
  # odds ratios
  expect_equal(m$parameters$odds$est[1L], 2.157)
  expect_equal(m$parameters$odds$lower_2.5ci[2L], 6.438)
  
  # confidence intervals for odds ratios
  expect_equal(m$parameters$ci.odds$low.5[3L], 0.985)
})