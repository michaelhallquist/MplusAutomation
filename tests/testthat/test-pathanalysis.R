test_that("Mplus User Guide 3.1 - linear regression results can be read in", {
  m <- readModels(target = system.file("extdata", "ex3.1.out", package = "MplusAutomation"))
  b <- coef(m, params = "expectation")
  expect_equal(b$est, 0.511)
  expect_equal(b$se, 0.043)
  expect_equal(m$summaries$BIC, 1413.526)
})

test_that("Mplus User Guide 3.4 - probit regression results can be read in", {
  m <- readModels(target = system.file("extdata", "ex3.4.out", package = "MplusAutomation"))
  b <- coef(m, params = "expectation")
  expect_equal(b$est, 0.984)
  expect_equal(b$se, 0.119)
  expect_equal(m$summaries$ChiSqBaseline_Value, 193.243)
})

test_that("Mplus User Guide 3.5 - logistic regression results can be read in", {
  m <- readModels(target = system.file("extdata", "ex3.5.out", package = "MplusAutomation"))
  b <- coef(m, params = "expectation")
  expect_equal(b$est, 1.026)
  expect_equal(b$se, 0.137)
  expect_equal(m$summaries$BIC, 423.884)
})


test_that("Mplus User Guide 3.6 - multinomial logistic regression results can be read in", {
  m <- readModels(target = system.file("extdata", "ex3.6.out", package = "MplusAutomation"))
  b <- coef(m, params = "expectation")
  expect_equal(b$est[1], -0.749)
  expect_equal(b$se[1], 0.158)
  expect_equal(m$summaries$BIC, 904.140)
})


test_that("Mplus User Guide 3.7 - poisson regression results can be read in", {
  m <- readModels(target = system.file("extdata", "ex3.7.out", package = "MplusAutomation"))
  b <- coef(m, params = "expectation")
  expect_equal(b$est, 1.026)
  expect_equal(b$se, .030)
  expect_equal(m$summaries$BIC, 1952.412)
})


test_that("Mplus User Guide 3.11 - continuous path analysis results can be read in", {
  m <- readModels(target = system.file("extdata", "ex3.11.out", package = "MplusAutomation"))
  b <- coef(m, params = "expectation")
  expect_equal(b$est[1], -1.064)
  expect_equal(b$se[1], 0.046)
  expect_equal(m$summaries$BIC, 4821.223)
})

test_that("Mplus User Guide 3.16 - path analysis with bootstrapping results can be read in", {
  m <- readModels(target = system.file("extdata", "ex3.16.out", package = "MplusAutomation"))
  b <- coef(m, params = "expectation")
  expect_equal(b$est[1], -1.064)
  expect_equal(b$se[1], 0.046)
  expect_equal(m$summaries$BIC, 4821.223)
  ci <- confint(m, params = "expectation")
  expect_equal(ci$LowerCI[1], -1.150)
  expect_equal(ci$UpperCI[1], -0.969)
})

test_that("Mplus User Guide 3.18 - Bayesian moderated mediation results can be read in", {
  m <- readModels(target = system.file("extdata", "ex3.18.out", package = "MplusAutomation"))
  b <- coef(m, params = "expectation")
  expect_equal(b$est[1], -0.134)
  expect_equal(b$se[1], 0.087)
  expect_equal(m$summaries$DIC, 745.265)
})

