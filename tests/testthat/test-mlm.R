test_that("Mplus User Guide 9.2c - Multilevel regression with random effect results can be read in", {
  m <- readModels(target = testthat::test_path("ex9.2c.out"))
  b <- coef(m, params = "regression")
  expect_equal(b$est[1], 0.569)
  expect_equal(b$se[1], 0.094)
  expect_equal(m$summaries$BIC, 6246.063)
})


test_that("Mplus User Guide 9.31 - Bayesian Multilevel time series AR(1) results can be read in", {
  m <- readModels(target = testthat::test_path("ex9.31.out"))
  b <- coef(m, params = "regression")
  expect_equal(b$est[1], 0.120)
  expect_equal(b$se[1], 0.016)

  b <- coef(m, params = "variability")
  expect_equal(b$est[4], 0.104) ## random residual variance
  expect_equal(b$se[4], 0.023)
  
  expect_equal(m$summaries$DIC, 14729.136)
})
