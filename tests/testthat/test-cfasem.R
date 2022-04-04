test_that("Mplus User Guide 5.1 - CFA with continuous indicators results can be read in", {
  m <- readModels(target = testthat::test_path("ex5.1.out"))
  b <- coef(m, params = "loading")
  expect_equal(b$est[1], 1.000)
  expect_equal(b$se[1], 0.000)
  expect_equal(m$summaries$BIC, 9931.295)
})

test_that("Mplus User Guide 5.2 - CFA with categorical indicators results can be read in", {
  m <- readModels(target = testthat::test_path("ex5.2.out"))
  b <- coef(m, params = "loading")
  expect_equal(b$est[1], 1.000)
  expect_equal(b$se[1], 0.000)
  expect_equal(m$summaries$SRMR, 0.021)
})

test_that("Mplus User Guide 5.5 part 4 - 4PL IRT results can be read in", {
  m <- readModels(target = testthat::test_path("ex5.5part4.out"))
  b <- coef(m, params = "loading")
  expect_equal(b$est[1], 0.918)
  expect_equal(b$se[1], 0.155)
  expect_equal(m$summaries$BIC, 269933.988)
})

test_that("Mplus User Guide 5.12 - SEM results can be read in", {
  m <- readModels(target = testthat::test_path("ex5.12.out"))
  b <- coef(m, params = "loading")
  expect_equal(b$est[1], 1.000)
  expect_equal(b$se[1], 0.000)
  
  b <- coef(m, params = "regression")
  expect_equal(b$est[1], 0.473)
  expect_equal(b$se[1], 0.057)
  
  expect_equal(m$summaries$BIC, 19542.505)
})

test_that("Mplus User Guide 5.33 - Bayesian SEM multiple group results can be read in", {
  m <- readModels(target = testthat::test_path("ex5.33.out"))
  b <- coef(m, params = "loading")
  expect_equal(b$est[1], 0.848)
  expect_equal(b$se[1], 0.061)  
  expect_equal(m$summaries$DIC, 35277.206)
})
