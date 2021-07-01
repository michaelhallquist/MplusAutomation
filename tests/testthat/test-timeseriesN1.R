test_that("Mplus User Guide 6.23 - Bayesian N = 1 time series results can be read in", {
  m <- readModels(target = htmlout("https://statmodel.com/usersguide/chap6/ex6.23.html"))
  b <- coef(m, params = "regression")
  expect_equal(b$est[1], 0.171)
  expect_equal(b$se[1], 0.104)
  expect_equal(m$summaries$DIC, 298.426)
})
