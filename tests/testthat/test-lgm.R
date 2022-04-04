test_that("Mplus User Guide 6.1 - LGM for continuous outcome results can be read in", {
  m <- readModels(target = "ex6.1.out")
  b <- coef(m, params = "loading")
  expect_equal(b$est[1], 1.000)
  expect_equal(b$se[1], 0.000)
  expect_equal(m$summaries$BIC, 6088.703)
})

test_that("Mplus User Guide 6.4 - LGM for categorical outcome results can be read in", {
  m <- readModels(target = "ex6.4.out")
  b <- coef(m, params = "loading")
  expect_equal(b$est[1], 1.000)
  expect_equal(b$se[1], 0.000)
  expect_equal(m$summaries$WRMR, 0.281)
})
