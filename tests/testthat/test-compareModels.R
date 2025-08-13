test_that("compareModels returns a list with diff test", {
  # not a valid diff test comparison -- just arbitrarily testing that we get the result
  m1 <- readModels(target = system.file("extdata", "ex6.4.out", package = "MplusAutomation"))
  m2 <- readModels(target = system.file("extdata", "ex3.7.out", package = "MplusAutomation"))
  res <- compareModels(m2, m1, diffTest = TRUE)
  expect_true(is.list(res))
  expect_true("summaries" %in% names(res))
  expect_true("diffTest" %in% names(res))
})
