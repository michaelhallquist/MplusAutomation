test_that("compareModels returns a list with diff test", {
  m1 <- readModels(target = system.file("extdata", "ex3.1.out", package = "MplusAutomation"))
  m2 <- readModels(target = system.file("extdata", "ex3.5.out", package = "MplusAutomation"))
  res <- compareModels(m1, m2, show = "summaries", diffTest = TRUE)
  expect_true(is.list(res))
  expect_true("summaries" %in% names(res))
  expect_true("diffTest" %in% names(res))
})
