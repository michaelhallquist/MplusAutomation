test_that("Mplus User Guide 7.3 - LCA results can be read in", {
  m <- readModels(target = testthat::test_path("ex7.3.out"))
  expect_equal(m$summaries$LL, -965.244)
  expect_equal(nrow(m$tech10$bivar_model_fit_info), 24)
})