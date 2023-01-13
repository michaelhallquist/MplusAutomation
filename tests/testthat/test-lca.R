test_that("Mplus User Guide 7.3 - LCA results can be read in", {
  m <- readModels(target = testthat::test_path("ex7.3.out"))
  expect_equal(m$summaries$LL, -965.244)
  expect_equal(nrow(m$tech10$bivar_model_fit_info), 24)
})

test_that("Mplus User Guide 7.3 - LCA results can be read in (tech10 error)", {
  m <- readModels(target = testthat::test_path("ex7.3_error.out"))
  expect_equal(m$summaries$LL, -965.244)
  expect_equal(m$tech10, list())
})

test_that("Mplus User Guide 7.3 - LCA results can be read in (older Mplus version)", {
  m <- readModels(target = testthat::test_path("ex7.3_old.out"))
  expect_equal(m$summaries$LL, -965.244)
  expect_equal(m$tech10, list())
})