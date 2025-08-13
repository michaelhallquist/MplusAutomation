test_that("indirect effects with # in outcome are parsed", {
  m <- readModels(target = testthat::test_path("nbh_indirect.out"), what = "indirect")
  ind <- get_indirect(m)
  expect_true(any(ind$unstandardized$overall$outcome == "DVNB#1"))
  expect_true(any(ind$unstandardized$overall$outcome == "DVNB"))
})
