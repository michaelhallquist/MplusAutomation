test_that("mixtureSummaryTable accepts a readModels mixture model", {
  m <- readModels(
    target = testthat::test_path("iris_2_class.out"),
    what = c("summaries", "class_counts", "warn_err", "input")
  )

  tab <- mixtureSummaryTable(m)

  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), 1L)
  expect_true(all(c("Title", "Classes", "AIC", "BIC") %in% names(tab)))
  expect_equal(tab$Classes, 2)
})

test_that("mixtureSummaryTable gives an informative error for bare summaries", {
  m <- readModels(
    target = testthat::test_path("iris_2_class.out"),
    what = c("summaries", "class_counts", "warn_err", "input")
  )

  expect_error(
    mixtureSummaryTable(m$summaries),
    "requires full model objects, not readModels\\(\\.\\.\\.\\)\\$summaries"
  )
})
