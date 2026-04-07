test_that("readModels returns an empty typed sampstat object when no sampstat-related sections exist", {
  m <- MplusAutomation::readModels(
    target = testthat::test_path("nbh_indirect.out"),
    what = "sampstat",
    quiet = TRUE
  )
  
  expect_true("sampstat" %in% names(m))
  expect_s3_class(m$sampstat, "mplus.sampstat")
  expect_length(m$sampstat, 0L)
})
