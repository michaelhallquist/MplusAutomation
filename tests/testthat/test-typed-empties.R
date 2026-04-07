test_that("readModels returns an empty typed tech8 object when TECH8 output is absent", {
  m <- MplusAutomation::readModels(
    target = testthat::test_path("ex3.1.out"),
    what = "tech8",
    quiet = TRUE
  )
  
  expect_true("tech8" %in% names(m))
  expect_s3_class(m$tech8, "mplus.tech8")
  expect_true("psr" %in% names(m$tech8))
  expect_s3_class(m$tech8$psr, "mplus.psr.data.frame")
  expect_length(m$tech8$psr, 0L)
})

test_that("readModels returns an empty typed indirect object when indirect output is absent", {
  m <- MplusAutomation::readModels(
    target = testthat::test_path("ex3.1.out"),
    what = "indirect",
    quiet = TRUE
  )
  
  expect_true("indirect" %in% names(m))
  expect_s3_class(m$indirect, "mplus.indirect")
  expect_true(all(c("overall", "specific") %in% names(m$indirect)))
  expect_s3_class(m$indirect$overall, "data.frame")
  expect_s3_class(m$indirect$specific, "data.frame")
  expect_equal(nrow(m$indirect$overall), 0L)
  expect_equal(nrow(m$indirect$specific), 0L)
})
