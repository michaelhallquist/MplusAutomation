test_that("readModels returns typed empties for requested classed sections", {
  m <- MplusAutomation::readModels(
    target = testthat::test_path("ex3.1.out"),
    what = c("residuals", "tech1", "tech3", "tech4", "tech7", "tech9", "tech12", "tech15", "fac_score_stats"),
    quiet = TRUE
  )
  
  expect_s3_class(m$residuals, "mplus.residuals")
  expect_length(m$residuals, 0L)
  
  expect_s3_class(m$tech1, "mplus.tech1")
  expect_s3_class(m$tech1$parameterSpecification, "mplus.parameterSpecification")
  expect_s3_class(m$tech1$startingValues, "mplus.startingValues")
  
  expect_s3_class(m$tech3, "mplus.tech3")
  expect_length(m$tech3, 0L)
  
  expect_s3_class(m$tech4, "mplus.tech4")
  expect_length(m$tech4, 0L)
  
  expect_s3_class(m$tech7, "mplus.tech7")
  expect_length(m$tech7, 0L)
  
  expect_s3_class(m$tech9, "mplus.tech9")
  expect_length(m$tech9, 0L)
  
  expect_s3_class(m$tech12, "mplus.tech12")
  expect_length(m$tech12, 0L)
  
  expect_s3_class(m$tech15, "mplus.tech15")
  expect_true("conditional.probabilities" %in% names(m$tech15))
  expect_length(m$tech15$conditional.probabilities, 0L)
  
  expect_s3_class(m$fac_score_stats, "mplus.facscorestats")
  expect_length(m$fac_score_stats, 0L)
})
