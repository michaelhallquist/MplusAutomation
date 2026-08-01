test_that("v9.1 categorical residual matrices are extracted", {
  model <- MplusAutomation::readModels(
    target = testthat::test_path("v91-ex5.2-residuals.out"),
    what = "residuals",
    quiet = TRUE
  )

  residuals <- model$residuals
  expect_equal(residuals$meanEst[1, "U1$1"], 0.010)
  expect_equal(residuals$meanResid[1, "U6$1"], 0)

  expect_equal(residuals$correlationEst["U2", "U1"], 0.854)
  expect_equal(residuals$correlationResid["U2", "U1"], 0)
  expect_equal(residuals$partialCorrelationEst["U2", "U1"], 0.546)
  expect_equal(residuals$partialCorrelationResid["U2", "U1"], 0.030)
})
