test_that("readModels reports normal Mplus termination with a tri-state converged field", {
  model <- MplusAutomation::readModels(
    target = testthat::test_path("ex3.1.out"),
    what = "converged",
    quiet = TRUE
  )

  expect_true(is.logical(model$converged))
  expect_length(model$converged, 1L)
  expect_true(model$converged)
})

test_that("explicit conventional and Bayesian non-convergence messages take precedence", {
  ml <- MplusAutomation:::extractConvergence_1file(c(
    "WARNING: THE SAMPLE COVARIANCE MATRIX IS SINGULAR.",
    "NO CONVERGENCE.  NUMBER OF ITERATIONS EXCEEDED."
  ))
  bayes <- MplusAutomation:::extractConvergence_1file(c(
    "THE MODEL ESTIMATION TERMINATED NORMALLY",
    "THE CONVERGENCE CRITERION IS NOT SATISFIED."
  ))

  expect_false(ml$converged)
  expect_identical(ml$status, "not_converged")
  expect_identical(ml$reason, "max_iterations_exceeded")
  expect_false(bayes$converged)
  expect_identical(bayes$reason, "bayes_criterion_not_satisfied")
})

test_that("non-convergence messages are retained in errors", {
  input <- structure(list(), class = c("mplus.inp", "list"))
  attr(input, "start.line") <- 0L
  attr(input, "end.line") <- 0L
  output <- c(
    "THE CONVERGENCE CRITERION IS NOT SATISFIED.",
    "INCREASE THE MAXIMUM NUMBER OF ITERATIONS.",
    "",
    "MODEL FIT INFORMATION"
  )

  parsed <- MplusAutomation:::extractWarningsErrors_1file(output, "model.out", input)

  expect_s3_class(parsed$errors, "mplus.errors")
  expect_length(parsed$errors, 1L)
  expect_match(paste(parsed$errors[[1L]], collapse = " "), "CONVERGENCE CRITERION IS NOT SATISFIED")
})

test_that("convergence is unknown when Mplus provides no termination message", {
  model <- MplusAutomation:::extractConvergence_1file("MODEL RESULTS")

  expect_true(is.na(model$converged))
  expect_identical(model$status, "unknown")
  expect_true(is.na(model$reason))
  expect_length(model$message, 0L)
})
