test_that("SummaryTable digits only affect rendered output", {
  m <- readModels(
    target = get_mplus_file("ch5/ex5.6.out", mplus_version = "8.11"),
    quiet = TRUE
  )

  raw_table <- SummaryTable(
    m,
    type = "none",
    keepCols = c("Title", "ChiSqM_Value"),
    digits = 0
  )

  expect_type(raw_table$ChiSqM_Value, "double")
  expect_equal(raw_table$ChiSqM_Value, 46.743)
})

test_that("SummaryTable markdown honors explicit digits", {
  m <- readModels(
    target = get_mplus_file("ch5/ex5.6.out", mplus_version = "8.11"),
    quiet = TRUE
  )

  markdown_output <- capture.output(
    SummaryTable(
      m,
      type = "markdown",
      keepCols = c("Title", "ChiSqM_Value"),
      digits = 3
    )
  )

  expect_true(any(grepl("46\\.743", markdown_output)))
})

test_that("LatexSummaryTable honors explicit digits", {
  m <- readModels(
    target = get_mplus_file("ch5/ex5.6.out", mplus_version = "8.11"),
    quiet = TRUE
  )

  latex_output <- capture.output(
    print(
      LatexSummaryTable(
        m,
        keepCols = c("Title", "ChiSqM_Value"),
        digits = 3
      ),
      type = "latex"
    )
  )

  expect_true(any(grepl("46\\.743", latex_output)))
})

test_that("LatexSummaryTable preserves xtable defaults when digits is omitted", {
  m <- readModels(
    target = get_mplus_file("ch5/ex5.6.out", mplus_version = "8.11"),
    quiet = TRUE
  )

  latex_output <- capture.output(
    print(
      LatexSummaryTable(
        m,
        keepCols = c("Title", "ChiSqM_Value")
      ),
      type = "latex"
    )
  )

  expect_true(any(grepl("46\\.74", latex_output)))
  expect_false(any(grepl("46\\.743", latex_output)))
})
