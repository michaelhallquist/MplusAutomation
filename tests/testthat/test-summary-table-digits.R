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

test_that("integer summary count fields are not formatted with decimal places", {
  int_fields <- c(
    "ParametersWithAux",
    "T11_KM1Starts",
    "T11_KM1Final",
    "BLRT_KM1AnalysisStarts",
    "BLRT_KGenFinal",
    "NumFactors_Between",
    "NumFactors_Within"
  )

  for (nm in int_fields) {
    expect_false(
      MplusAutomation:::is_decimal_summary_field(nm, c(10, 20)),
      info = nm
    )
  }

  df <- data.frame(
    Title = "Model 1",
    ChiSqM_Value = 46.743,
    T11_KM1Starts = 20,
    BLRT_KGenFinal = 5,
    NumFactors_Between = 2,
    stringsAsFactors = FALSE
  )

  formatted <- MplusAutomation:::format_summary_display(df, digits = 3)
  expect_identical(formatted$ChiSqM_Value, "46.743")
  expect_identical(formatted$T11_KM1Starts, 20)
  expect_identical(formatted$BLRT_KGenFinal, 5)
  expect_identical(formatted$NumFactors_Between, 2)
})
