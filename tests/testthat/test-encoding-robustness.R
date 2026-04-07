test_that("readModels recovers latin1 input text instead of failing during input parsing", {
  outfile <- tempfile(fileext = ".out")
  lines <- c(
    "Mplus VERSION 8.3",
    "MUTHEN & MUTHEN",
    "",
    "INPUT INSTRUCTIONS",
    "",
    "  title: espa\\u00f1a;",
    "  data: file is test.dat;",
    "  variable:",
    "    names are y;",
    "  analysis:",
    "    estimator is ml;",
    "  model:",
    "    y;",
    "  output: sampstat;",
    "",
    "INPUT READING TERMINATED NORMALLY",
    "",
    "espa\\u00f1a;",
    "",
    "SUMMARY OF ANALYSIS",
    "",
    "Estimator                                                       ML",
    "Input data file(s)",
    "  test.dat",
    "",
    "Input data format  FREE"
  )
  
  latin1_lines <- iconv(lines, from = "UTF-8", to = "latin1")
  con <- file(outfile, open = "wb")
  writeLines(latin1_lines, con = con, useBytes = TRUE)
  close(con)
  
  m <- readModels(outfile, what = "input", quiet = TRUE)
  expect_s3_class(m$input, "mplus.inp")
  expect_equal(m$input$title, "espa\\u00f1a;")
  expect_equal(m$input$data$file, "test.dat")
})

test_that("sanitize_mplus_text recovers invalid single-byte lines", {
  bad <- rawToChar(as.raw(c(0x74, 0x69, 0x74, 0x6c, 0x65, 0x3a, 0x20, 0xe9, 0x3b)), multiple = FALSE)
  Encoding(bad) <- "unknown"
  
  expect_warning(
    clean <- MplusAutomation:::sanitize_mplus_text(bad, "synthetic.out"),
    "Recovered 1 line\\(s\\) with invalid text encoding in synthetic.out"
  )
  expect_equal(clean, "title: \u00e9;")
})

test_that("matrixExtract truncates overflow rows instead of throwing subscript errors", {
  text <- c(
    "Correlations",
    "   M1            M2",
    "   ________      ________",
    " M1    1.000",
    " M2    0.300       1.000       9.999",
    "",
    "NEXT"
  )
  
  expect_warning(
    mat <- MplusAutomation:::matrixExtract(text, "Correlations", "synthetic.out"),
    "Matrix rows exceeded detected column headers"
  )
  expect_equal(dim(mat), c(2L, 2L))
  expect_equal(unname(mat["M1", "M1"]), 1.000)
  expect_equal(unname(mat["M2", "M1"]), 0.300)
  expect_equal(unname(mat["M2", "M2"]), 1.000)
})
