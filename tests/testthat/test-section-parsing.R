test_that("getMultilineSection preserves legacy same-indentation behavior by default", {
  outfile <- MplusAutomation:::parse_into_sections(readLines(testthat::test_path("threeclass1.out")))
  tech11 <- MplusAutomation:::getSection("^\\s*TECHNICAL 11 OUTPUT\\s*$", outfile)
  expect_s3_class(tech11, "mplus.section")
  
  vlmr <- MplusAutomation:::getMultilineSection(
    "VUONG-LO-MENDELL-RUBIN LIKELIHOOD RATIO TEST FOR \\d+ \\(H0\\) VERSUS \\d+ CLASSES",
    tech11,
    filename = "threeclass1.out"
  )
  
  expect_false(any(grepl("WARNING:", vlmr, fixed = TRUE)))
  expect_false(any(grepl("H0 Loglikelihood Value", vlmr, fixed = TRUE)))
  expect_null(attr(vlmr, "section_alerts", exact = TRUE))
})

test_that("getMultilineSection can continue through warning blocks and capture alerts", {
  outfile <- MplusAutomation:::parse_into_sections(readLines(testthat::test_path("threeclass1.out")))
  tech11 <- MplusAutomation:::getSection("^\\s*TECHNICAL 11 OUTPUT\\s*$", outfile)
  
  vlmr <- MplusAutomation:::getMultilineSection(
    "VUONG-LO-MENDELL-RUBIN LIKELIHOOD RATIO TEST FOR \\d+ \\(H0\\) VERSUS \\d+ CLASSES",
    tech11,
    filename = "threeclass1.out",
    continue_on_alert = TRUE
  )
  
  expect_true(any(grepl("H0 Loglikelihood Value", vlmr, fixed = TRUE)))
  expect_true(any(grepl("P-Value", vlmr, fixed = TRUE)))
  
  alerts <- attr(vlmr, "section_alerts", exact = TRUE)
  expect_type(alerts, "character")
  expect_length(alerts, 1L)
  expect_true(grepl("WARNING:", alerts[[1L]], fixed = TRUE))
  expect_true(grepl("DID NOT\\s+TERMINATE NORMALLY", alerts[[1L]], perl = TRUE))
})

test_that("section helpers return NULL on no match", {
  text <- c("HEADER", "  value")
  
  expect_null(MplusAutomation:::getMultilineSection("MISSING", text, filename = "synthetic.out"))
  expect_null(MplusAutomation:::getSection("^MISSING$", MplusAutomation:::parse_into_sections(text)))
})

test_that("getMultilineSection allowMultiple and allowSpace FALSE still return repeated sections", {
  text <- c(
    "VAR1",
    "  first row",
    "  second row",
    "VAR2",
    "  third row"
  )
  
  sections <- MplusAutomation:::getMultilineSection(
    "\\S+",
    text,
    filename = "synthetic.out",
    allowMultiple = TRUE,
    allowSpace = FALSE
  )
  
  expect_s3_class(sections, "mplus.section.list")
  expect_s3_class(sections[[1L]], "mplus.section")
  expect_length(sections, 2L)
  expect_equal(unname(attr(sections, "matchlines")), c(1L, 4L))
  expect_equal(as.character(sections[[1L]]), c("  first row", "  second row"))
  expect_equal(as.character(sections[[2L]]), c("  third row"))
})

test_that("getMultilineSection supports nested headers with allowMultiple", {
  text <- c(
    "GROUP A",
    "  STATS",
    "    value A1",
    "    value A2",
    "",
    "GROUP B",
    "  STATS",
    "    value B1",
    "",
    "NEXT HEADER"
  )
  
  sections <- MplusAutomation:::getMultilineSection(
    "GROUP [AB]::{+1b}STATS",
    text,
    filename = "synthetic.out",
    allowMultiple = TRUE
  )
  
  expect_s3_class(sections, "mplus.section.list")
  expect_length(sections, 2L)
  expect_equal(unname(attr(sections, "matchlines")), c(2L, 7L))
  expect_equal(as.character(sections[[1L]]), c("    value A1", "    value A2"))
  expect_equal(as.character(sections[[2L]]), c("    value B1"))
})

test_that("getMultilineSection nested headers and blank-line sections still work", {
  text <- c(
    "TOP SECTION",
    "  NESTED HEADER",
    "    value 1",
    "    value 2",
    "",
    "OTHER SECTION"
  )
  
  section <- MplusAutomation:::getMultilineSection(
    "TOP SECTION::{+1b}NESTED HEADER",
    text,
    filename = "synthetic.out"
  )
  
  expect_equal(as.character(section), c("    value 1", "    value 2"))
})

test_that("getMultilineSection offset and blank-line syntax still work", {
  text <- c(
    "HEADER",
    "skip this line",
    "keep 1",
    "keep 2",
    "",
    "NEXT HEADER"
  )
  
  section <- MplusAutomation:::getMultilineSection(
    "{+2b}HEADER",
    text,
    filename = "synthetic.out"
  )
  
  expect_equal(as.character(section), c("keep 1", "keep 2"))
})

test_that("getMultilineSection still scans past 20-line chunks for same-indentation endings", {
  text <- c(
    "TOP",
    "  LONG SECTION",
    sprintf("    row %02d", 1:25),
    "  NEXT SECTION",
    "    trailing row"
  )
  
  section <- MplusAutomation:::getMultilineSection(
    "LONG SECTION",
    text,
    filename = "synthetic.out"
  )
  
  expect_length(section, 25L)
  expect_equal(section[[1L]], "    row 01")
  expect_equal(section[[25L]], "    row 25")
  expect_false(any(grepl("NEXT SECTION", section, fixed = TRUE)))
})
