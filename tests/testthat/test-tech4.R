test_that("extractTech4 handles title-case matrix headings", {
  tech4_section <- structure(
    "ignored",
    class = c("mplus.section", "character"),
    lines = 1L,
    header.line = 1L
  )
  tech4_subsection <- structure(
    c(
      "Estimated Means for the Latent Variables",
      "   F1",
      "   ________",
      " F1    0.000",
      "Estimated Covariance Matrix for the Latent Variables",
      "   F1",
      "   ________",
      " F1    1.000",
      "Estimated Correlation Matrix for the Latent Variables",
      "   F1",
      "   ________",
      " F1    1.000",
      "NEXT"
    ),
    class = c("mplus.section", "character"),
    lines = seq_len(13L),
    header.line = 1L
  )
  tech4_subsections <- structure(
    list(tech4_subsection),
    class = c("mplus.section.list", "list"),
    matchlines = 1L
  )
  real_getMultilineSection <- MplusAutomation:::getMultilineSection
  
  tech4 <- testthat::with_mocked_bindings(
    MplusAutomation:::extractTech4("ignored", "mock.out"),
    getSection = function(...) tech4_section,
    getMultilineSection = function(header, ...) {
      if (grepl("^ESTIMATES DERIVED FROM THE MODEL", header)) {
        return(tech4_subsections)
      }
      real_getMultilineSection(header, ...)
    },
    .package = "MplusAutomation"
  )
  
  expect_s3_class(tech4, "mplus.tech4")
  expect_equal(unname(tech4$latMeansEst["F1", "F1"]), 0)
  expect_equal(unname(tech4$latCovEst["F1", "F1"]), 1)
  expect_equal(unname(tech4$latCorEst["F1", "F1"]), 1)
})
