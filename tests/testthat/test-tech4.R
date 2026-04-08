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
  expect_null(tech4$latMeansSE)
  expect_null(tech4$latCovSE)
  expect_null(tech4$latCorSE)
})

test_that("extractTech4 merges repeated unnamed subsections", {
  tech4_section <- structure(
    c(
      "ESTIMATES DERIVED FROM THE MODEL",
      "ESTIMATES DERIVED FROM THE MODEL"
    ),
    class = c("mplus.section", "character"),
    lines = c(1L, 20L),
    header.line = 1L
  )
  estimates_subsection <- structure(
    c(
      "Estimated Covariance Matrix for the Latent Variables",
      "   F1           F2",
      "   ________     ________",
      " F1    1.000",
      " F2    0.250        0.900",
      "Estimated Correlation Matrix for the Latent Variables",
      "   F1           F2",
      "   ________     ________",
      " F1    1.000",
      " F2    0.263        1.000",
      "NEXT"
    ),
    class = c("mplus.section", "character"),
    lines = seq_len(11L),
    header.line = 1L
  )
  se_only_subsection <- structure(
    c(
      "S.E. for Estimated Covariance Matrix for the Latent Variables",
      "   F1           F2",
      "   ________     ________",
      " F1    0.100",
      " F2    0.050        0.090",
      "NEXT"
    ),
    class = c("mplus.section", "character"),
    lines = 20:25,
    header.line = 20L
  )
  tech4_subsections <- structure(
    list(estimates_subsection, se_only_subsection),
    class = c("mplus.section.list", "list"),
    matchlines = c(1L, 2L)
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
  expect_null(tech4$X)
  expect_equal(unname(tech4$latCovEst["F2", "F1"]), 0.25)
  expect_equal(unname(tech4$latCorEst["F2", "F1"]), 0.263)
  expect_equal(unname(tech4$latCovSE["F2", "F1"]), 0.05)
  expect_null(tech4$latCovEstSE)
  expect_null(tech4$latCovPValue)
})

test_that("readModels extracts grouped TECH4 output from ex5.16", {
  m <- MplusAutomation::readModels(
    target = get_mplus_file("ch5/ex5.16.out", mplus_version = "8.11"),
    what = c("tech4"),
    quiet = TRUE
  )
  
  expect_s3_class(m$tech4, "mplus.tech4")
  expect_equal(sort(names(m$tech4)), c("FEMALE", "MALE"))
  
  expect_equal(unname(m$tech4$MALE$latMeansEst[1, "F1"]), 0.021, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latMeansSE[1, "F1"]), 0.033, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latMeansEstSE[1, "F1"]), 0.634, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latMeansPValue[1, "F1"]), 0.526, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latCovEst["F2", "F1"]), 1.061, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latCovSE["F2", "F1"]), 0.103, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latCovEstSE["F2", "F1"]), 10.300, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latCovPValue["F2", "F1"]), 0, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latCorEst["F2", "F1"]), 0.585, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latCorSE["F2", "F1"]), 0.032, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latCorEstSE["F2", "F1"]), 18.415, tolerance = 1e-7)
  expect_equal(unname(m$tech4$MALE$latCorPValue["F2", "F1"]), 0, tolerance = 1e-7)
  
  expect_equal(unname(m$tech4$FEMALE$latMeansEst[1, "F1"]), -0.212, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latMeansSE[1, "F1"]), 0.123, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latMeansEstSE[1, "F1"]), -1.728, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latMeansPValue[1, "F1"]), 0.084, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latCovEst["F2", "F1"]), 0.946, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latCovSE["F2", "F1"]), 0.251, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latCovEstSE["F2", "F1"]), 3.768, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latCovPValue["F2", "F1"]), 0, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latCorEst["F2", "F1"]), 0.591, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latCorSE["F2", "F1"]), 0.031, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latCorEstSE["F2", "F1"]), 19.132, tolerance = 1e-7)
  expect_equal(unname(m$tech4$FEMALE$latCorPValue["F2", "F1"]), 0, tolerance = 1e-7)
})
