test_that("Mplus versions are compared by numeric components", {
  versions <- c("6", "6.12.3", "7", "7.2.10", "7.3", "7.3.0", "7.10", "8.11", "9.1.1", "10.0.0")
  before_7_3 <- vapply(
    versions, MplusAutomation:::mplus_version_before, logical(1L),
    threshold = "7.3", USE.NAMES = FALSE
  )
  expect_identical(before_7_3, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_false(MplusAutomation:::mplus_version_before(c("9.1.1", "9.1.1"), "7.3"))
  expect_false(MplusAutomation:::mplus_version_before(" 9.1.1 ", "7.3"))

  for (version in list(NULL, character(), NA_character_, "", "unknown", "9..1")) {
    expect_true(MplusAutomation:::mplus_version_before(version, "7.3"))
  }
})

test_that("patch versions preserve class counts through readModels", {
  fixtures <- c(
    test_path("ex7.3.out"),
    test_path("mplus_ug", "8.11", "ch7", "ex7.15.out")
  )
  dir <- tempfile("mplus-version-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (fixture in fixtures) {
    original <- readLines(fixture)
    baseline <- readModels(fixture, what = c("summaries", "class_counts"))
    expect_gt(nrow(baseline$class_counts$modelEstimated), 0L)

    for (version in c("9.1.1", "9.1.10", "7.10", "10.0.0")) {
      lines <- original
      lines[1L] <- paste("Mplus VERSION", version, "(Mac)")
      file <- file.path(dir, basename(fixture))
      writeLines(lines, file)
      expect_silent(m <- readModels(file, what = c("summaries", "class_counts")))
      expect_identical(m$summaries$Mplus.version, version)
      expect_identical(m$class_counts, baseline$class_counts)
      expect_identical(m$summaries$NLatentClasses, baseline$summaries$NLatentClasses)
    }
  }
})

test_that("BPARAMETERS reads retain parameter metadata with patch versions", {
  original <- readLines(test_path("ex3.18.out"))
  dir <- tempfile("bparameters-version-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  draws <- file.path(dir, "draws.dat")
  writeLines(c("1 0.1 0.2", "1 0.3 0.4", "1 0.5 0.6", "1 0.7 0.8"), draws)
  savedata <- c(
    "", "SAVEDATA INFORMATION", "",
    "  Bayesian Parameters", "",
    "  Save file", "    draws.dat", "",
    "  Order of parameters saved", "",
    "    Chain number",
    "    Parameter 1, [ Y ]",
    "    Parameter 2, [ C#1 ]", "",
    "     Beginning Time:  10:00:00"
  )

  for (version in c("9.1.1", "7.10.0", "7", "8.11")) {
    lines <- original
    lines[1L] <- paste("Mplus VERSION", version, "(Mac)")
    file <- file.path(dir, "bayes.out")
    writeLines(c(lines, savedata), file)
    expect_silent(m <- readModels(file, what = c("summaries", "bparameters", "savedata")))
    expect_identical(m$summaries$Mplus.version, version)
    expect_identical(m$savedata_info$bayesVarNames,
                     c("Chain.number", "Parameter.1_MEAN.Y", "Parameter.2_MEAN.C#1"))
    expect_s3_class(m$bparameters, "mplus.bparameters")
    expect_identical(ncol(m$bparameters$valid_draw), 3L)
    expect_equal(as.numeric(m$bparameters$valid_draw[, 3L]), c(0.6, 0.8))
  }

  text <- MplusAutomation:::parse_into_sections(c("SAVEDATA INFORMATION", savedata[-(1:3)]))
  for (summaries in list(NULL, list(Mplus.version = NA_character_), list(Mplus.version = "6.12.3"))) {
    expect_silent(info <- MplusAutomation:::l_getSavedata_Fileinfo("bayes.out", text, summaries))
    expect_identical(info$bayesVarNames, c("Chain.number", "Parameter.1_MEAN.Y"))
  }
  expect_silent(info <- MplusAutomation:::l_getSavedata_Fileinfo("bayes.out", text))
  expect_identical(info$bayesVarNames, c("Chain.number", "Parameter.1_MEAN.Y"))
})

test_that("class-count version checks use the first summary row and handle absent versions", {
  file <- test_path("mplus_ug", "8.11", "ch7", "ex7.15.out")
  text <- MplusAutomation:::parse_into_sections(readLines(file))
  summaries <- data.frame(NCategoricalLatentVars = c(3L, 3L), Mplus.version = c("9.1.1", "9.1.1"))
  expect_silent(counts <- MplusAutomation:::extractClassCounts(text, file, summaries))
  expect_identical(nrow(counts$modelEstimated), 6L)

  text <- MplusAutomation:::parse_into_sections(readLines(test_path("ex7.3.out")))
  expect_silent(counts <- MplusAutomation:::extractClassCounts(text, "mixture.out"))
  expect_identical(nrow(counts$modelEstimated), 2L)
  expect_silent(counts <- MplusAutomation:::extractClassCounts(
    text, "mixture.out", list(NCategoricalLatentVars = 1L, Mplus.version = NA_character_)
  ))
  expect_identical(nrow(counts$modelEstimated), 2L)
})
