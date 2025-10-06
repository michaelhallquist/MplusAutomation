test_that("mplusModel R6 object can be initialized with syntax and data", {
  syn <- "
TITLE:  this is an example of a simple linear
        regression for a continuous observed
        dependent variable with two covariates
DATA:   FILE IS ex3.1.dat;
VARIABLE:       NAMES ARE y1 x1 x3;
MODEL:  y1 ON x1 x3;
"
  dat <- as.data.frame(data.table::fread(testthat::test_path("submitModels","ex3.1.dat"), data.table=FALSE))
  names(dat) <- c("y1","x1","x3")
  tmp <- normalizePath(tempdir())
  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(syntax = syn, data = dat, inp_file = file.path(tmp, "ex3.1.inp"), Mplus_command = mplus_fake)

  expect_true(inherits(m, "mplusModel_r6"))
  expect_equal(m$model_dir, tmp)
  expect_equal(m$inp_file, file.path(tmp, "ex3.1.inp"))
})

test_that("mplusModel writes input and data files", {
  syn <- "
TITLE:  this is an example of a simple linear
        regression for a continuous observed
        dependent variable with two covariates
DATA:   FILE IS ex3.1.dat;
VARIABLE:       NAMES ARE y1 x1 x3;
MODEL:  y1 ON x1 x3;
"
  
  dat <- as.data.frame(data.table::fread(testthat::test_path("submitModels","ex3.1.dat"), data.table=FALSE))
  names(dat) <- c("y1","x1","x3")
  tmp <- normalizePath(tempdir())
  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(syntax = syn, data = dat, inp_file = file.path(tmp, "ex3.1.inp"), Mplus_command = mplus_fake)
  m$write_dat()
  m$write_inp()
  expect_true(file.exists(m$dat_file))
  expect_true(file.exists(m$inp_file))
})

test_that("mplusModel reads existing output", {
  tmp <- tempdir()
  file.copy(testthat::test_path("submitModels","ex3.1.inp"), tmp)
  file.copy(testthat::test_path("submitModels","ex3.1.dat"), tmp)
  file.copy(testthat::test_path("ex3.1.out"), tmp)
  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(inp_file = file.path(tmp, "ex3.1.inp"), read = TRUE, Mplus_command = mplus_fake)
  expect_equal(m$summaries$AIC, 1396.667, tolerance = 1e-3)
  expect_equal(nrow(m$data), 500)
})

test_that("mplusModel falls back to basename for missing data file", {
  tmp <- tempdir()
  file.copy(testthat::test_path("submitModels", "ex3.1.dat"), tmp, overwrite = TRUE)
  file.copy(testthat::test_path("submitModels", "ex3.1.inp"), tmp, overwrite = TRUE)
  file.copy(testthat::test_path("ex3.1.out"), tmp, overwrite = TRUE)

  bad_path <- normalizePath(file.path(tempdir(), "nonexistent", "path", "ex3.1.dat"),
                            winslash = "/", mustWork = FALSE)
  inp_lines <- readLines(file.path(tmp, "ex3.1.inp"))
  inp_lines <- gsub("ex3.1.dat", bad_path, inp_lines, fixed = TRUE)
  writeLines(inp_lines, file.path(tmp, "ex3.1.inp"))
  out_lines <- readLines(file.path(tmp, "ex3.1.out"))
  out_lines <- gsub("ex3.1.dat", bad_path, out_lines, fixed = TRUE)
  writeLines(out_lines, file.path(tmp, "ex3.1.out"))

  mplus_fake <- tempfile(); file.create(mplus_fake)
  m <- mplusModel(inp_file = file.path(tmp, "ex3.1.inp"), read = TRUE, Mplus_command = mplus_fake)
  expect_equal(nrow(m$data), 500)
  expect_equal(m$dat_file, file.path(tmp, "ex3.1.dat"))
})

test_that("mplusModel exposes readModels sections", {
  tmp <- tempdir()
  file.copy(testthat::test_path("submitModels","ex3.1.inp"), tmp, overwrite = TRUE)
  file.copy(testthat::test_path("submitModels","ex3.1.dat"), tmp, overwrite = TRUE)
  file.copy(testthat::test_path("ex3.1.out"), tmp, overwrite = TRUE)
  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(inp_file = file.path(tmp, "ex3.1.inp"), read = TRUE, Mplus_command = mplus_fake)
  expect_true(length(m$output) > 0)
  expect_true(length(m$errors) == 0L)
})


test_that("mplusModel only rewrites input and data when changed", {
  syn <- "
TITLE:  this is an example of a simple linear
        regression for a continuous observed
        dependent variable with two covariates
DATA:   FILE IS ex3.1.dat;
VARIABLE:       NAMES ARE y1 x1 x3;
MODEL:  y1 ON x1 x3;
"
  dat <- as.data.frame(data.table::fread(
    testthat::test_path("submitModels", "ex3.1.dat"),
    data.table = FALSE
  ))
  names(dat) <- c("y1", "x1", "x3")
  tmp <- tempfile()
  dir.create(tmp)
  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = file.path(tmp, "ex3.1.inp"),
    Mplus_command = mplus_fake
  )
  
  
  fake_runModels <- function(target, ...) {
    file.create(sub("\\.inp$", ".out", target))
    invisible(NULL)
  }
  fake_readModels <- function(...) {
    list()
  }
  
  run_stub <- function() {
    testthat::with_mocked_bindings(
      m$run(replaceOutfile = "always"),
      runModels = fake_runModels,
      readModels = fake_readModels
    )
  }
  
  # run things the first time, get modified times
  run_stub()
  mtime_inp1 <- file.info(m$inp_file)$mtime
  mtime_dat1 <- file.info(m$dat_file)$mtime
  
  # run again with no changes -- should not write files again
  Sys.sleep(1)
  run_stub()
  mtime_inp2 <- file.info(m$inp_file)$mtime
  mtime_dat2 <- file.info(m$dat_file)$mtime
  
  expect_equal(mtime_inp2, mtime_inp1)
  expect_equal(mtime_dat2, mtime_dat1)
  
  # run again with new syntax -- inp changes, dat does not
  Sys.sleep(1)
  m$syntax <- c(m$syntax, "! new comment")
  run_stub()
  mtime_inp3 <- file.info(m$inp_file)$mtime
  mtime_dat3 <- file.info(m$dat_file)$mtime
  
  expect_gt(mtime_inp3, mtime_inp2)
  expect_equal(mtime_dat3, mtime_dat2)
  
  # run again with new data -- inp stays the same, dat changes
  Sys.sleep(1)
  dat2 <- m$data
  dat2$y1[1] <- dat2$y1[1] + 1
  m$data <- dat2
  run_stub()
  mtime_inp4 <- file.info(m$inp_file)$mtime
  mtime_dat4 <- file.info(m$dat_file)$mtime
  
  expect_equal(mtime_inp4, mtime_inp3)
  expect_gt(mtime_dat4, mtime_dat3)
})
