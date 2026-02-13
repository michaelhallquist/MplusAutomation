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

test_that("write_inp honors explicit inp_file argument", {
  syn <- "
DATA:   FILE IS ex3.1.dat;
VARIABLE:       NAMES ARE y1 x1 x3;
MODEL:  y1 ON x1 x3;
"

  dat <- as.data.frame(data.table::fread(
    testthat::test_path("submitModels", "ex3.1.dat"),
    data.table = FALSE
  ))
  names(dat) <- c("y1", "x1", "x3")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  default_inp <- file.path(tmp_dir, "default.inp")
  alternate_inp <- file.path(tmp_dir, "alternate.inp")
  mplus_fake <- tempfile()
  file.create(mplus_fake)

  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = default_inp,
    Mplus_command = mplus_fake
  )

  m$write_dat(quiet = TRUE)
  m$write_inp(inp_file = alternate_inp, overwrite = TRUE, quiet = TRUE)

  expect_true(file.exists(alternate_inp))
  expect_false(file.exists(default_inp))
  expect_equal(readLines(alternate_inp), m$syntax)
})

test_that("write_inp uses filename when data is in the model directory", {
  syn <- "
DATA:   FILE IS ex3.1.dat;
VARIABLE:       NAMES ARE y1 x1 x3;
MODEL:  y1 ON x1 x3;
"

  dat <- as.data.frame(data.table::fread(
    testthat::test_path("submitModels", "ex3.1.dat"),
    data.table = FALSE
  ))
  names(dat) <- c("y1", "x1", "x3")
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  inp_path <- file.path(tmp_dir, "model.inp")
  mplus_fake <- tempfile()
  file.create(mplus_fake)

  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = inp_path,
    Mplus_command = mplus_fake
  )

  m$write_dat(quiet = TRUE)
  m$write_inp(overwrite = TRUE, quiet = TRUE)

  file_line <- m$syntax[grep("^FILE", m$syntax)]
  expect_equal(file_line, 'FILE = "ex3.1.dat";')
})

test_that("write_inp preserves absolute data paths outside the model directory", {
  syn <- "
DATA:   FILE IS ex3.1.dat;
VARIABLE:       NAMES ARE y1 x1 x3;
MODEL:  y1 ON x1 x3;
"

  dat <- as.data.frame(data.table::fread(
    testthat::test_path("submitModels", "ex3.1.dat"),
    data.table = FALSE
  ))
  names(dat) <- c("y1", "x1", "x3")

  base_dir <- tempfile()
  dir.create(base_dir)
  model_dir <- file.path(base_dir, "model")
  dir.create(model_dir)
  external_dir <- file.path(base_dir, "external")
  dir.create(external_dir)

  inp_path <- file.path(model_dir, "model.inp")
  external_path <- file.path(external_dir, "external.dat")
  mplus_fake <- tempfile()
  file.create(mplus_fake)

  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = inp_path,
    Mplus_command = mplus_fake
  )

  m$.__enclos_env__$private$set_dat_file(external_path)
  m$write_dat(quiet = TRUE)
  m$write_inp(overwrite = TRUE, quiet = TRUE)

  file_idx <- grep("^FILE", m$syntax)
  following <- m$syntax[file_idx:length(m$syntax)]
  end_idx <- file_idx - 1 + which(grepl('";$', following))[1]
  data_lines <- m$syntax[file_idx:end_idx]
  data_block <- paste(data_lines, collapse = "")
  expected_spec <- gsub("//+", "/", gsub("\\\\", "/", external_path))
  expected_path <- normalizePath(external_path, winslash = "/", mustWork = FALSE)

  expect_true(grepl(expected_spec, data_block, fixed = TRUE))
  expect_equal(normalizePath(m$dat_file, winslash = "/", mustWork = FALSE), expected_path)
})

test_that("write_inp preserves relative data paths outside the model directory", {
  syn <- "
DATA:   FILE IS ex3.1.dat;
VARIABLE:       NAMES ARE y1 x1 x3;
MODEL:  y1 ON x1 x3;
"

  dat <- as.data.frame(data.table::fread(
    testthat::test_path("submitModels", "ex3.1.dat"),
    data.table = FALSE
  ))
  names(dat) <- c("y1", "x1", "x3")

  base_dir <- tempfile()
  dir.create(base_dir)
  model_dir <- file.path(base_dir, "model")
  dir.create(model_dir)
  external_dir <- file.path(base_dir, "external")
  dir.create(external_dir)

  inp_path <- file.path(model_dir, "model.inp")
  rel_path <- file.path("..", basename(external_dir), "external.dat")
  mplus_fake <- tempfile()
  file.create(mplus_fake)

  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = inp_path,
    Mplus_command = mplus_fake
  )

  m$.__enclos_env__$private$set_dat_file(rel_path)
  m$write_dat(quiet = TRUE)
  m$write_inp(overwrite = TRUE, quiet = TRUE)

  file_idx <- grep("^FILE", m$syntax)
  following <- m$syntax[file_idx:length(m$syntax)]
  end_idx <- file_idx - 1 + which(grepl('";$', following))[1]
  data_lines <- m$syntax[file_idx:end_idx]
  data_block <- paste(data_lines, collapse = "")

  expected_spec <- gsub("\\\\", "/", rel_path)
  expect_true(grepl(expected_spec, data_block, fixed = TRUE))

  expected_path <- normalizePath(file.path(model_dir, rel_path), winslash = "/", mustWork = FALSE)
  expect_equal(normalizePath(m$dat_file, winslash = "/", mustWork = FALSE), expected_path)
})

test_that("write_inp round-trips long wrapped data paths", {
  syn <- "
DATA:   FILE IS ex3.1.dat;
VARIABLE:       NAMES ARE y1 x1 x3;
MODEL:  y1 ON x1 x3;
"

  dat <- as.data.frame(data.table::fread(
    testthat::test_path("submitModels", "ex3.1.dat"),
    data.table = FALSE
  ))
  names(dat) <- c("y1", "x1", "x3")

  base_dir <- tempfile()
  dir.create(base_dir)
  model_dir <- file.path(base_dir, "model")
  dir.create(model_dir)
  external_dir <- file.path(
    base_dir,
    "segment_aaaaaaaaaaaaaaaaaaaa",
    "segment_bbbbbbbbbbbbbbbbbbbb",
    "segment_cccccccccccccccccccc",
    "segment_dddddddddddddddddddd"
  )
  dir.create(external_dir, recursive = TRUE)

  inp_path <- file.path(model_dir, "model.inp")
  external_path <- file.path(external_dir, "external_long_filename.dat")
  expect_gt(nchar(external_path), 90)
  mplus_fake <- tempfile()
  file.create(mplus_fake)

  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = inp_path,
    Mplus_command = mplus_fake
  )

  m$.__enclos_env__$private$set_dat_file(external_path)
  m$write_dat(quiet = TRUE)
  m$write_inp(overwrite = TRUE, quiet = TRUE)

  inp_lines <- readLines(inp_path)
  file_idx <- grep("^\\s*FILE\\s*=", inp_lines)
  following <- inp_lines[file_idx:length(inp_lines)]
  end_idx <- file_idx - 1 + which(grepl(";\\s*$", following))[1]
  data_file_lines <- inp_lines[file_idx:end_idx]
  expect_lte(max(nchar(data_file_lines)), 90)

  m2 <- mplusModel(inp_file = inp_path, read = FALSE, Mplus_command = mplus_fake)
  expect_equal(
    normalizePath(m2$dat_file, winslash = "/", mustWork = FALSE),
    normalizePath(external_path, winslash = "/", mustWork = FALSE)
  )
  expect_true(file.exists(m2$dat_file))
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
  expect_equal(
    normalizePath(m$dat_file, winslash = "/", mustWork = FALSE),
    normalizePath(file.path(tmp, "ex3.1.dat"), winslash = "/", mustWork = FALSE)
  )
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

test_that("update() on mplusModel supports formula replace and append", {
  syn <- "
TITLE: update semantics test;
VARIABLE: NAMES = y x z;
MODEL: y ON x;
"
  dat <- data.frame(y = rnorm(10), x = rnorm(10), z = rnorm(10))
  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = file.path(tempdir(), "update_formula.inp"),
    Mplus_command = mplus_fake
  )

  m2 <- update(
    m,
    MODEL = ~ . + "y ON z;",
    ANALYSIS = ~ "ESTIMATOR = MLR;"
  )

  expect_false(any(grepl("y ON z;", m$syntax, fixed = TRUE)))
  expect_true(any(grepl("y ON x;", m2$syntax, fixed = TRUE)))
  expect_true(any(grepl("y ON z;", m2$syntax, fixed = TRUE)))
  expect_true(any(grepl("ESTIMATOR = MLR;", m2$syntax, fixed = TRUE)))
})

test_that("mplusModel$update mutates in place and update() clones by default", {
  syn <- "
VARIABLE: NAMES = y x z;
MODEL: y ON x;
"
  dat <- data.frame(y = rnorm(10), x = rnorm(10), z = rnorm(10))
  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = file.path(tempdir(), "update_clone.inp"),
    Mplus_command = mplus_fake
  )

  m_clone <- update(m, MODEL = ~ . + "y ON z;")
  expect_false(identical(m, m_clone))
  expect_false(any(grepl("y ON z;", m$syntax, fixed = TRUE)))
  expect_true(any(grepl("y ON z;", m_clone$syntax, fixed = TRUE)))

  m$update(MODEL = ~ . + "y ON z;")
  expect_true(any(grepl("y ON z;", m$syntax, fixed = TRUE)))
})

test_that("update() clears loaded output when syntax changes", {
  tmp <- tempdir()
  file.copy(testthat::test_path("submitModels", "ex3.1.inp"), tmp, overwrite = TRUE)
  file.copy(testthat::test_path("submitModels", "ex3.1.dat"), tmp, overwrite = TRUE)
  file.copy(testthat::test_path("ex3.1.out"), tmp, overwrite = TRUE)
  mplus_fake <- tempfile()
  file.create(mplus_fake)

  m <- mplusModel(inp_file = file.path(tmp, "ex3.1.inp"), read = TRUE, Mplus_command = mplus_fake)
  expect_false(is.null(m$summaries))

  m2 <- update(m, MODEL = ~ . + "y1 ON x1;")

  expect_false(is.null(m$summaries))
  expect_true(is.null(m2$summaries))
})

test_that("update() supports usevariables overrides and reset", {
  syn <- "
VARIABLE: NAMES = y x z;
MODEL: y ON x;
"
  dat <- data.frame(y = rnorm(10), x = rnorm(10), z = rnorm(10))
  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = file.path(tempdir(), "update_variables.inp"),
    Mplus_command = mplus_fake
  )

  expect_equal(m$variables, c("y", "x"))

  m2 <- update(m, usevariables = c("y", "z"))
  expect_equal(m2$variables, c("y", "z"))

  m3 <- update(m2, usevariables = NULL)
  expect_equal(m3$variables, c("y", "x"))
})
