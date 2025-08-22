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
  m <- mplusModel(syntax = syn, data = dat, inp_file = file.path(tmp, "ex3.1.inp"))

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
  m <- mplusModel(syntax = syn, data = dat, inp_file = file.path(tmp, "ex3.1.inp"))
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
  m <- mplusModel(inp_file = file.path(tmp, "ex3.1.inp"), read = TRUE)
  expect_equal(m$summaries$AIC, 1396.667, tolerance = 1e-3)
  expect_equal(nrow(m$data), 500)
})

