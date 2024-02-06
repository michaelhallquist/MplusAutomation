# test use of long file paths in prepareMplusData
test_that("prepareMplusData long path", {
  data(iris)
  subdirs <- replicate(3, paste(sample(letters, 25, replace=TRUE), collapse=""), simplify = FALSE)
  path <- do.call(file.path, c(tempdir(), subdirs))
  expect_gte(nchar(path), 80) # make sure we force a long enough path
  dir.create(path, recursive = TRUE)
  ff <- file.path(path, "iris.dat")
  syn <- prepareMplusData(iris, filename=ff)
  dline <- grep("^DATA:", syn)
  dlens <- sapply(strsplit(syn[dline], "\n")[[1]], nchar)
  expect_no_error(dat <- read.table(ff))
  expect_lt(max(dlens), 90) # ensure that longest line is less than 90 chars
  file.remove(ff)
})

test_that("prepareMplusData short path", {
  data(iris)
  path <- tempdir()
  expect_lte(nchar(path), 70) # make sure we force a short enough path
  ff <- file.path(path, "i.dat")
  syn <- prepareMplusData(iris, filename=ff)
  dline <- grep("^DATA:", syn)
  dlens <- sapply(strsplit(syn[dline], "\n")[[1]], nchar)
  expect_no_error(dat <- read.table(ff))
  expect_lt(max(dlens), 90) # ensure that longest line is less than 90 chars
  file.remove(ff)
})
