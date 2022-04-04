# I'm trying to run several LCA's, but when I try the example from
# https://rdrr.io/cran/MplusAutomation/man/createMixtures.html, I get the
# following error for the first line of code (## Not run:
# createMixtures(classes = 1:3, filename_stem = "iris", rdata = iris):

# Error in check_mixtures(modelList) : mixtureSummaryTable requires a list of
# mixture models as its first argument. In addition: Warning message: In
# any(mixtures) : coercing argument of type 'list' to logical


test_that("createmixtures works", {
  skip_on_cran()
  expect_error({
    out <- createMixtures(classes = 1:3, filename_stem = "iris", rdata = iris)
    suppressWarnings(print(out))
  }, NA)
})
