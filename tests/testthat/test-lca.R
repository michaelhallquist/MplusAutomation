test_that("Mplus User Guide 7.3 - LCA results can be read in", {
  m <- readModels(target = testthat::test_path("ex7.3.out"))
  expect_equal(m$summaries$LL, -965.244)
  
  expect_equal(nrow(m$tech10$bivar_model_fit_info), 24)
  expect_equal(m$tech10$bivar_model_fit_info[1,], data.frame(
    var1 = "U1",
    var2 = "U2",
    cat1 = "Category 1",
    cat2 = "Category 1",
    h1 = "0.582",
    h0 = "0.578",
    z = "0.161"
  ))
  
  expect_equal(nrow(m$tech10$bivar_chi_square), 6)
  expect_equal(m$tech10$bivar_chi_square[1,], data.frame(
    var1 = "U1",
    var2 = "U2",
    Pearson = "0.162",
    `Log-Liklihood` = "0.163",
    Significant = "0",
    check.names = F
  ))
})

test_that("Mplus User Guide 7.3 - LCA results can be read in (tech10 error)", {
  m <- readModels(target = testthat::test_path("ex7.3_error.out"))
  expect_equal(m$summaries$LL, -965.244)
  expect_equal(m$tech10, list())
})

test_that("Mplus User Guide 7.3 - LCA results can be read in (older Mplus version)", {
  m <- readModels(target = testthat::test_path("ex7.3_old.out"))
  expect_equal(m$summaries$LL, -965.244)
  
  expect_equal(nrow(m$tech10$bivar_model_fit_info), 24)
  expect_equal(m$tech10$bivar_model_fit_info[1,], data.frame(
    var1 = "U1",
    var2 = "U2",
    cat1 = "Category 1",
    cat2 = "Category 1",
    h1 = "0.582",
    h0 = "0.578",
    z = "0.161"
  ))
  
  expect_equal(nrow(m$tech10$bivar_chi_square), 6)
  expect_equal(m$tech10$bivar_chi_square[1,], data.frame(
    var1 = "U1",
    var2 = "U2",
    Pearson = "0.162",
    `Log-Liklihood` = "0.163",
    check.names = F
  ))
})

# Output file obtained from running code from:
#  https://github.com/garberadamc/SEM-Lab9
test_that("Test LCA manual 3rd step", {
  m <- readModels(target = testthat::test_path("lca_man_3step.out"))
  expect_equal(nrow(m$parameters$unstandardized.alt$ref.cat.1), 6)
  expect_equal(m$parameters$unstandardized.alt$ref.cat.1$est[[1]], 0.294)
})