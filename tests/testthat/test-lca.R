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
    `Log-Likelihood` = "0.163",
    Significant = "0",
    check.names = F
  ))

  expect_equal(nrow(m$tech10$univar_class), 16)
  expect_equal(m$tech10$univar_class[1,], data.frame(
    variable = "U1",
    category = 1,
    observed = 0.113,
    estimated = 0.113,
    resid = 0,
    stand_resid = 0,
    LatentClass = 1
  ))

  expect_equal(nrow(m$tech10$bivar_class), 48)
  expect_equal(m$tech10$bivar_class[1,], data.frame(
    var1 = "U1",
    var2 = "U2",
    cat1 = "Category 1",
    cat2 = "Category 1",
    observed = 0.028,
    estimated = 0.017,
    resid = 0.011,
    stand_resid = 0.974,
    LatentClass = 1
  ))
})

test_that("Mplus User Guide 7.3 - LCA results can be read in (tech10 error)", {
  m <- readModels(target = testthat::test_path("ex7.3_error.out"))
  expect_equal(m$summaries$LL, -965.244)
  expect_setequal(names(m$tech10), c("bivar_class", "univar_class"))
  expect_gt(nrow(m$tech10$univar_class), 0)
  expect_gt(nrow(m$tech10$bivar_class), 0)
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
    `Log-Likelihood` = "0.163",
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

# Mplus 9 uses different header: "LOGISTIC REGRESSION ODDS RATIO RESULTS FOR LATENT CLASS VARIABLES"
test_that("Mplus 9 - LCA with covariates odds ratios are parsed", {
  m <- readModels(target = testthat::test_path("three_cov.out"))
  
  # odds ratios should be present
  expect_false(is.null(m$parameters$odds))
  expect_equal(nrow(m$parameters$odds), 3L)
  expect_equal(m$parameters$odds$est[1L], 1.309)
  expect_equal(m$parameters$odds$lower_2.5ci[1L], 1.078)
  expect_equal(m$parameters$odds$upper_2.5ci[1L], 1.589)
  
  # alternative parameterizations should be present
  expect_false(is.null(m$parameters$unstandardized.alt))
  expect_equal(length(m$parameters$unstandardized.alt), 3L)
})

test_that("TECH11 VLMR values are parsed when warning text interrupts the section", {
  m <- readModels(target = testthat::test_path("threeclass1.out"))
  
  expect_equal(m$summaries$T11_VLMR_PValue, 0.6519, tolerance = 1e-4)
  expect_equal(m$summaries$T11_LMR_PValue, 0.6515, tolerance = 1e-4)
  
  section_alerts <- attr(m$summaries, "section_alerts")
  expect_true("T11_VLMR" %in% names(section_alerts))
  expect_length(section_alerts$T11_VLMR, 1L)
  expect_true(grepl("DID NOT\\s+TERMINATE NORMALLY", section_alerts$T11_VLMR[[1L]], perl = TRUE))
})
