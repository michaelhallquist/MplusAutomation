test_that("TECH10 pattern-level sections are parsed", {
  model <- readModels(
    target = testthat::test_path("mplus_ug/8.11/ch7/ex7.15.out"),
    what = "tech10"
  )
  tech10 <- model$tech10

  expect_equal(nrow(tech10$univar_model_fit_info), 6)
  expect_equal(tech10$univar_model_fit_info$h1[1], "0.403")
  expect_equal(tech10$univar_chi_square$Pearson, rep("0.000", 3))
  expect_equal(
    tech10$univar_overall$value[tech10$univar_overall$stat == "Pearson"],
    "0.000"
  )

  expect_equal(nrow(tech10$univar_pattern), 48)
  expect_equal(nrow(tech10$bivar_pattern), 96)
  expect_setequal(
    unique(tech10$univar_pattern$LatentClassPattern),
    c("1.1.1", "1.1.2", "1.2.1", "1.2.2", "2.1.1", "2.1.2", "2.2.1", "2.2.2")
  )

  u1_u2_pattern <- subset(
    tech10$bivar_pattern,
    LatentClassPattern == "1.1.1" &
      var1 == "U1" & var2 == "U2" &
      cat1 == "Category 1" & cat2 == "Category 1"
  )
  expect_equal(u1_u2_pattern$observed, 1)
  expect_equal(u1_u2_pattern$stand_resid, -0.007)

  chi_square_row <- subset(
    tech10$bivar_chi_square,
    var1 == "U1" & var2 == "U2"
  )
  expect_equal(chi_square_row$Pearson, "0.408")
  expect_equal(chi_square_row$Significant, "0")
})


test_that("TECH10 class-level sections are parsed", {
  model <- readModels(
    target = testthat::test_path("mplus_ug/8.11/ch7/ex7.6.out"),
    what = "tech10"
  )
  tech10 <- model$tech10

  expect_equal(nrow(tech10$univar_class), 24)
  expect_setequal(unique(tech10$univar_class$LatentClass), c(1, 2))

  class1_u1 <- subset(tech10$univar_class, variable == "U1" & LatentClass == 1)
  expect_equal(class1_u1$observed, c(0.649, 0.110, 0.241))

  class2_u3 <- subset(tech10$univar_class, variable == "U3" & LatentClass == 2)
  expect_equal(class2_u3$observed[class2_u3$category == 1], 0.605)

  expect_equal(nrow(tech10$bivar_class), 108)
  expect_equal(
    tech10$bivar_chi_square$Significant[
      tech10$bivar_chi_square$var1 == "U1" &
        tech10$bivar_chi_square$var2 == "U2"
    ],
    "1"
  )

  bivar_summary <- subset(
    tech10$bivar_model_fit_info,
    var1 == "U1" & var2 == "U2" & cat1 == "Category 2" & cat2 == "Category 2"
  )
  expect_equal(bivar_summary$h1, "0.017")
  expect_equal(bivar_summary$z, "2.254")
})
