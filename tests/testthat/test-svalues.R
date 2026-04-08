test_that("readModels extracts raw SVALUES text and parsed parameter rows", {
  m <- MplusAutomation::readModels(
    target = get_mplus_file("ch8/ex8.6_svalues.out", mplus_version = "8.11"),
    what = c("parameters", "svalues"),
    quiet = TRUE
  )
  
  expect_true("svalues" %in% names(m))
  expect_type(m$svalues, "character")
  expect_true(any(grepl("%OVERALL%", m$svalues, fixed = TRUE)))
  expect_true(any(grepl("%C#1%", m$svalues, fixed = TRUE)))
  
  expect_true("svalues" %in% names(m$parameters))
  expect_s3_class(m$parameters$svalues, "mplus.params")
  expect_equal(
    names(m$parameters$svalues),
    c("paramHeader", "param", "est", "fixed", "label", "LatentClass")
  )
  expect_equal(nrow(m$parameters$svalues), 50L)
  
  growth_y4 <- subset(m$parameters$svalues, paramHeader == "S.|" & param == "Y4")
  expect_equal(growth_y4$est, c(3, 3))
  expect_equal(growth_y4$fixed, c(TRUE, TRUE))
  expect_equal(growth_y4$LatentClass, c("1", "2"))
  
  cat_latent <- subset(m$parameters$svalues, LatentClass == "Categorical.Latent.Variables")
  expect_equal(nrow(cat_latent), 2L)
  expect_equal(cat_latent$est[cat_latent$paramHeader == "C#1.ON"], -1.09543, tolerance = 1e-7)
  expect_equal(cat_latent$est[cat_latent$paramHeader == "Intercepts"], -0.01861, tolerance = 1e-7)
  
  threshold_1 <- subset(
    m$parameters$svalues,
    paramHeader == "Thresholds" & param == "U$1" & LatentClass == "1"
  )
  expect_equal(threshold_1$est, 1.01878, tolerance = 1e-7)
  expect_false(threshold_1$fixed)
  expect_true(is.na(threshold_1$label))
  
  resid_y1 <- subset(
    m$parameters$svalues,
    paramHeader == "Residual.Variances" & param == "Y1" & LatentClass == "2"
  )
  expect_equal(resid_y1$est, 0.60796, tolerance = 1e-7)
  expect_false(resid_y1$fixed)
  expect_equal(resid_y1$label, "1")
  
  reg_1 <- subset(
    m$parameters$svalues,
    paramHeader == "I.ON" & param == "X" & LatentClass == "1"
  )
  expect_equal(reg_1$label, "7")
})

test_that("readModels can return SVALUES text without parameter extraction", {
  m <- MplusAutomation::readModels(
    target = get_mplus_file("ch8/ex8.6_svalues.out", mplus_version = "8.11"),
    what = c("svalues"),
    quiet = TRUE
  )
  
  expect_true("svalues" %in% names(m))
  expect_true("parameters" %in% names(m))
  expect_true("svalues" %in% names(m$parameters))
  expect_true(any(grepl("c#1 ON x\\*-1\\.09543", m$svalues, perl = TRUE)))
})

test_that("SVALUES parser matches ordinary parameter tables on dedicated fixtures", {
  files <- sort(list.files(
    testthat::test_path("svalues"),
    pattern = "[.]out$",
    full.names = TRUE
  ))
  expect_equal(length(files), 2L)
  
  for (f in files) {
    m <- MplusAutomation::readModels(
      target = f,
      what = c("parameters", "svalues"),
      quiet = TRUE
    )
    
    expect_true("unstandardized" %in% names(m$parameters), info = basename(f))
    expect_true("svalues" %in% names(m$parameters), info = basename(f))
    
    u <- m$parameters$unstandardized
    s <- m$parameters$svalues
    expect_equal(nrow(s), nrow(u), info = basename(f))
    
    keycols <- intersect(
      c("paramHeader", "param", "LatentClass", "BetweenWithin", "Group", "ReferenceClass"),
      union(names(u), names(s))
    )
    
    u2 <- u[, c(keycols, "est"), drop = FALSE]
    s2 <- s[, c(keycols, "est"), drop = FALSE]
    names(u2)[names(u2) == "est"] <- "est_u"
    names(s2)[names(s2) == "est"] <- "est_s"
    
    cmp <- merge(u2, s2, by = keycols, all = TRUE)
    mism <- cmp[
      is.na(cmp$est_u) |
        is.na(cmp$est_s) |
        abs(cmp$est_u - cmp$est_s) > 1e-3,
      ,
      drop = FALSE
    ]
    
    expect_equal(nrow(mism), 0L, info = basename(f))
  }
})

test_that("readModels extracts grouped SVALUES rows and labels from ex5.16", {
  m <- MplusAutomation::readModels(
    target = get_mplus_file("ch5/ex5.16.out", mplus_version = "8.11"),
    what = c("parameters", "svalues"),
    quiet = TRUE
  )
  
  expect_true("svalues" %in% names(m))
  expect_true("svalues" %in% names(m$parameters))
  expect_s3_class(m$parameters$svalues, "mplus.params")
  expect_equal(
    names(m$parameters$svalues),
    c("paramHeader", "param", "est", "fixed", "label", "Group")
  )
  
  male_u2 <- subset(m$parameters$svalues, Group == "MALE" & paramHeader == "F1.BY" & param == "U2")
  expect_equal(nrow(male_u2), 1L)
  expect_equal(male_u2$est, 0.95730, tolerance = 1e-7)
  expect_false(male_u2$fixed)
  expect_equal(male_u2$label, "7")
  
  female_u3 <- subset(m$parameters$svalues, Group == "FEMALE" & paramHeader == "F1.BY" & param == "U3")
  expect_equal(nrow(female_u3), 1L)
  expect_equal(female_u3$est, 0.77675, tolerance = 1e-7)
  expect_false(female_u3$fixed)
  expect_true(is.na(female_u3$label))
  
  female_u1_threshold <- subset(
    m$parameters$svalues,
    Group == "FEMALE" & paramHeader == "Thresholds" & param == "U1$1"
  )
  expect_equal(nrow(female_u1_threshold), 1L)
  expect_equal(female_u1_threshold$est, -0.90507, tolerance = 1e-7)
  expect_equal(female_u1_threshold$label, "1")
})
