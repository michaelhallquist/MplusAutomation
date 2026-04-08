test_that("readModels extracts grouped model_table rows from ex5.16", {
  m <- MplusAutomation::readModels(
    target = get_mplus_file("ch5/ex5.16.out", mplus_version = "8.11"),
    what = c("model_table"),
    quiet = TRUE
  )
  
  expect_s3_class(m$model_table, "mplus.model.syntax")
  expect_equal(
    names(m$model_table),
    c("paramHeader", "param", "modifier", "value", "fixed", "label", "Group", "source_section")
  )
  expect_equal(nrow(m$model_table), 15L)
  
  base_u1 <- subset(m$model_table, source_section == "model" & paramHeader == "F1.BY" & param == "U1")
  expect_equal(nrow(base_u1), 1L)
  expect_true(is.na(base_u1$value))
  expect_false(base_u1$fixed)
  
  female_u3 <- subset(m$model_table, source_section == "model.female" & paramHeader == "F1.BY" & param == "U3")
  expect_equal(nrow(female_u3), 1L)
  expect_equal(female_u3$Group, "FEMALE")
  expect_true(is.na(female_u3$value))
  
  female_scale <- subset(m$model_table, source_section == "model.female" & paramHeader == "Scales" & param == "U3")
  expect_equal(nrow(female_scale), 1L)
  expect_equal(female_scale$modifier, "@")
  expect_equal(female_scale$value, 1)
  expect_true(female_scale$fixed)
})

test_that("get_model_table returns the parsed model_table", {
  m <- MplusAutomation::readModels(
    target = get_mplus_file("ch5/ex5.16.out", mplus_version = "8.11"),
    what = c("model_table"),
    quiet = TRUE
  )
  
  expect_identical(MplusAutomation::get_model_table(m), m$model_table)
})

test_that("extractModelTable parses inline labels and modifiers from input syntax", {
  inp <- structure(
    list(model = c(
      "f1 BY y1* (a) y2*0.5 (b);",
      "[y1$1] (t1);",
      "f1@1 (v1);"
    )),
    class = c("mplus.inp", "list")
  )
  
  tab <- MplusAutomation:::extractModelTable(inp, "mock.out")
  
  expect_s3_class(tab, "mplus.model.syntax")
  expect_equal(nrow(tab), 4L)
  
  by_rows <- subset(tab, paramHeader == "F1.BY")
  expect_equal(by_rows$param, c("Y1", "Y2"))
  expect_equal(by_rows$label, c("a", "b"))
  expect_equal(by_rows$modifier, c("*", "*"))
  expect_true(is.na(by_rows$value[1]))
  expect_equal(by_rows$value[2], 0.5, tolerance = 1e-7)
  
  threshold_row <- subset(tab, paramHeader == "Thresholds" & param == "Y1$1")
  expect_equal(threshold_row$label, "t1")
  
  variance_row <- subset(tab, paramHeader == "Variances" & param == "F1")
  expect_equal(variance_row$label, "v1")
  expect_equal(variance_row$modifier, "@")
  expect_equal(variance_row$value, 1, tolerance = 1e-7)
  expect_true(variance_row$fixed)
})
