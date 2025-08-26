# tests/testthat/test-expandCmd.R

test_that("expands simple identifier ranges", {
  expect_equal(expandCmd("y1-y3"), "y1 y2 y3")
  expect_equal(expandCmd("x10-x12"), "x10 x11 x12")
})

test_that("expands multiple ranges on one line", {
  expect_equal(expandCmd("y1-y3 y5-y6"), "y1 y2 y3 y5 y6")
  expect_equal(expandCmd("y1-y3, y5-y6;"), "y1 y2 y3, y5 y6;")
})

test_that("propagates @ suffix from right token to all", {
  expect_equal(expandCmd("y1-y3@1"), "y1@1 y2@1 y3@1")
  expect_equal(expandCmd("BY y1-y3@0.5;"), "BY y1@0.5 y2@0.5 y3@0.5;")
})

test_that("propagates * suffix from right token to all (with or without value)", {
  expect_equal(expandCmd("y1-y3*5"), "y1*5 y2*5 y3*5")
  expect_equal(expandCmd("y1-y3*"), "y1* y2* y3*")
  expect_equal(expandCmd("LOAD BY y1-y2*1;"), "LOAD BY y1*1 y2*1;")
})

test_that("respects token boundaries (commas, semicolons, parentheses, whitespace)", {
  expect_equal(expandCmd("(y1-y3)"), "(y1 y2 y3)")
  expect_equal(expandCmd("y1-y3, y7-y8;"), "y1 y2 y3, y7 y8;")
  expect_equal(expandCmd("BY y1-y3 ;"), "BY y1 y2 y3 ;")
  expect_equal(expandCmd("BY\ny1-y3;"), "BY\ny1 y2 y3;")
})

test_that("does not expand arithmetic subtraction or mixed numeric expressions", {
  # classic subtraction cases should remain unchanged
  expect_equal(expandCmd("x3*1 -4.25*x4"), "x3*1 -4.25*x4")
  expect_equal(expandCmd("MODEL CONSTRAINT: a = b1-b2;"), "MODEL CONSTRAINT: a = b1-b2;")
  expect_equal(expandCmd("c = (u2 - u1) / s;"), "c = (u2 - u1) / s;")
})

test_that("prefix mismatch does not expand", {
  expect_equal(expandCmd("y1-z3"), "y1-z3")
  expect_equal(expandCmd("alpha10-beta12"), "alpha10-beta12")
})

test_that("descending ranges are left unchanged", {
  expect_equal(expandCmd("y3-y1"), "y3-y1")
  expect_equal(expandCmd("BY y5-y4;"), "BY y5-y4;")
})

test_that("multiple independent ranges with different suffixes expand correctly", {
  expect_equal(
    expandCmd("y1-y2@1 z1-z2*2"),
    "y1@1 y2@1 z1*2 z2*2"
  )
})

test_that("numeric ranges expand in list-like contexts (if supported)", {
  # Only expect this if your expandCmd implements numeric list expansion.
  # If you *disable* numeric expansion, change these to expect equality with input.
  expect_equal(expandCmd("1-3", expand_numeric = TRUE), "1 2 3")
  expect_equal(expandCmd("NAMES ARE 1-3;", expand_numeric = TRUE), "NAMES ARE 1 2 3;")
  # Should not trigger in arithmetic context:
  expect_equal(expandCmd("d = 1 - 3;"), "d = 1 - 3;")
})

test_that("whitespace variations around hyphen are handled", {
  expect_equal(expandCmd("y1 - y3"), "y1 y2 y3")
  expect_equal(expandCmd("y1-  y3"), "y1 y2 y3")
  expect_equal(expandCmd("y1  -y3"), "y1 y2 y3")
})

test_that("no-op when there are no expandable ranges", {
  expect_equal(expandCmd("BY y1 y2 y3;"), "BY y1 y2 y3;")
  expect_equal(expandCmd("! comment line - with hyphen"), "! comment line - with hyphen")
})

# Stress / regression cases
test_that("multiple ranges and arithmetic in one line", {
  input  <- "BY y1-y3@1 y5-y6 x1 - 2*(x3 - x1) z10-z12*;"
  expect_equal(
    expandCmd(input),
    "BY y1@1 y2@1 y3@1 y5 y6 x1 - 2*(x3 - x1) z10* z11* z12*;"
  )
})

