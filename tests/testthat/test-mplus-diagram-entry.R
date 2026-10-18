skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

test_that("diagram entry rejects ambiguous inputs before reading models", {
  expect_error(mplus_diagram(list()), "explicit select", class = "mplus_diagram_input_error")
  expect_error(mplus_diagram(tempdir()), "explicit select")
  expect_error(mplus_diagram(list(a = 1), select = "missing"), "exactly one")
  expect_error(mplus_diagram(list(a = 1), select = 2), "positive index")
  expect_error(mplus_diagram(1), "single MplusAutomation")
  expect_error(mplus_diagram("missing.out"), "existing .out")
  expect_error(mplus_diagram(NULL, options = list(digits = 9)), "digits")
  expect_error(mplus_diagram(NULL, options = list(mystery = TRUE)), "supported option")
  expect_error(mplus_diagram(NULL, config = NA_character_), "filename")
})

test_that("noninteractive editing fails immediately instead of blocking render", {
  withr::local_options(knitr.in.progress = TRUE)
  expect_error(mplus_diagram(NULL, action = "edit"), "interactive R session",
               class = "mplus_diagram_edit_error")
})
