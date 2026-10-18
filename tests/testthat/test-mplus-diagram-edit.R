skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

test_that("local editor writes only validated state and returns a passive widget", {
  skip_if_not_installed("shiny")
  w <- suppressWarnings(mplus_diagram(system.file("extdata", "mplus-diagram", "ex5.11.out", package = "MplusAutomation")))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  cfg <- .mplus_diagram_config(w)
  cfg$state$aliases[[w$x$model$structure$nodes[[1]]$id]] <- "保存后的变量"
  returned <- NULL
  output_fun <- function(outputId, width = "100%", height = "400px")
    htmlwidgets::shinyWidgetOutput(outputId, "mplus_diagram", width, height, package = "MplusAutomation")
  server <- .mplus_diagram_editor_server(w, w, path, output_fun,
    finish = function(value) returned <<- value)
  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(diagram_event = list(action = "save", config = cfg))
    expect_true(file.exists(path))
    expect_s3_class(returned, "htmlwidget")
    expect_null(returned$x$editor)
    expect_equal(returned$x$state$aliases, cfg$state$aliases)
  })
})

test_that("cancel and save failure preserve the previous file", {
  skip_if_not_installed("shiny")
  w <- suppressWarnings(mplus_diagram(system.file("extdata", "mplus-diagram", "ex5.11.out", package = "MplusAutomation")))
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path))
  cfg <- .mplus_diagram_config(w)
  .mplus_diagram_write_config(cfg, path, w$x$model)
  before <- readBin(path, "raw", file.info(path)$size)
  returned <- NULL
  output_fun <- function(outputId, width = "100%", height = "400px")
    htmlwidgets::shinyWidgetOutput(outputId, "mplus_diagram", width, height, package = "MplusAutomation")
  server <- .mplus_diagram_editor_server(w, w, path, output_fun,
    finish = function(value) returned <<- value)
  shiny::testServer(server, {
    session$flushReact()
    bad <- cfg; bad$fingerprint <- "wrong-model"
    session$setInputs(diagram_event = list(action = "save", config = bad))
    expect_null(returned)
    expect_match(output$save_status, "Save failed")
    expect_identical(readBin(path, "raw", file.info(path)$size), before)
    session$setInputs(diagram_event = list(action = "cancel"))
    expect_equal(returned$x$state, w$x$state)
    expect_identical(readBin(path, "raw", file.info(path)$size), before)
  })
})

test_that("browser handoff preserves edits across a Viewer disconnect gap", {
  skip_if_not_installed("shiny")
  withr::local_options(browser = function(...) invisible(TRUE))
  w <- suppressWarnings(mplus_diagram(system.file("extdata", "mplus-diagram", "ex5.11.out", package = "MplusAutomation")))
  cfg <- .mplus_diagram_config(w)
  cfg$state$aliases[[w$x$model$structure$nodes[[1]]$id]] <- "Pending browser edits"
  returned <- NULL
  delayed <- list()
  output_fun <- function(outputId, width = "100%", height = "400px")
    htmlwidgets::shinyWidgetOutput(outputId, "mplus_diagram", width, height, package = "MplusAutomation")
  server <- .mplus_diagram_editor_server(w, w, tempfile(fileext = ".json"), output_fun,
    finish = function(value) returned <<- value,
    defer = function(callback, seconds) delayed[[length(delayed) + 1L]] <<- callback)
  shiny::testServer(server, {
    session$flushReact()
    session$setInputs(diagram_event = list(action = "browser", config = cfg))
    expect_match(output$save_status, "Opening your current edits")
    session$close()
    expect_null(returned)
    expect_length(delayed, 1L)
  })
  shiny::testServer(server, {
    session$flushReact()
    expect_equal(session_widget$x$state$aliases, cfg$state$aliases)
    session$setInputs(diagram_event = list(action = "cancel"))
    expect_equal(returned$x$state, w$x$state)
  })
})

test_that("an older handoff timeout cannot cancel a newer browser transfer", {
  skip_if_not_installed("shiny")
  withr::local_options(browser = function(...) invisible(TRUE))
  w <- suppressWarnings(mplus_diagram(system.file("extdata", "mplus-diagram", "ex5.11.out", package = "MplusAutomation")))
  cfg <- .mplus_diagram_config(w)
  returned <- NULL
  delayed <- list()
  output_fun <- function(outputId, width = "100%", height = "400px")
    htmlwidgets::shinyWidgetOutput(outputId, "mplus_diagram", width, height, package = "MplusAutomation")
  server <- .mplus_diagram_editor_server(w, w, tempfile(fileext = ".json"), output_fun,
    finish = function(value) returned <<- value,
    defer = function(callback, seconds) delayed[[length(delayed) + 1L]] <<- callback)
  for (attempt in seq_len(2L)) {
    shiny::testServer(server, {
      session$flushReact()
      session$setInputs(diagram_event = list(action = "browser", config = cfg))
      session$close()
    })
  }
  expect_null(returned)
  expect_length(delayed, 2L)
  delayed[[1]]()
  expect_null(returned)
  delayed[[2]]()
  expect_equal(returned$x$state, w$x$state)
})
