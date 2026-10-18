# Explicit local editing only; ordinary view/render has no Shiny dependency.
.mplus_diagram_edit <- function(widget, target) {
  for (pkg in c("shiny", "htmltools", "later")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      .mplus_diagram_abort(sprintf("Install the optional '%s' package to use action = 'edit'.", pkg), "edit")
    }
  }
  if (is.null(target)) .mplus_diagram_abort(
    "This object has no reliable source path. Supply config = 'path/model.diagram.json' before editing.", "edit")
  target <- normalizePath(target, winslash = "/", mustWork = FALSE)
  edit_widget <- widget
  edit_widget$x$editor <- list(input_id = "diagram_event")
  output_widget <- function(outputId, width = "100%", height = "80vh") {
    htmlwidgets::shinyWidgetOutput(outputId, "mplus_diagram", width, height, package = "MplusAutomation")
  }
  ui <- shiny::fluidPage(
    htmltools::tags$p("Editing project configuration: ", htmltools::tags$code(target)),
    htmltools::tags$p("Save and return writes this file. Cancel or closing the editor leaves it unchanged."),
    shiny::textOutput("save_status"), output_widget("diagram"))
  server <- .mplus_diagram_editor_server(widget, edit_widget, target, output_widget)
  launch <- function(url) {
    viewer <- getOption("viewer")
    if (is.function(viewer)) viewer(url) else utils::browseURL(url)
  }
  result <- shiny::runApp(shiny::shinyApp(ui, server), host = "127.0.0.1",
                         launch.browser = launch, quiet = TRUE)
  if (is.null(result)) widget else result
}

.mplus_diagram_editor_server <- function(widget, edit_widget, target, output_widget,
                                       finish = shiny::stopApp,
                                       defer = function(callback, seconds) later::later(callback, delay = seconds)) {
  force(widget); force(edit_widget); force(target); force(output_widget); force(finish); force(defer)
  shared <- new.env(parent = emptyenv())
  shared$widget <- edit_widget
  shared$sessions <- 0L
  shared$finished <- FALSE
  shared$handoff <- FALSE
  shared$handoff_id <- 0L
  function(input, output, session) {
    shared$sessions <- shared$sessions + 1L
    shared$handoff <- FALSE
    session_widget <- shared$widget
    status <- shiny::reactiveVal("Unsaved edits remain in this session until Save and return succeeds.")
    output$save_status <- shiny::renderText(status())
    output$diagram <- htmlwidgets::shinyRenderWidget(session_widget, output_widget,
                                                     env = environment(), quoted = FALSE)
    shiny::observeEvent(input$diagram_event, {
      event <- input$diagram_event
      if (!is.list(event) || !is.character(event$action) || length(event$action) != 1L) {
        status("Invalid editor request; nothing was saved.")
      } else if (identical(event$action, "cancel")) {
        shared$finished <- TRUE
        finish(widget)
      } else if (identical(event$action, "save")) {
        saved <- tryCatch(.mplus_diagram_write_config(event$config, target, widget$x$model),
                          error = identity)
        if (inherits(saved, "error")) {
          status(paste("Save failed:", conditionMessage(saved), "Continue editing or cancel."))
        } else {
          result <- widget
          result$x$state <- saved$state
          shared$finished <- TRUE
          finish(result)
        }
      } else if (identical(event$action, "browser")) {
        pending <- tryCatch(.mplus_diagram_validate_config(event$config, widget$x$model),
                            error = identity)
        if (inherits(pending, "error")) status(paste("Cannot transfer edits:", conditionMessage(pending)))
        else {
          shared$widget$x$state <- pending$state
          port <- as.character(session$clientData$url_port)
          if (is.character(port) && length(port) == 1L && grepl("^[0-9]+$", port)) {
            shared$handoff <- TRUE
            # Allow the new browser to connect before treating the old Viewer
            # disconnect as cancellation. Never save merely because it closes.
            shared$handoff_id <- shared$handoff_id + 1L
            defer(local({
              handoff_id <- shared$handoff_id
              function() {
                # A later transfer owns its own grace period. An older timer
                # must not end that transfer while the next browser connects.
                if (!identical(handoff_id, shared$handoff_id)) return(invisible(NULL))
                shared$handoff <- FALSE
                if (!shared$finished && shared$sessions == 0L) {
                  shared$finished <- TRUE
                  finish(widget)
                }
              }
            }), seconds = 30)
            utils::browseURL(paste0("http://127.0.0.1:", port))
            status("Opening your current edits in the browser. Wait for the diagram there before closing Viewer.")
          } else status("Browser address is unavailable; continue editing here.")
        }
      } else status("Unknown editor action; nothing was saved.")
    }, ignoreInit = TRUE)
    session$onSessionEnded(function() {
      shared$sessions <- shared$sessions - 1L
      if (!shared$finished && !shared$handoff && shared$sessions == 0L) {
        shared$finished <- TRUE
        finish(widget)
      }
    })
  }
}
