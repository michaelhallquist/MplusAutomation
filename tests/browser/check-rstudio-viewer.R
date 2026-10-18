# Run with source() in a fresh interactive macOS RStudio session after installing
# the reviewed MplusAutomation build. All files stay in a new temporary directory.
# 1. In Viewer, toggle Residuals; change a name and drag a node.
# 2. Try Open in browser, continue editing there, then Save and return.
# 3. Confirm the returned ordinary Viewer displays the same saved state.
# Repeat with Cancel and return if desired; a cancellation is reported distinctly.
local({
  if (!interactive() || !is.function(getOption("viewer")))
    stop("Run this script in an interactive RStudio session with a Viewer.")
  review_dir <- tempfile("lin-rstudio-review-")
  dir.create(review_dir)
  review_config <- file.path(review_dir, "cfa.diagram.json")
  review_out <- system.file("extdata", "mplus-diagram", "cfa-ml.out", package = "MplusAutomation")
  if (!nzchar(review_out)) stop("Install the current MplusAutomation build before this check.")
  message("Temporary review configuration: ", review_config)
  edited <- MplusAutomation::mplus_diagram(review_out, action = "edit", config = review_config,
    options = list(residuals = TRUE, stars = TRUE, nonsignificant_dotted = TRUE))
  print(edited)
  if (file.exists(review_config)) {
    reopened <- MplusAutomation::mplus_diagram(review_out, config = review_config)
    stopifnot(isTRUE(all.equal(edited$x$state, reopened$x$state)))
    message("PASS: saved configuration reproduces the returned widget state.")
  } else {
    message("No save was made: cancelled/closed editor left no configuration file.")
  }
  message("Record separately whether Viewer rendering and browser handoff looked correct;",
          " an R assertion cannot establish that visual result.")
})
