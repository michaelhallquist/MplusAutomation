# Browser and installed-package checks

Run commands from the repository root unless noted. These checks use the shipped
widget and real installed-package HTML. Synthetic graphs in `*.test.cjs` exercise
rendering/interaction only; fixed Mplus files and independent expectations
separately validate parsing. CI does not run Mplus or use private study data.

## JavaScript interaction

```sh
npm ci --prefix tests/browser
# From tests/browser, install Playwright Chromium once:
# npx playwright install chromium
npm --prefix tests/browser test
```

Set `CHROME_PATH` to an existing Chromium executable if needed. The suite checks
scale isolation, APA labels/inference, editing, undo/redo, JSON download/import,
PNG/SVG, residual rotation, covariance-side dragging and straight/curve/polyline
routing (including overlapped path selection) using real pointer events.
`MPLUS_DIAGRAM_HTML` adds an installed real-report check;
`MPLUS_DIAGRAM_CONFIG_EXPORT` preserves its downloaded JSON for R-side validation.

## Fresh R processes and the full model matrix

```sh
Rscript --vanilla tests/browser/workflow-check.R . /tmp/mplus-workflow
MPLUS_DIAGRAM_HTML=/tmp/mplus-workflow/workflow.html npm --prefix tests/browser test
Rscript --vanilla tests/browser/render-model-matrix.R . /tmp/mplus-matrix /tmp/mplus-workflow/library
node tests/browser/model-matrix.test.cjs /tmp/mplus-matrix/manifest.json
```

Set `RSTUDIO_PANDOC` if Pandoc is not discoverable. The workflow installs only
inside the requested artifact directory and checks that a second R process renders
without an editor, Viewer or config write. The matrix covers 24 positive models
in both views and scales. Its optional third argument reuses the same isolated
library; omitting it creates another isolated installation. Both modes compare
installed function bodies and widget hashes with the checkout, rejecting stale
libraries before producing acceptance results.

For arrowhead editing followed by a fresh public R import:

```sh
MPLUS_COVARIANCE_HTML=/tmp/mplus-matrix/cfa-ml.html \
MPLUS_COVARIANCE_SEM_HTML=/tmp/mplus-matrix/sem-ml.html \
MPLUS_COVARIANCE_CONFIG_EXPORT=/tmp/mplus-matrix/cfa-right.diagram.json \
  node tests/browser/covariance-side.test.cjs
Rscript --vanilla tests/browser/restore-covariance-side.R \
  /tmp/mplus-workflow/library /tmp/mplus-matrix/cfa-right.diagram.json \
  /tmp/mplus-matrix/cfa-restored.html
```

## Visual fixtures, generated when needed

```sh
R_LIBS=/tmp/mplus-workflow/library \
  Rscript --vanilla tests/browser/render-gallery.R /tmp/mplus-gallery
MPLUS_DIAGRAM_GALLERY=/tmp/mplus-gallery \
MPLUS_VISUAL_ARTIFACTS=/tmp/mplus-visuals \
  node tests/browser/visual.test.cjs
```

The generator verifies installed R function bodies and widget assets against the
checkout. It creates ten individual pages and `gallery.html`, with local browser
dependencies. The visual suite checks bounds, labels, covariance geometry, fonts
and pointer edits. Screenshots, configs and measurements are generated artifacts,
not tracked baselines or substitutes for human visual review.

To reproduce the README and PR previews from that gallery:

```sh
node tests/browser/capture-example.cjs /tmp/mplus-gallery vignettes/figures
```

## Official Firefox

```sh
python3 tests/browser/mplus-diagram-webdriver.py \
  --firefox /path/to/firefox --geckodriver /path/to/geckodriver \
  --html /tmp/mplus-workflow/workflow.html /tmp/mplus-gallery/gallery.html \
  --artifacts /tmp/mplus-firefox-results
```

The artifact directory must be new/empty. Temporary profiles and a non-listening
proxy isolate the test and block external HTTP(S). The suite checks actual editing,
exact JSON downloads/imports, standalone SVG/PNG and reload/instance isolation.
Original Blob JSON is compared with the download; any one-ULP differences in
WebDriver object transport are logged separately without relaxing equality.

`setup-firefox-linux.sh` downloads pinned official Firefox/geckodriver archives
into a supplied empty directory and verifies hashes and versions. It does not
change user browser profiles or system settings. Required shared libraries must
already be installed; CI provisions them explicitly.

## Interactive desktop check

Run `source("tests/browser/check-rstudio-viewer.R")` in a fresh interactive
RStudio session with the current package installed. It uses temporary files for
Viewer → browser → Save and return and checks saved-state reuse. The optional
`mplus-diagram-shiny.test.cjs` can drive an already-running editor using
`MPLUS_DIAGRAM_SHINY_URL`; message submission alone does not prove disk save, so
also verify the returned R widget/config.

Generated files belong under temporary directories or ignored `artifacts/`.
`node_modules/` is a reinstallable dependency cache and is not committed.
