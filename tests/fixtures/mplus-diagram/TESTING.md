# Diagram regression checks

The R tests use fixed Mplus output and independently transcribed expectations.
They do not estimate models or require an Mplus executable. The browser tests
are separate from `devtools::test()` and exercise the installed HTML widget.

From the repository root:

```sh
Rscript --vanilla -e 'devtools::test(filter = "mplus-diagram")'
python3 tests/fixtures/mplus-diagram/validate-fixtures.py
npm ci --prefix tests/browser
npm exec --prefix tests/browser -- playwright install chromium
npm --prefix tests/browser test
```

See [browser instructions](../../browser/README.md) for isolated installation,
fresh-process rendering, the 24-model matrix, and optional desktop checks.
Fixed output, inputs and expected-value JSON files are regression resources;
generated screenshots, reports and logs normally belong in a temporary directory.
The two README screenshots are intentional documentation assets, reproducible
using `render-gallery.R` followed by `capture-example.cjs`.

For the entire upstream R suite, use `NOT_CRAN=false` to skip its optional test
that estimates mixture models with Mplus:

```sh
NOT_CRAN=false Rscript --vanilla -e 'devtools::test()'
```
