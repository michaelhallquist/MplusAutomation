# Fixed diagram fixtures

The `.expected.json` files contain independently transcribed node/relationship
identities, coefficients, p-value source rows, fixed values and semantic roles.
They are assertions about the input, not snapshots produced by the diagram parser.
Keep them when removing generated plots or development logs.

Raw `.out` files remain in `inst/extdata/mplus-diagram/` , because package examples and tests use `system.file()`.
Their manifest records exact hashes and origins. `validate-fixtures.py` compares
the manifest, literal expected values and source rows without R, network or an
Mplus executable.

`inputs/` and `prepare-data.py` are optional reproducibility sources for the
synthetic fixtures. They do not run during package installation or tests and
never invoke Mplus automatically. No individual-level research data is stored.
See the raw-output README for source attribution and redistribution notes.

Boundary variants such as reordered parameters or missing standardized rows are
constructed directly in `tests/testthat/`. They are explicitly synthetic copies;
the original output files are never modified by the tests.
