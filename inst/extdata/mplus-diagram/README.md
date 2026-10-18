# Fixed Mplus diagram acceptance inputs

All `.out` files are genuine Mplus output, either downloaded unchanged from
Muthén & Muthén or estimated locally with Mplus 9 (Mac). `manifest.json` records
source URLs, archive members, engine versions, input/data hashes, and SHA-256
checksums. The four chapter-5 fixtures retain the exact historical bytes used for the
independent expectations and are isolated here from other package examples.

`tests/fixtures/mplus-diagram/inputs/` contains the exact input files for local runs.
`tests/fixtures/mplus-diagram/prepare-data.py`
recreates the synthetic data and optionally copies the public User's Guide
example data into an explicitly chosen temporary directory. It never runs
Mplus. Neither CI nor ordinary diagram creation needs an engine, license,
network connection, or individual-level data.

The seed 741920 panel is synthetic: 1,000 observations, three waves and two
constructs, with random intercepts and within-person lagged relations. It is
used unchanged for CLPM, explicit RI and hat RI, separately under ML and MLR.
The exact orthogonal `special.dat` design gives an estimated coefficient of
999 and an estimated zero. These are genuine engine estimates, not edited
output. `failed-convergence.out` was produced using ITERATIONS=1; the early
label-count failure is also preserved as actual error output.

The five-wave Web Talk 4 outputs are downloaded examples from the public
teaching archive. Only aggregate output is included, no raw MWI records.

Third-party outputs retain their original Muthén & Muthén copyright notices.
The official site distributes these examples publicly for teaching; an
additional open-source license for their text was not found. They are recorded
as attributed test/reference artifacts, not claimed as contributor-authored source code. Repository-authored inputs, synthetic-data recipe and expected-value
annotations are distinct from that third-party material. Review redistribution
terms before any broader bundled release if the maintainer requires an
explicit third-party grant.

Expectations in `tests/fixtures/mplus-diagram/*.expected.json` were transcribed
from the output tables and interpreted against the input echo, without using
the diagram parser or readModels() to generate them. They cover all loadings,
regressions and covariances in each positive fixture, plus specified boundary
constraints. They do not claim exhaustive mean/variance-table transcription.
The JSON `lhs/operator/rhs` follows Mplus notation: `Y ON X` means X → Y;
`F BY Y` means F → Y. `WITH` is undirected.

Synthetic object mutations (row reordering, partial results and missing tables)
are constructed directly by the tests in `tests/testthat/`. They are not presented
as engine output; the fixed `.out` files remain untouched.
