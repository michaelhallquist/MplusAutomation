# Plot latent transition model

Plots latent transition probabilities and classification probabilities
for a single latent transition model (a model with multiple categorical
latent variables, regressed on one another). Stroke thickness of nodes
represents the proportion of cases most likely assigned to that class,
with wider strokes representing greater probability. Edge thickness and
transparency represent the probability of making a particular transition
(left to right), with thicker/darker edges representing greater
probability.

## Usage

``` r
plotLTA(
  mplusModel,
  node_stroke = 2,
  max_edge_width = 2,
  node_labels = "variable.class",
  x_labels = "variable"
)
```

## Arguments

- mplusModel:

  A single Mplus model object, returned by . This function additionally
  requires the model to be a mixture model with multiple categorical
  latent variables.

- node_stroke:

  Integer. Base stroke thickness for nodes. Set to `NULL` to give each
  node the same stroke thickness.

- max_edge_width:

  Integer. Maximum width of edges.

- node_labels:

  Character vector, defaults to `"variable.class"`, which labels each
  node by the name of the variable, and the number of the class it
  represents. Set to `"class"` to display only class numbers, or provide
  a named character vector where the names correspond to original class
  labels, and the values correspond to their substitute values.

- x_labels:

  Character vector, defaults to `"variable"`, which labels the x-axis
  with the names of the categorical latent variables. Set to `NULL` to
  remove axis labels, or provide a named character vector where the
  names correspond to original x-axis labels, and the values correspond
  to their substitute values.

## Value

An object of class 'ggplot'.

## Author

Caspar J. van Lissa

## Examples

``` r
if (FALSE) { # \dontrun{
mydat <- read.csv(
system.file("extdata", "ex8.13.csv", package = "MplusAutomation"))
createMixtures(
classes = 2,
filename_stem = "dating",
model_overall = "c2 ON c1;",
model_class_specific = c(
"[u11$1] (a{C});  [u12$1] (b{C});  [u13$1] (c{C});  [u14$1] (d{C});  [u15$1] (e{C});",
"[u21$1] (a{C});  [u22$1] (b{C});  [u23$1] (c{C});  [u24$1] (d{C});  [u25$1] (e{C});"
),
rdata = mydat,
ANALYSIS = "PROCESSORS IS 2;  LRTSTARTS (0 0 40 20);  PARAMETERIZATION = PROBABILITY;",
VARIABLE = "CATEGORICAL = u11-u15 u21-u25;"
)
runModels(filefilter = "dating")
results <- readModels(filefilter = "dating")
plotLTA(results)
} # }
```
