# Create syntax for a batch of mixture models

Dynamically creates syntax for a batch of mixture models, with
intelligent defaults. This function is a wrapper around `mplusObject`
and `mplusModeler`, and the respective arguments of those functions can
be passed on using `...`. For instance, passing the argument `run = 1L`
means that the models will be evaluated and returned.

## Usage

``` r
createMixtures(
  classes = 1L,
  filename_stem = NULL,
  model_overall = NULL,
  model_class_specific = NULL,
  rdata = NULL,
  usevariables = NULL,
  OUTPUT = "TECH11 TECH14;",
  SAVEDATA = "FILE IS {filename_stem}_{C}.dat;  SAVE = cprobabilities;",
  quiet = TRUE,
  ...
)
```

## Arguments

- classes:

  A vector of integers, indicating which class solutions to generate.
  Defaults to 1L. E.g., `classes = 1:6`, `classes = c(1:4, 6:8)`.

- filename_stem:

  Character. A stem for the automatically generated filenames of the
  syntax and data files.

- model_overall:

  Character. Mplus syntax for the overall model (across classes).

- model_class_specific:

  Character vector. Mplus syntax for the class-specific model(s) of one
  or more categorical latent variables. Each element of
  `model_class_specific` is used as the class-specific syntax of a
  different categorical latent variable. This allows one to easily
  specify latent transition analyses (see second example). The character
  string “{C}” is substituted with the correct class number, for example
  to set unique parameter labels for each class, or to specify equality
  constraints.

- rdata:

  Data.frame. An R dataset to be used for the model.

- usevariables:

  Character vector, specifying the names of variables in the rdata
  object which should be included in the Mplus data file and model.

- OUTPUT:

  Character. Syntax for Mplus' OUTPUT option. Highly recommended when
  determining the appropriate number of latent classes. TECH11 is
  required to obtain the VLMR-test; TECH14 is required for the BLR-test.

- SAVEDATA:

  Character. Syntax for Mplus' savedata option. Highly recommended when
  conducting mixture models. The default option will often be adequate.

- quiet:

  optional. If `TRUE`, show status messages in the console.

- ...:

  Additional arguments, passed to
  [mplusObject](https://michaelhallquist.github.io/MplusAutomation/reference/mplusObject.md),
  such as syntax for other Mplus options.

## Value

None, unless the argument `run = 1L` is specified. In that case, a list
with elements of class `mplusObject` is returned. Otherwise, this
function is used for its side effects (generating syntax).

## Details

In the arguments `model_class_specific` and `SAVEDATA`, the character
string “{C}” is substituted with the correct class number. The character
string “{filename_stem}” is substituted with the filename stem, for
example, to name savedata in line with the input files.

In all arguments to `mplusObject`, a double space (“ ”) is replaced with
a newline character. This can be used to obtain nicely formatted Mplus
syntax.

## See also

[`mplusObject`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusObject.md),
[`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.md)

## Author

Caspar J. van Lissa

## Examples

``` r
if (FALSE) { # \dontrun{
createMixtures(classes = 1:3, filename_stem = "iris", rdata = iris)
} # }
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
} # }
```
