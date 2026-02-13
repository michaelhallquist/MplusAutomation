# Working with the mplusModel Object

## Overview

[`mplusModel()`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModel.md)
creates an R6 object that represents one Mplus model. It stores:

- model syntax
- model data
- `.inp`, `.dat`, and `.out` paths
- parsed output sections (after reading/running a model)

Compared with the traditional `mplusObject`, `mplusModel` is designed
around a single model lifecycle: create, update, write files,
run/submit, and inspect results.

## Create an `mplusModel`

``` r
model_syntax <- "
TITLE: OLS regression with mtcars data;
DATA: FILE IS mtcars_demo.dat;
VARIABLE:
  NAMES = mpg wt hp cyl qsec;
  USEVARIABLES = mpg wt hp;
ANALYSIS:
  ESTIMATOR = ML;
MODEL:
  mpg ON wt hp;
OUTPUT:
  STANDARDIZED;
"

model_data <- mtcars[, c("mpg", "wt", "hp", "cyl", "qsec")]

m <- mplusModel(
  syntax = model_syntax,
  data = model_data,
  inp_file = file.path(tmp_dir, "mtcars_demo.inp"),
  Mplus_command = fake_mplus
)

m$model_dir
#> [1] "/tmp/Rtmp9H865X/mplusModel_vignette_71bf430d98de"
m$inp_file
#> [1] "/tmp/Rtmp9H865X/mplusModel_vignette_71bf430d98de/mtcars_demo.inp"
m$dat_file
#> [1] "/tmp/Rtmp9H865X/mplusModel_vignette_71bf430d98de/mtcars_demo.dat"
m$variables
#> [1] "mpg" "wt"  "hp"
```

The object uses active bindings, so values like `m$inp_file` and
`m$dat_file` are always synchronized with the current object state.

## Write Mplus Files

`write_dat()` writes the data file, and `write_inp()` writes the Mplus
input syntax.

``` r
m$write_dat()
#> Writing data to file: /tmp/Rtmp9H865X/mplusModel_vignette_71bf430d98de/mtcars_demo.dat
m$write_inp()
#> Writing Mplus syntax to file: /tmp/Rtmp9H865X/mplusModel_vignette_71bf430d98de/mtcars_demo.inp

file.exists(m$dat_file)
#> [1] TRUE
file.exists(m$inp_file)
#> [1] TRUE
```

You can re-create an object from an existing `.inp` file:

``` r
m_from_inp <- mplusModel(
  inp_file = m$inp_file,
  read = FALSE,
  Mplus_command = fake_mplus
)

head(m_from_inp$syntax, n = 8)
#> [1] "TITLE:"                           "OLS regression with mtcars data;"
#> [3] "DATA:"                            "FILE = mtcars_demo.dat;"         
#> [5] "VARIABLE:"                        "NAMES = mpg wt hp;"              
#> [7] "USEVARIABLES = mpg wt hp;"        "MISSING = .;"
```

## Update Syntax and Variables

There are two update styles:

- non-mutating: `update(m, ...)` returns an updated clone
- in place: `m$update(...)` mutates the current object

For syntax sections, formulas support replace and append semantics:

- `~ "new text"` replaces
- `~ . + "additional text"` appends

``` r
m_clone <- update(
  m,
  MODEL = ~ . + "mpg ON cyl;",
  ANALYSIS = ~ "ESTIMATOR = MLR;"
)

any(grepl("mpg ON cyl;", m$syntax, fixed = TRUE))
#> [1] FALSE
any(grepl("mpg ON cyl;", m_clone$syntax, fixed = TRUE))
#> [1] TRUE
```

``` r
m$update(MODEL = ~ . + "mpg ON qsec;")
any(grepl("mpg ON qsec;", m$syntax, fixed = TRUE))
#> [1] TRUE
```

The `variables` binding controls which columns are written to the `.dat`
file:

``` r
m$variables
#> [1] "mpg"  "wt"   "hp"   "qsec"

m$variables <- c("mpg", "wt", "hp", "cyl")
m$variables
#> [1] "mpg" "wt"  "hp"  "cyl"

m$variables <- NULL
m$variables
#> [1] "mpg"  "wt"   "hp"   "qsec"
```

## Read Existing Output into `mplusModel`

This example uses an output file shipped with the package
(`inst/extdata/ex3.1.out`).

``` r
out_file <- system.file("extdata", "ex3.1.out", package = "MplusAutomation")
file.copy(out_file, file.path(tmp_dir, "ex3.1.out"), overwrite = TRUE)
#> [1] TRUE

writeLines(
  c(
    "TITLE: User's Guide Example 3.1;",
    "DATA: FILE IS ex3.1.dat;",
    "VARIABLE: NAMES ARE y1 x1 x3;",
    "MODEL: y1 ON x1 x3;"
  ),
  con = file.path(tmp_dir, "ex3.1.inp")
)

m_out <- mplusModel(
  inp_file = file.path(tmp_dir, "ex3.1.inp"),
  data = data.frame(y1 = 0, x1 = 0, x3 = 0),
  read = TRUE,
  Mplus_command = fake_mplus
)

m_out$summaries[c("AIC", "BIC", "CFI", "RMSEA_Estimate")]
#>        AIC      BIC CFI RMSEA_Estimate
#> 1 1396.667 1413.526   1              0
head(m_out$parameters$unstandardized[, c("paramHeader", "param", "est", "se", "pval")])
#>          paramHeader param   est    se pval
#> 1              Y1.ON    X1 0.969 0.042    0
#> 2              Y1.ON    X3 0.649 0.044    0
#> 3         Intercepts    Y1 0.511 0.043    0
#> 4 Residual.Variances    Y1 0.941 0.060    0
```

`mplusModel` exposes all major sections returned by
[`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md),
including `input`, `summaries`, `parameters`, `mod_indices`, `errors`,
and more.

## Run Locally or Submit to HPC

`mplusModel` wraps both local runs and scheduler submission. These
commands require a real Mplus executable.

``` r
# Run locally
m$run(replaceOutfile = "modifiedDate")

# Submit to SLURM/Torque (arguments passed through to submitModels())
m$submit(
  scheduler = "slurm",
  replaceOutfile = "modifiedDate",
  memgb_per_model = 8L,
  cores_per_model = 1L,
  time_per_model = "01:00:00"
)
```

## Summary

Use `mplusModel` when you want an object-oriented workflow for a single
model:

1.  define syntax and data
2.  write `.inp` / `.dat`
3.  update syntax programmatically
4.  run or submit
5.  read and inspect results
