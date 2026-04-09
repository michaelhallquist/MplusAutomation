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

## Three Ways to Start

There are three common entry points:

1.  Start from syntax and data when you want to define a model in R and
    choose its file location with `dir` and `file_stem`.
2.  Start from an existing `.inp` file when the input syntax already
    exists on disk.
3.  Start from an existing `.out` file when you want to recover a model
    object from Mplus output, even if the `.inp` file is missing.

In all three cases, the object tracks a canonical file identity through
`dir` and `file_stem`, and derives the `.inp`, `.out`, `.gh5`, and
`.dat` paths from that state.

## Start from Syntax and Data

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
  dir = tmp_dir,
  file_stem = "mtcars_demo",
  Mplus_command = fake_mplus
)

m$dir
#> [1] "/tmp/RtmpyPtJB6/mplusModel_vignette_25662f6db15f"
m$file_stem
#> [1] "mtcars_demo"
m$model_dir
#> [1] "/tmp/RtmpyPtJB6/mplusModel_vignette_25662f6db15f"
m$inp_file
#> [1] "/tmp/RtmpyPtJB6/mplusModel_vignette_25662f6db15f/mtcars_demo.inp"
m$dat_file
#> [1] "/tmp/RtmpyPtJB6/mplusModel_vignette_25662f6db15f/mtcars_demo.dat"
m$variables
#> [1] "mpg" "wt"  "hp"
```

The object uses active bindings, so values like `m$dir`, `m$file_stem`,
`m$inp_file`, and `m$dat_file` are always synchronized with the current
object state. To relocate the model files, update `m$dir` and/or
`m$file_stem`; `write_dat()` and `write_inp()` always write to those
canonical locations.

## Start from an Existing `.inp` File

If you already have an Mplus input file, initialize the object from
`inp_file`.

``` r
m$write_dat()
#> Writing data to file: /tmp/RtmpyPtJB6/mplusModel_vignette_25662f6db15f/mtcars_demo.dat
m$write_inp()
#> Writing Mplus syntax to file: /tmp/RtmpyPtJB6/mplusModel_vignette_25662f6db15f/mtcars_demo.inp

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

## Write Mplus Files

`write_dat()` writes the data file, and `write_inp()` writes the Mplus
input syntax.

``` r
m$write_dat()
#> The file(s)
#>  'mtcars_demo.dat' 
#> currently exist(s) and will be overwritten
#> Writing data to file: /tmp/RtmpyPtJB6/mplusModel_vignette_25662f6db15f/mtcars_demo.dat
m$write_inp()
#> Writing Mplus syntax to file: /tmp/RtmpyPtJB6/mplusModel_vignette_25662f6db15f/mtcars_demo.inp

file.exists(m$dat_file)
#> [1] TRUE
file.exists(m$inp_file)
#> [1] TRUE
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

## Start from an Existing `.out` File

This example uses an output file shipped with the package
(`inst/extdata/ex3.1.out`). The constructor rebuilds syntax from the
echoed input stored in the output file and, when `read = TRUE`, also
loads the parsed results.

``` r
out_file <- system.file("extdata", "ex3.1.out", package = "MplusAutomation")
file.copy(out_file, file.path(tmp_dir, "ex3.1.out"), overwrite = TRUE)
#> [1] TRUE

m_out <- mplusModel(
  out_file = file.path(tmp_dir, "ex3.1.out"),
  data = data.frame(y1 = 0, x1 = 0, x3 = 0),
  read = TRUE,
  Mplus_command = fake_mplus
)

m_out$inp_file
#> [1] "/tmp/RtmpyPtJB6/mplusModel_vignette_25662f6db15f/ex3.1.inp"
m_out$syntax[1:4]
#> [1] "TITLE:"                                                                              
#> [2] "this is an example of a simple linear regression for a continuous observed dependent"
#> [3] "    variable with two covariates"                                                    
#> [4] "DATA:"
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

1.  Start from `syntax` + `dir` + `file_stem`, an existing `inp_file`,
    or an existing `out_file`.
2.  Work with canonical file paths through `dir`, `file_stem`,
    `inp_file`, `out_file`, and `dat_file`.
3.  Write `.inp` / `.dat`, update syntax programmatically, and run or
    submit the model.
4.  Read and inspect parsed output sections from the corresponding
    `.out` file.
