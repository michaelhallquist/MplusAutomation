# Create an mplusModel object for a given model

Create an mplusModel object for a given model

## Usage

``` r
mplusModel(
  syntax = NULL,
  data = NULL,
  inp_file = NULL,
  out_file = NULL,
  dir = NULL,
  file_stem = NULL,
  read = TRUE,
  Mplus_command = NULL
)
```

## Arguments

- syntax:

  a character vector of Mplus input syntax for this model

- data:

  a data.frame to be used for estimating the model

- inp_file:

  the location of .inp file for this model

- out_file:

  the location of .out file for this model

- dir:

  the directory for this model's files

- file_stem:

  the shared filename stem for this model's `.inp`, `.out`, `.gh5`, and
  `.dat` files

- read:

  If TRUE and the .out file already exists, read the contents of the
  .out file using `readModels`

- Mplus_command:

  The location of the Mplus executable to run. If NULL, use
  [`detectMplus()`](https://michaelhallquist.github.io/MplusAutomation/reference/detectMplus.md)

## Value

a `mplusModel_r6` object containing information about the model
