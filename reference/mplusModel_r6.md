# mplusModel R6 class

An R6 class for a single Mplus model

## Details

Wrapped by `mplusModel`

## Active bindings

- `model_dir`:

  the directory for Mplus files corresponding to this model

- `inp_file`:

  the location of the Mplus .inp file for this model

- `out_file`:

  the location of the Mplus .out file for this model

- `dat_file`:

  the location of the Mplus .dat (data) file for this model

- `data`:

  the dataset used for estimating this model

- `variables`:

  variables to write to the .dat file. Set NULL to restore automatic
  detection.

- `Mplus_command`:

  the location of the Mplus program

- `syntax`:

  the Mplus syntax for this model as a character vector

- `input`:

  Read-only accessor for the `input` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `warnings`:

  Read-only accessor for the `warnings` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `errors`:

  Read-only accessor for the `errors` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `data_summary`:

  Read-only accessor for the `data_summary` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `sampstat`:

  Read-only accessor for the `sampstat` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `covariance_coverage`:

  Read-only accessor for the `covariance_coverage` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `summaries`:

  Read-only accessor for the `summaries` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `invariance_testing`:

  Read-only accessor for the `invariance_testing` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `parameters`:

  Read-only accessor for the `parameters` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `class_counts`:

  Read-only accessor for the `class_counts` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `indirect`:

  Read-only accessor for the `indirect` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `mod_indices`:

  Read-only accessor for the `mod_indices` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `residuals`:

  Read-only accessor for the `residuals` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `savedata`:

  Read-only accessor for the `savedata` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `savedata_info`:

  Read-only accessor for the `savedata_info` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `bparameters`:

  Read-only accessor for the `bparameters` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `tech1`:

  Read-only accessor for the `tech1` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `tech3`:

  Read-only accessor for the `tech3` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `tech4`:

  Read-only accessor for the `tech4` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `tech7`:

  Read-only accessor for the `tech7` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `tech8`:

  Read-only accessor for the `tech8` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `tech9`:

  Read-only accessor for the `tech9` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `tech10`:

  Read-only accessor for the `tech10` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `tech12`:

  Read-only accessor for the `tech12` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `tech15`:

  Read-only accessor for the `tech15` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `fac_score_stats`:

  Read-only accessor for the `fac_score_stats` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `lcCondMeans`:

  Read-only accessor for the `lcCondMeans` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `r3step`:

  Read-only accessor for the `r3step` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `gh5`:

  Read-only accessor for the `gh5` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `h5results`:

  Read-only accessor for the `h5results` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

- `output`:

  Read-only accessor for the `output` section returned by
  [`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md).

## Methods

### Public methods

- [`mplusModel_r6$new()`](#method-mplusModel_r6-new)

- [`mplusModel_r6$read()`](#method-mplusModel_r6-read)

- [`mplusModel_r6$write_dat()`](#method-mplusModel_r6-write_dat)

- [`mplusModel_r6$write_inp()`](#method-mplusModel_r6-write_inp)

- [`mplusModel_r6$submit()`](#method-mplusModel_r6-submit)

- [`mplusModel_r6$run()`](#method-mplusModel_r6-run)

- [`mplusModel_r6$update()`](#method-mplusModel_r6-update)

- [`mplusModel_r6$clone()`](#method-mplusModel_r6-clone)

------------------------------------------------------------------------

### Method `new()`

generate an mplusModel_r6 object

#### Usage

    mplusModel_r6$new(
      syntax = NULL,
      data = NULL,
      inp_file = NULL,
      read = TRUE,
      Mplus_command = NULL
    )

#### Arguments

- `syntax`:

  a character vector of Mplus input syntax for this model

- `data`:

  a data.frame to be used for estimating the model

- `inp_file`:

  the location of .inp file for this model

- `read`:

  If TRUE and the .out file already exists, read the contents of the
  .out file using `readModels`

- `Mplus_command`:

  N.B.: No need to pass this parameter for most users (has intelligent
  defaults). Allows the user to specify the name/path of the Mplus
  executable to be used for running models. This covers situations where
  Mplus is not in the system's path, or where one wants to test
  different versions of the Mplus program.

------------------------------------------------------------------------

### Method `read()`

read the results of an Mplus model from the .out file using `readModels`

#### Usage

    mplusModel_r6$read(force = FALSE)

#### Arguments

- `force`:

  if `TRUE`, re-read the .out file even if was already previously loaded
  in to this object

------------------------------------------------------------------------

### Method `write_dat()`

write the .inp and .dat files for this model to the intended location

#### Usage

    mplusModel_r6$write_dat(overwrite = TRUE, quiet = FALSE, ...)

#### Arguments

- `overwrite`:

  if `TRUE`, overwrite existing data. Default: `TRUE`.

- `quiet`:

  if `TRUE`, do not produce messages about the outcome of this command
  (e.g., a message about overwriting existing data)

- `...`:

  additional arguments passed to `prepareMplusData`

------------------------------------------------------------------------

### Method `write_inp()`

write the .inp and .dat files for this model to the intended location

#### Usage

    mplusModel_r6$write_inp(overwrite = TRUE, inp_file = NULL, quiet = FALSE)

#### Arguments

- `overwrite`:

  if `TRUE`, overwrite existing data. Default: `TRUE`.

- `inp_file`:

  The location of the input file to write. If NULL (default), use the
  `$inp_file` of this object.

- `quiet`:

  if `TRUE`, do not produce messages about the outcome of this command
  (e.g., a message about overwriting existing data)

------------------------------------------------------------------------

### Method `submit()`

submit this model for estimation on an HPC using `submitModels`

#### Usage

    mplusModel_r6$submit(replaceOutfile = "modifiedDate", ...)

#### Arguments

- `replaceOutfile`:

  Currently supports three settings: "always", which runs all models,
  regardless of whether an output file for the model exists; "never",
  which does not run any model that has an existing output file; and
  "modifiedDate", which only runs a model if the modified date for the
  input file is more recent than the output file modified date (implying
  there have been updates to the model).

- `...`:

  additional arguments passed to `submitModels`

------------------------------------------------------------------------

### Method `run()`

run this model locally using `runModels`. The method detects changes in
the data and input syntax and only rewrites the corresponding files when
updates are detected.

#### Usage

    mplusModel_r6$run(replaceOutfile = "modifiedDate", ...)

#### Arguments

- `replaceOutfile`:

  Currently supports three settings: "always", which runs all models,
  regardless of whether an output file for the model exists; "never",
  which does not run any model that has an existing output file; and
  "modifiedDate", which only runs a model if the modified date for the
  input file is more recent than the output file modified date (implying
  there have been updates to the model).

- `...`:

  additional arguments passed to `runModels`

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update model sections using
[`update()`](https://rdrr.io/r/stats/update.html)-style formula
semantics.

#### Usage

    mplusModel_r6$update(..., in_place = TRUE, quiet = TRUE)

#### Arguments

- `...`:

  Named updates. For Mplus input sections, use formulas: `~ "new text"`
  replaces, `~ . + "additional text"` appends.

- `in_place`:

  If `TRUE` (default), mutate this object. If `FALSE`, return an updated
  clone.

- `quiet`:

  If `TRUE`, suppress status messages.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    mplusModel_r6$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
