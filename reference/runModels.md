# Run Mplus Models

This function runs a group of Mplus models (.inp files) located within a
single directory or nested within subdirectories.

## Usage

``` r
runModels(
  target = getwd(),
  recursive = FALSE,
  filefilter = NULL,
  showOutput = FALSE,
  replaceOutfile = "always",
  logFile = "Mplus Run Models.log",
  Mplus_command = detectMplus(),
  killOnFail = TRUE,
  local_tmpdir = FALSE,
  quiet = TRUE
)
```

## Arguments

- target:

  a character vector where each element is a directory containing Mplus
  input files (.inp) to run OR a single .inp file to be run. Elements
  may be a full path, relative path, or a filename within the working
  directory. Defaults to the current working directory. Example:
  “C:/Users/Michael/Mplus Runs”

- recursive:

  optional. If `TRUE`, run all models nested in subdirectories within
  `directory`. Defaults to `FALSE`. Not relevant if `target` is a single
  file.

- filefilter:

  a Perl regular expression (PCRE-compatible) specifying particular
  input files to be run among those found in `target`. See `regex` or
  <https://www.pcre.org/pcre.txt> for details about regular expression
  syntax.

- showOutput:

  optional. If `TRUE`, show estimation output (TECH8) in the R console.
  Note that if run within Rgui, output will display within R, but if run
  via Rterm, a separate window will appear during estimation.

- replaceOutfile:

  optional. Currently supports three settings: “always”, which runs all
  models, regardless of whether an output file for the model exists;
  “never”, which does not run any model that has an existing output
  file; and “modifiedDate”, which only runs a model if the modified date
  for the input file is more recent than the output file modified date
  (implying there have been updates to the model).

- logFile:

  optional. If non-null, specifies a file (and optionally, directory)
  that records the settings passed into the function and the models run
  (or skipped) during the run.

- Mplus_command:

  optional. N.B.: No need to pass this parameter for most users (has
  intelligent defaults). Allows the user to specify the name/path of the
  Mplus executable to be used for running models. This covers situations
  where Mplus is not in the system's path, or where one wants to test
  different versions of the Mplus program.

- killOnFail:

  optional. Windows only for now. If `TRUE`, kill all processes named
  mplus.exe when `runModels` does not terminate normally. Defaults to
  `TRUE`.

- local_tmpdir:

  optional. Linux/Mac for now. If `TRUE`, set the TMPDIR environment
  variable to the location of the .inp file prior to execution. This is
  useful in Monte Carlo studies where many instances of Mplus may run in
  parallel and we wish to avoid collisions in temporary files among
  processes.

- quiet:

  optional. If `FALSE`, show status messages in the console.

## Value

None. Function is used for its side effects (running models).

## See also

[`runModels_Interactive`](https://michaelhallquist.github.io/MplusAutomation/reference/runModels_Interactive.md)

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
  runModels("C:/Users/Michael/Mplus Runs", recursive=TRUE, showOutput=TRUE,
    replaceOutfile="modifiedDate", logFile="MH_RunLog.txt",
    Mplus_command="C:\\Users\\Michael\\Mplus Install\\Mplus51.exe")
} # }
if (FALSE) { # \dontrun{
  runModels(getwd(), filefilter = "ex8.*", logFile=NULL)
} # }
```
