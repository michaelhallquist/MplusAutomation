# Run Mplus Models Using Graphical Interface

This function is provides a graphical user interface to the `runModels`
function. It uses Tcl/Tk to display a window in which the user can
specify parameters for `runModels`, including the directory for runs,
recursing through subdirectories, displaying output on the console, and
replacing existing outfiles.

## Usage

``` r
runModels_Interactive(
  directory = getwd(),
  recursive = "0",
  showOutput = "1",
  replaceOutfile = "1",
  checkDate = "0",
  logFile = "1"
)
```

## Arguments

- directory:

  optional. The starting directory that will display in the dialog
  window. Defaults to the current working directory.

- recursive:

  optional. Whether the recursive checkbox should be checked when the
  window opens. “0” for `FALSE`, “1” for `TRUE`.

- showOutput:

  optional. Whether the show output checkbox should be checked when the
  window opens. “0” for `FALSE`, “1” for `TRUE`.

- replaceOutfile:

  optional. Whether the replace outfile checkbox should be checked when
  the window opens. “0” for `FALSE`, “1” for `TRUE`.

- checkDate:

  optional. Whether the check modified date checkbox should be checked
  when the window opens. “0” for `FALSE`, “1” for `TRUE`.

- logFile:

  optional. Whether the log file checkbox should be checked when the
  window opens. “0” for `FALSE`, “1” for `TRUE`.

## Value

None. Function is used to display user interface for running models.

## Details

This function exists as a GUI wrapper for `runModels` and does not
provide any distinct functionality.

## See also

[`runModels`](https://michaelhallquist.github.io/MplusAutomation/reference/runModels.md)

## Author

Michael Hallquist

## Examples

``` r
# interactive, none
```
