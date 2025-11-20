# local function that does the work of getSaveData_Fileinfo

This function is split out so that `getSaveData_Fileinfo` is exposed to
the user, but the parsing function can be used by `readModels`

## Usage

``` r
l_getSavedata_Fileinfo(outfile, outfiletext, summaries)
```

## Arguments

- outfile:

  A character string giving the name of the Mplus output file.

- outfiletext:

  The contents of the output file, for example as read by `scan`

## Value

A list that includes:

- `fileName`: The name of the file containing the analysis dataset
  created by the Mplus SAVEDATA command.

- `fileVarNames`: A character vector containing the names of variables
  in the dataset.

- `fileVarFormats`: A character vector containing the Fortran-style
  formats of variables in the dataset.

- `fileVarWidths`: A numeric vector containing the widths of variables
  in the dataset (which is stored in fixed-width format).

- `bayesFile`: The name of the BPARAMETERS file containing draws from
  the posterior distribution created by the Mplus SAVEDATA BPARAMETERS
  command.

- `bayesVarNames`: A character vector containing the names of variables
  in the BPARAMETERS dataset.

- `tech3File`: A character vector of the tech 3 output.

- `tech4File`: A character vector of the tech 4 output.

## See also

[`getSavedata_Data`](https://michaelhallquist.github.io/MplusAutomation/reference/getSavedata_Data.md)

## Examples

``` r
# make me!
```
