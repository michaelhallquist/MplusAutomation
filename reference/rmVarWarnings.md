# Remove variable name length warnings from Mplus output file

This function is designed to remove warnings in Mplus output files about
variable names being greater than 8 characters. It replaces them with a
note that the warnings were removed and a count of how many warnings
were removed.

## Usage

``` r
rmVarWarnings(file)
```

## Arguments

- file:

  A file name (including path if necessary) to an Mplus output file.
  Note that you must have read and write privileges on the file for this
  function to work properly.

## Value

Usually NULL. Called for the side effect of removing warnings in Mplus
output files. If
[`file.access`](https://rdrr.io/r/base/file.access.html) testing for
write permissions returns `FALSE`, a character string note that
`rmVarWarnings` could not run.

## Details

This is an internal function and not meant to be directly called by the
user under most circumstances. It is called by
[`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.md)
when the `varwarnings = FALSE` argument is used.

## See also

[`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.md)

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
# to do
```
