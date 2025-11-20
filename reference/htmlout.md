# Read in Mplus Output File from HTML on the Mplus Website

This is an internal utility function to help with testing
MplusAutomation. On the Mplus website, many output files are included,
which are good tests. However, they are stored in HTML files. This
function reads them in, strips the HTML parts leaving just the output
file, and then writes it to disk and returns the file name, which can be
used with
[`readModels()`](https://michaelhallquist.github.io/MplusAutomation/reference/readModels.md)
for testing.

## Usage

``` r
htmlout(url)
```

## Arguments

- url:

  The url giving an Mplus output file in HTML format.

## Value

A character string that is the filename where the output file was
written. Always a temp file in a temp directory.

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
MplusAutomation:::htmlout("https://statmodel.com/usersguide/chap3/ex3.1.html")
#> [1] "/tmp/RtmpYwymZb/UserGuide69be51c6af85.out"
```
