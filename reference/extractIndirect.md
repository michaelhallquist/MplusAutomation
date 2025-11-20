# Extract Indirect Effects output

This function parses both unstandardized and standardized indirect
effects It returns a list composed of \$unstandardized and
\$standardized. The base structure of each is a list containing
\$overall and \$specific effects (as data.frames)

## Usage

``` r
extractIndirect(outfiletext, curfile)
```

## Arguments

- outfiletext:

  a character vector containing the indirect effects output section
  returned by getSection

- curfile:

  the name of the current output file being parsed

## Value

An mplus.indirect object (inherits list) containing \$overall and
\$specific
