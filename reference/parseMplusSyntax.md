# Convert an Mplus syntax string into a parsed list

Convert an Mplus syntax string into a parsed list

## Usage

``` r
parseMplusSyntax(syntax, dropSectionNames = TRUE)
```

## Arguments

- syntax:

  the character vector containing Mplus syntax

- dropSectionNames:

  Logical. If TRUE, section names will be dropped from the parsed list

## Value

a parsed list of Mplus syntax

## Details

This function converts an Mplus syntax string into a list parsed into
sections such as TITLE and DATA
