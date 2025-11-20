# Extract Indirect Effects output

This function parses a given indirect section It returns a list composed
of \$overall and \$specific effects

## Usage

``` r
extractIndirect_section(indirectSection, curfile, sectionType)
```

## Arguments

- indirectSection:

  a character vector containing the indirect effects for a specific
  section (e.g., stdyx)

- curfile:

  the name of the current output file being parsed

## Value

An mplus.indirect object (inherits list) containing \$overall and
\$specific
