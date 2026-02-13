# Extract observed variable names from Mplus syntax

Internal utility used by `mplusModel` and `detectVariables`.

## Usage

``` r
extractMplusVariables(
  parsed_syntax = NULL,
  variable = NULL,
  define = NULL,
  model = NULL,
  model_sections = NULL,
  data_names = NULL
)
```

## Arguments

- parsed_syntax:

  Parsed Mplus syntax list (typically from `parseMplusSyntax`).

- variable:

  Mplus VARIABLE section (character string or parsed list).

- define:

  Mplus DEFINE section.

- model:

  Mplus MODEL section.

- model_sections:

  Optional list of MODEL\* sections to scan.

- data_names:

  Optional character vector of data.frame column names used to filter
  detected identifiers down to observed variables.

## Value

Character vector of detected observed variables.
