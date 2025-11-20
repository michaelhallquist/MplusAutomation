# Parses tags in the body section

Parses tags in the body section (character vector) and init collection
(list of vars defined in the init section). This is an internal
function.

## Usage

``` r
parseTags(bodySection, initCollection)
```

## Arguments

- bodySection:

  The body

- initCollection:

  The initial collection

## Value

A list with three elements, where each list represents the location,
start character, end character, tag type, etc. of each tag.

- `initTags`: initMatches

- `bodyTags`: bodyMatches

- `bodyText`: bodySection
