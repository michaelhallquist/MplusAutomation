# Friendly Regular Expression

Creates data frame documenting the start and end of all tags.

## Usage

``` r
friendlyGregexpr(pattern, charvector, perl = TRUE, include_tag = TRUE)
```

## Arguments

- pattern:

  The pattern to search for

- charvector:

  Character vector

- perl:

  A logical whether or not to use perl based regular expressions.
  Defaults to `TRUE`.

- include_tag:

  if TRUE, return the match as a character string. This is the default,
  but setting to `FALSE` is a bit faster

## Value

A `data.frame`

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
  friendlyGregexpr("(BY|WITH|ON)", 
    c("POS_WI BY", "X WITH Y WITH Z")
  )
} # } 
```
