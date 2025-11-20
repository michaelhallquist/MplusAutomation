# Simple tag lookup

The purpose of this function is to set the currentValue column for the
bodyTags and initTags data.frames for simple tags only. Most values will
be replaced at the bottom level of recursion, but simple tags do not
change over iterations, so can be set one time.

## Usage

``` r
lookupSimpleTags(templateTags, initCollection)
```

## Arguments

- templateTags:

  The template tags

- initCollection:

  The initial collection

## Value

A tag.
