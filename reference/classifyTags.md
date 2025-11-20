# Classifies Tags

Accepts a vector of tags to be classified as well as the iterators. Tags
are classified as ‘iterator’, ‘array’, ‘conditional’, or ‘simple’. This
is an internal function.

## Usage

``` r
classifyTags(tagVector, iteratorsVector)
```

## Arguments

- tagVector:

  A vector of tags to be classified

- iteratorsVector:

  a vector of the iterators to correctly classify tags

## Value

A character vector the same length as the vectors to be tagged
