# Updates current values

Body tags currentValues are substituted at the bottom-most level after
init collection is finalized (recursively process any nested tags)

## Usage

``` r
updateCurrentValues(templateTags, initCollection)
```

## Arguments

- templateTags:

  The template tags

- initCollection:

  Initial collection

## Value

Updated current value or the original if no match.
