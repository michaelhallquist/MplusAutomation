# Subset a list of Mplus model results

a helper function to be used by wrappers that generate HTML, LaTex, and
on-screen displays of summary statistics

## Usage

``` r
subsetModelList(modelList, keepCols, dropCols, sortBy = NULL)
```

## Arguments

- modelList:

  A list object of Mplus models

- keepCols:

  Columns to keep

- dropCols:

  Columns to drop (use only one of keep/dropCols)

- sortBy:

  How to sort. Defaults to `NULL`, which does not sort the list.

## Value

Extracted and sorted data

## Examples

``` r
# make me!!!
```
