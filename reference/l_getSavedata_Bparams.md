# Internal function to load the draws from the Bayesian model posterior distribution

To do: add details.

## Usage

``` r
l_getSavedata_Bparams(outfile, outfiletext, fileInfo, discardBurnin = TRUE)
```

## Arguments

- outfile:

  The Mplus output file to be used.

- outfiletext:

  The contents of the Mplus output file

- fileInfo:

  The file info

- discardBurnin:

  Logical value whether to discard the burnin iterations or not.
  Defaults to `TRUE`.

## Value

A list of class `mcmc` and `mplus.bparameters`
