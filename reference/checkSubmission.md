# check on the status of submitted Mplus jobs on the cluster

check on the status of submitted Mplus jobs on the cluster

## Usage

``` r
checkSubmission(mplus_submission_df = NULL, quiet = FALSE)
```

## Arguments

- mplus_submission_df:

  The data.frame returned by `submitModels` containing jobs to check on

- quiet:

  If `TRUE`, do not print out the submission data.frame with current
  status

## Value

invisibly, the `mplus_submission_df` with `$status` amd `$status_time`
updated
