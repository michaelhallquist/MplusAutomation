# This function checks the status of one or more compute jobs

This function checks the status of one or more compute jobs

## Usage

``` r
get_job_status(job_ids, scheduler = "slurm", quiet = TRUE)
```

## Arguments

- job_ids:

  One or more job ids of existing PBS or slurm jobs, or process ids of a
  local process for `scheduler="sh"`.

- scheduler:

  What scheduler is used for job execution. Options: c("torque", "qsub",
  "slurm", "sbatch", "sh", "local")

- quiet:

  If `TRUE`, `wait_for_job` will not print out any status updates on
  jobs. If `FALSE`, the function prints out status updates for each
  tracked job so that the user knows what's holding up progress.

## Value

A vector of job statuses corresponding to each job id

## Details

Note that for the `scheduler` argument, "torque" and "qsub" are the
same; "slurm" and "sbatch" are the same, and "sh" and "local" are the
same.

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
# example on qsub/torque cluster
get_job_status("7968857.torque01.util.production.int.aci.ics.psu.edu", scheduler = "torque")

# example of checking two jobs on slurm cluster
get_job_status(c("24147864", "24147876"), scheduler = "slurm")

# example of checking two jobs on local machine
get_job_status(c("9843", "9844"), scheduler = "local")
} # }
```
