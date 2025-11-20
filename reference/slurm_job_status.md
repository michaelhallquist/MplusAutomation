# check the status of jobs submitted on a SLURM cluster

check the status of jobs submitted on a SLURM cluster

## Usage

``` r
slurm_job_status(
  job_ids = NULL,
  user = NULL,
  sacct_format = "jobid,submit,timelimit,start,end,state"
)
```

## Arguments

- job_ids:

  a vector of job ids to check

- user:

  the user who submitted the jobs

- sacct_format:

  the format string passed to sacct for job status

## Value

a data.frame containing the status of each job id

## Details

This function calls `sacct -j` to check the status of jobs on a slurm
cluster
