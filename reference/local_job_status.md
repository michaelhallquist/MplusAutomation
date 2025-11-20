# helper function to get job status on local \*nix-based machine

helper function to get job status on local \*nix-based machine

## Usage

``` r
local_job_status(
  job_ids = NULL,
  user = NULL,
  ps_format = "user,pid,state,time,etime,%cpu,%mem,comm,xstat"
)
```

## Arguments

- job_ids:

  a vector of job ids (process IDs) to check

- user:

  the user who owns the processes (defaults to current user)

- ps_format:

  the format string passed to ps

## Value

a data.table with job information for each job id
