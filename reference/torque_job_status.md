# helper function to get the status of jobs on a TORQUE cluster

helper function to get the status of jobs on a TORQUE cluster

## Usage

``` r
torque_job_status(job_ids, user = NULL)
```

## Arguments

- job_ids:

  a vector of job ids to check

- user:

  the user who submitted the jobs

## Value

a data.frame with the job status for each id

## Details

Torque does not keep information about completed jobs available in qstat
or qselect. Thus, we need to log when a job is listed as queued, so that
it 'going missing' is evidence of it being completed.
