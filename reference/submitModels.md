# Submit Mplus models to a high-performance cluster scheduler

This function submits a group of Mplus models (.inp files) located
within a single directory or nested within subdirectories.

## Usage

``` r
submitModels(
  target = getwd(),
  recursive = FALSE,
  filefilter = NULL,
  replaceOutfile = "modifiedDate",
  Mplus_command = NULL,
  quiet = FALSE,
  scheduler = "slurm",
  sched_args = NULL,
  env_variables = NULL,
  export_all = FALSE,
  cores_per_model = 1L,
  memgb_per_model = 8L,
  time_per_model = "1:00:00",
  pre = NULL,
  post = NULL,
  batch_outdir = NULL,
  job_script_prefix = NULL,
  combine_jobs = TRUE,
  max_time_per_job = "24:00:00",
  combine_memgb_tolerance = 1,
  combine_cores_tolerance = 2,
  debug = FALSE,
  fail_on_error = TRUE
)
```

## Arguments

- target:

  a character vector where each element is a directory containing Mplus
  input files (.inp) to run OR a single .inp file to be run. Elements
  may be a full path, relative path, or a filename within the working
  directory. Defaults to the current working directory. Example:
  “C:/Users/Michael/Mplus Runs”

- recursive:

  optional. If `TRUE`, run all models nested in subdirectories within
  `directory`. Defaults to `FALSE`. Not relevant if `target` is a single
  file.

- filefilter:

  a Perl regular expression (PCRE-compatible) specifying particular
  input files to be run among those found in `target`. See `regex` or
  <https://www.pcre.org/pcre.txt> for details about regular expression
  syntax.

- replaceOutfile:

  optional. Currently supports three settings: “always”, which runs all
  models, regardless of whether an output file for the model exists;
  “never”, which does not run any model that has an existing output
  file; and “modifiedDate”, which only runs a model if the modified date
  for the input file is more recent than the output file modified date
  (implying there have been updates to the model).

- Mplus_command:

  optional. N.B.: No need to pass this parameter for most users (has
  intelligent defaults). Allows the user to specify the name/path of the
  Mplus executable to be used for running models. This covers situations
  where Mplus is not in the system's path, or where one wants to test
  different versions of the Mplus program.

- quiet:

  optional. If `FALSE`, show status messages in the console.

- scheduler:

  Which scheduler to use for job submission. Options are 'qsub',
  'torque', 'sbatch', 'slurm', 'local', or 'sh'. The terms `'qsub'` and
  `'torque'` are aliases (where 'torque' submits via the qsub command).
  Likewise for 'sbatch' and 'slurm'. If `'local'` or `'sh'` are
  specified, `submitModels` does not submit to any scheduler at all, but
  instead executes the command locally via a shell script.

- sched_args:

  A character vector of arguments to be included in the scheduling
  command. On TORQUE, these will typically begin with '-l' such as '-l
  wall_time=10:00:00'. These are added inside the submission script for
  each model and are shared across all models. To add model-specific
  arguments, include `! #SBATCH` or `! #PBS` lines inside the individual
  .inp files

- env_variables:

  A named character vector containing environment variables and their
  values to be passed to the `script` at execution time. This is handled
  by the `-v` directive on TORQUE clusters and by `--export` on Slurm
  clusters. The names of this vector are the environment variable names
  and the values of the vector are the environment variable values to be
  passed in. If you want to propagate the current value of an
  environment variable to the compute node at runtime, use NA as the
  value of the element in `env_variables`. See examples.

- export_all:

  Whether to export all environment variables to the compute node at
  runtime. Default: FALSE

- cores_per_model:

  How many cpus/cores are requested for each model (can be overriden
  using `! BATCH` directives in .inp files). Default: 1.

- memgb_per_model:

  amount of memory (RAM) requested for each model (in GB). Default: 8.

- time_per_model:

  amount of time requested for each model. Default: "1:00:00" (1 hour).
  If a number is provided, we will treat this as the number of minutes.

- pre:

  user-specified shell commands to include in the job script prior to
  running Mplus (e.g., module load commands)

- post:

  user-specified shell commands to include in the job script after Mplus
  runs (e.g., execute results wrangling script)

- batch_outdir:

  the directory where job scripts should be written

- job_script_prefix:

  the filename prefix for each job script

- combine_jobs:

  if TRUE, `submitModels` will seek to combine similar models into
  batches to reduce the total number of jobs

- max_time_per_job:

  The maximum time (in days-hours:minutes:seconds format) allowed for a
  combined job

- combine_memgb_tolerance:

  The memory tolerance for combining similar models in GB. Defaults to 1
  (i.e., models that differ by \<= 1 GB can be combined)

- combine_cores_tolerance:

  The cores tolerance for combining models with similar core requests.
  Defaults to 2 (i.e., models whose core requests differ by \<= 2 can be
  combined)

- debug:

  a logical indicating whether to actually submit the jobs (TRUE) or
  just create the scripts for inspection (FALSE)

- fail_on_error:

  Whether to stop execution of the script (TRUE), or issue a warning
  (FALSE) if the job submission fails. Defaults to TRUE.

## Value

A data.frame recording details of the jobs submitted by `submitModels`.
This can be passed to the `summary` function or to `checkSubmission` to
see the state of submitted jobs.

## Details

Note that if `fail_on_error` is `TRUE` and submission of one model
fails, the submission loop will stop, rather than submitting further
models.

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
  submitModels("~/Michael/submitTest", recursive=TRUE, sched_args=c("--mail=user", "--export=v"), 
    max_time_per_job = "2:10:00", combine_jobs = TRUE)
} # }
```
