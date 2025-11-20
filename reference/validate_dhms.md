# helper function to validate format of walltime inputs for HPC submission

helper function to validate format of walltime inputs for HPC submission

## Usage

``` r
validate_dhms(str)
```

## Arguments

- str:

  string containing a duration that may include a days specification

## Details

this always converts to an hms format, and if days are present, it
converts to dhms. Supported date formats match slurm sbatch:
https://slurm.schedmd.com/sbatch.html
