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

this normalizes overflow in clock components (for example, 90 seconds
becomes 1 minute and 30 seconds), converts to an hms format, and retains
a dhms format when days are present. Supported date formats match slurm
sbatch: https://slurm.schedmd.com/sbatch.html
