# Detect the location/name of the Mplus command

This is an utility function to help detect the correct name/path to
Mplus. It tries hard across operating systems to find Mplus and if it
cannot find the full version of Mplus to find a demo version of Mplus.

## Usage

``` r
detectMplus()
```

## Value

A character string that is the Mplus command possibly the path to the
mplus command or an error if it cannot be found.

## Details

It does not require any arguments.

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
## if you have Mplus installed, uncomment and run
## this will give an error if it cannot find Mplus.
## detectMplus()
```
