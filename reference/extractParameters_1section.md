# Extract Parameters for One Section

To do: add details

## Usage

``` r
extractParameters_1section(filename, modelSection, sectionName)
```

## Arguments

- filename:

  name of Mplus output file being processed

- modelSection:

  name of model output section being processed

- sectionName:

  name of output type to search for (e.g., "model_results")

## Value

A list of parameters

## Examples

``` r
if (FALSE) { # \dontrun{
  #a few examples of files to parse
  #mg + lc. Results in latent class pattern, not really different from
  #         regular latent class matching. See Example 7.21
  #mg + twolevel. Group is top, bw/wi is 2nd. See Example 9.11
  #lc + twolevel. Bw/wi is top, lc is 2nd. See Example 10.1.
  #               But categorical latent variables is even higher
  #test cases for more complex output: 7.21, 9.7, 9.11, 10.1
} # }
```
