# Read Parameters, Summary Statistics, and Savedata from Mplus Output

Extracts information from one or more Mplus output files, including fit
statistics and parameters. Its is to parse all (supported) aspects of
Mplus output and to combine these into a list object, with one element
per output file identified.

## Usage

``` r
readModels(
  target = getwd(),
  recursive = FALSE,
  filefilter,
  pathfilter,
  what = "all",
  quiet = TRUE
)
```

## Arguments

- target:

  the directory containing Mplus output files (.out) to parse OR the
  single output file to be parsed. May be a full path, relative path, or
  a filename within the working directory. Defaults to the current
  working directory. Example: "C:/Users/Michael/Mplus Runs"

- recursive:

  optional. If `TRUE`, parse all models nested in subdirectories within
  `target`. Defaults to `FALSE`.

- filefilter:

  a Perl regular expression (PCRE-compatible) specifying particular
  output files to be parsed within `directory` based on their file name.
  See `regex` or <https://www.pcre.org/pcre.txt> for details about
  regular expression syntax.

- pathfilter:

  a Perl regular expression (PCRE-compatible) specifying particular
  paths to be parsed within `directory` based on their absolute paths.

- what:

  a character vector denoting what aspects of Mplus output to extract.
  Defaults to `"all"`, which will extract all supported output sections.
  See details for additional information.

- quiet:

  whether to suppress printing to the screen the file currently being
  processed. Defaults to TRUE.

## Value

A list with one mplus.model per file. Each mplus.model object is
composed of elements containing major output sections, as detailed
below. If `target` is a single file, then the top-level elements will be
a single mplus.model object, not a list of files. Specific elements are:

- `input`: Mplus input syntax parsed into a list by major section

- `warnings`: Syntax and estimation warnings as a list

- `errors`: Syntax and estimation errors as a list

- `data_summary`: Output of SUMMARY OF DATA section, including cluster
  sizes and ICCs

- `sampstat`: Sample statistics provided by OUTPUT: SAMPSTAT, if
  specified

- `covariance_coverage`: Covariance coverage matrix for checking
  missingness patterns

- `summaries`: Summary statistics from `extractModelSummaries`, having
  structure as specified by that function

- `parameters`: Model parameters from `extractModelParameters`, having
  structure as specified by that function

- `class_counts`: Latent class counts and proportions for models that
  include a categorical latent variable

- `indirect`: Output of MODEL INDIRECT if available in output. Contains
  `$overall` and `$specific` data.frames for each indirect effect
  section

- `mod_indices`: Model modification indices from `extractModIndices`,
  having structure as specified by that function

- `residuals`: a list containing relevant information from OUTPUT:
  RESIDUALS

- `savedata_info`: File information about SAVEDATA files related to this
  output

- `savedata`: SAVEDATA file as an R `data.frame`, as described in
  `getSavedata_Data`

- `bparameters`: an `mcmc.list` object containing the draws from the
  MCMC chains for a Bayesian model that uses the SAVEDATA: BPARAMETERS
  command

- `tech1`: a list containing parameter specification and starting values
  from OUTPUT: TECH1

- `tech3`: a list containing parameter covariance and correlation
  matrices from OUTPUT: TECH3

- `tech4`: a list containing means, covariances, and correlations for
  latent variables from OUTPUT: TECH4

- `tech7`: a list containing sample statistics for each latent class
  from OUTPUT: TECH7

- `tech8`: a list containing optimization history of the model.
  Currently only supports potential scale reduction in BAYES. OUTPUT:
  TECH8

- `tech9`: a list containing warnings/errors from replication runs for
  MONTECARLO analyses from OUTPUT: TECH9

- `tech10`: a list containing model fit information from OUTPUT: TECH10

- `tech12`: a list containing observed versus estimated sample
  statistics for TYPE=MIXTURE analyses from OUTPUT: TECH12

- `fac_score_stats`: factor score mean, correlation, and covariance
  structure from SAMPLE STATISTICS FOR ESTIMATED FACTOR SCORES section

- `lcCondMeans`: conditional latent class means and pairwise
  comparisons, obtained using auxiliary(e) syntax in latent class models

- `r3step`: predictors of latent class membership using the 3-step
  procedure (R3STEP)

- `gh5`: a list containing data from the gh5 (graphics) file
  corresponding to this output. (Requires rhdf5 package)

- `h5results`: a list containing data from h5results file produced by
  Mplus v8.11+. (Requires rhdf5 package)

- `output`: The entire, raw output file.

## Details

The `what` parameter defaults to "all", which extracts all supported
output. If you would like to extract a reduced set of output sections
(especially to speed up the function when reading many files), specify
the sections as a character vector from the following options:

c("input", "warn_err", "data_summary", "sampstat",
"covariance_coverage", "summaries", "parameters", "class_counts",
"indirect", "mod_indices", "residuals", "savedata", "bparameters",
"tech1", "tech3", "tech4", "tech7", "tech8", "tech9", "tech10",
"tech12", "fac_score_stats", "lcCondMeans", "gh5", "output")

## Author

Michael Hallquist

## Examples

``` r
if (FALSE) { # \dontrun{
  allOutput <- readModels(
    "C:/Program Files/Mplus/Mplus Examples/User's Guide Examples", recursive=TRUE)
} # }
```
