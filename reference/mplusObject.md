# Create an Mplus model object

This is a function to create an Mplus model object in `R`. The object
holds all the sections of an Mplus input file, plus some extra `R` ones.
Once created, the model can be run using other functions such as
`mplusModeler` or updated using methods defined for the `update`
function.

## Usage

``` r
mplusObject(
  TITLE = NULL,
  DATA = NULL,
  VARIABLE = NULL,
  DEFINE = NULL,
  MONTECARLO = NULL,
  MODELPOPULATION = NULL,
  MODELMISSING = NULL,
  ANALYSIS = NULL,
  MODEL = NULL,
  MODELINDIRECT = NULL,
  MODELCONSTRAINT = NULL,
  MODELTEST = NULL,
  MODELPRIORS = NULL,
  OUTPUT = NULL,
  SAVEDATA = NULL,
  PLOT = NULL,
  usevariables = NULL,
  rdata = NULL,
  autov = TRUE,
  imputed = FALSE,
  quiet = TRUE,
  ...
)
```

## Arguments

- TITLE:

  A character string of the title for Mplus.

- DATA:

  A charater string of the data section for Mplus (note, do not define
  the filename as this is generated automatically)

- VARIABLE:

  A character string of the variable section for Mplus (note, do not
  define the variable names from the dataset as this is generated
  automatically)

- DEFINE:

  A character string of the define section for Mplus (optional)

- MONTECARLO:

  A character string of the montecarlo section for Mplus (optional). If
  used, `autov` is defaults to `FALSE` instead of the usual default,
  `TRUE`, but may still be overwritten, if desired.

- MODELPOPULATION:

  A character string of the MODEL POPULATION section for Mplus
  (optional).

- MODELMISSING:

  A character string of the MODEL MISSING section for Mplus (optional).

- ANALYSIS:

  A character string of the analysis section for Mplus (optional)

- MODEL:

  A character string of the model section for Mplus (optional, although
  typically you want to define a model)

- MODELINDIRECT:

  A character string of the MODEL INDIRECT section for Mplus (optional).

- MODELCONSTRAINT:

  A character string of the MODEL CONSTRAINT section for Mplus
  (optional).

- MODELTEST:

  A character string of the MODEL TEST section for Mplus (optional).

- MODELPRIORS:

  A character string of the MODEL PRIORS section for Mplus (optional).

- OUTPUT:

  A character string of the output section for Mplus (optional)

- SAVEDATA:

  A character string of the savedata section for Mplus (optional)

- PLOT:

  A character string of the plot section for Mplus (optional)

- usevariables:

  A character vector of the variables from the `R` dataset to use in the
  model.

- rdata:

  An `R` dataset to be used for the model.

- autov:

  A logical (defaults to `TRUE`) argument indicating whether R should
  attempt to guess the correct variables to use from the R dataset, if
  `usevariables` is left `NULL`.

- imputed:

  A logical whether the data are multiply imputed (a list). Defaults to
  `FALSE`.

- quiet:

  optional. If `TRUE`, show status messages in the console.

- ...:

  Arguments passed on to
  [`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.md)
  if `run > 0`.

## Value

A list of class `mplusObject` with elements

- `TITLE`: The title in Mplus (if defined)

- `DATA`: The data section in Mplus (if defined)

- `VARIABLE`: The variable section in Mplus (if defined)

- `DEFINE`: The define section in Mplus (if defined)

- `MONTECARLO`: The montecarlo section in Mplus (if defined)

- `MODELPOPULATION`: The modelpopulation section in Mplus (if defined)

- `MODELMISSING`: The modelmissing section in Mplus (if defined)

- `ANALYSIS`: The analysis section in Mplus (if defined)

- `MODEL`: The model section in Mplus (if defined)

- `MODELINDIRECT`: The modelindirect section in Mplus (if defined)

- `MODELCONSTRAINT`: The modelconstraint section in Mplus (if defined)

- `MODELTEST`: The modeltest section in Mplus (if defined)

- `MODELPRIORS`: The modelpriors section in Mplus (if defined)

- `OUTPUT`: The output section in Mplus (if defined)

- `SAVEDATA`: The savedata section in Mplus (if defined)

- `PLOT`: The plot section in Mplus (if defined)

- `results`: NULL by default, but can be later updated to include the
  results from the model run.

- `usevariables`: A character vector of the variables from the `R` data
  set to be used.

- `rdata`: The `R` data set to use for the model.

- `imputed`: A logical whether the data are multiply imputed.

- `autov`: A logical whether the data should have the usevariables
  detected automatically or not

## Details

Mplus model objects allow a base model to be defined, and then flexibly
update the data, change the precise model, etc. If a section does not
vary between models, you can leave it the same. For example, suppose you
are fitting a number of models, but in all cases, wish to use maximum
likelihood estimator, “ANALYSIS: ESTIMATOR = ML;” and would like
standardized output, “OUTPUT: STDYX;”. Rather than retype those in every
model, they can be defined in one Mplus model object, and then that can
simply be updated with different models, leaving the analysis and output
sections untouched. This also means that if a reviewer comes back and
asks for all analyses to be re-run say using the robust maximum
likelihood estimator, all you have to do is change it in the model
object once, and re run all your code.

## See also

[`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.md)

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
example1 <- mplusObject(MODEL = "mpg ON wt;",
  usevariables = c("mpg", "hp"), rdata = mtcars)
str(example1)
#> List of 21
#>  $ TITLE          : NULL
#>  $ DATA           : NULL
#>  $ VARIABLE       : NULL
#>  $ DEFINE         : NULL
#>  $ MONTECARLO     : NULL
#>  $ MODELPOPULATION: NULL
#>  $ MODELMISSING   : NULL
#>  $ ANALYSIS       : NULL
#>  $ MODEL          : chr "mpg ON wt;"
#>  $ MODELINDIRECT  : NULL
#>  $ MODELCONSTRAINT: NULL
#>  $ MODELTEST      : NULL
#>  $ MODELPRIORS    : NULL
#>  $ OUTPUT         : NULL
#>  $ SAVEDATA       : NULL
#>  $ PLOT           : NULL
#>  $ results        : NULL
#>  $ usevariables   : chr [1:2] "mpg" "hp"
#>  $ rdata          :'data.frame': 32 obs. of  11 variables:
#>   ..$ mpg : num [1:32] 21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>   ..$ cyl : num [1:32] 6 6 4 6 8 6 8 4 4 6 ...
#>   ..$ disp: num [1:32] 160 160 108 258 360 ...
#>   ..$ hp  : num [1:32] 110 110 93 110 175 105 245 62 95 123 ...
#>   ..$ drat: num [1:32] 3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#>   ..$ wt  : num [1:32] 2.62 2.88 2.32 3.21 3.44 ...
#>   ..$ qsec: num [1:32] 16.5 17 18.6 19.4 17 ...
#>   ..$ vs  : num [1:32] 0 0 1 1 0 1 0 1 1 1 ...
#>   ..$ am  : num [1:32] 1 1 1 0 0 0 0 0 0 0 ...
#>   ..$ gear: num [1:32] 4 4 4 3 3 3 3 4 4 4 ...
#>   ..$ carb: num [1:32] 4 4 1 1 2 1 4 2 2 4 ...
#>  $ imputed        : logi FALSE
#>  $ quiet          : logi TRUE
#>  - attr(*, "class")= chr [1:2] "mplusObject" "list"
rm(example1)

# R figures out the variables automagically, with a message
example2 <- mplusObject(MODEL = "mpg ON wt;",
  rdata = mtcars, autov = TRUE)
str(example2)
#> List of 21
#>  $ TITLE          : NULL
#>  $ DATA           : NULL
#>  $ VARIABLE       : NULL
#>  $ DEFINE         : NULL
#>  $ MONTECARLO     : NULL
#>  $ MODELPOPULATION: NULL
#>  $ MODELMISSING   : NULL
#>  $ ANALYSIS       : NULL
#>  $ MODEL          : chr "mpg ON wt;"
#>  $ MODELINDIRECT  : NULL
#>  $ MODELCONSTRAINT: NULL
#>  $ MODELTEST      : NULL
#>  $ MODELPRIORS    : NULL
#>  $ OUTPUT         : NULL
#>  $ SAVEDATA       : NULL
#>  $ PLOT           : NULL
#>  $ results        : NULL
#>  $ usevariables   : chr [1:2] "mpg" "wt"
#>  $ rdata          :'data.frame': 32 obs. of  11 variables:
#>   ..$ mpg : num [1:32] 21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>   ..$ cyl : num [1:32] 6 6 4 6 8 6 8 4 4 6 ...
#>   ..$ disp: num [1:32] 160 160 108 258 360 ...
#>   ..$ hp  : num [1:32] 110 110 93 110 175 105 245 62 95 123 ...
#>   ..$ drat: num [1:32] 3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#>   ..$ wt  : num [1:32] 2.62 2.88 2.32 3.21 3.44 ...
#>   ..$ qsec: num [1:32] 16.5 17 18.6 19.4 17 ...
#>   ..$ vs  : num [1:32] 0 0 1 1 0 1 0 1 1 1 ...
#>   ..$ am  : num [1:32] 1 1 1 0 0 0 0 0 0 0 ...
#>   ..$ gear: num [1:32] 4 4 4 3 3 3 3 4 4 4 ...
#>   ..$ carb: num [1:32] 4 4 1 1 2 1 4 2 2 4 ...
#>  $ imputed        : logi FALSE
#>  $ quiet          : logi TRUE
#>  - attr(*, "class")= chr [1:2] "mplusObject" "list"
rm(example2)

# R can also try to figure out a list of variables when
# variable names are hyphenated first-last variable, all variables
# between the first and last one will be included
example3 <- mplusObject(MODEL = "mpg ON wt-vs;",
  rdata = mtcars, autov = TRUE)
str(example3)
#> List of 21
#>  $ TITLE          : NULL
#>  $ DATA           : NULL
#>  $ VARIABLE       : NULL
#>  $ DEFINE         : NULL
#>  $ MONTECARLO     : NULL
#>  $ MODELPOPULATION: NULL
#>  $ MODELMISSING   : NULL
#>  $ ANALYSIS       : NULL
#>  $ MODEL          : chr "mpg ON wt-vs;"
#>  $ MODELINDIRECT  : NULL
#>  $ MODELCONSTRAINT: NULL
#>  $ MODELTEST      : NULL
#>  $ MODELPRIORS    : NULL
#>  $ OUTPUT         : NULL
#>  $ SAVEDATA       : NULL
#>  $ PLOT           : NULL
#>  $ results        : NULL
#>  $ usevariables   : chr [1:4] "mpg" "wt" "qsec" "vs"
#>  $ rdata          :'data.frame': 32 obs. of  11 variables:
#>   ..$ mpg : num [1:32] 21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>   ..$ cyl : num [1:32] 6 6 4 6 8 6 8 4 4 6 ...
#>   ..$ disp: num [1:32] 160 160 108 258 360 ...
#>   ..$ hp  : num [1:32] 110 110 93 110 175 105 245 62 95 123 ...
#>   ..$ drat: num [1:32] 3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#>   ..$ wt  : num [1:32] 2.62 2.88 2.32 3.21 3.44 ...
#>   ..$ qsec: num [1:32] 16.5 17 18.6 19.4 17 ...
#>   ..$ vs  : num [1:32] 0 0 1 1 0 1 0 1 1 1 ...
#>   ..$ am  : num [1:32] 1 1 1 0 0 0 0 0 0 0 ...
#>   ..$ gear: num [1:32] 4 4 4 3 3 3 3 4 4 4 ...
#>   ..$ carb: num [1:32] 4 4 1 1 2 1 4 2 2 4 ...
#>  $ imputed        : logi FALSE
#>  $ quiet          : logi TRUE
#>  - attr(*, "class")= chr [1:2] "mplusObject" "list"
rm(example3)

# R warns if the first 8 characters of a (used) variable name are not unique
# as they will be indistinguishable in the Mplus output
example4 <- mplusObject(MODEL = "basename_01 ON basename_02;",
  rdata = data.frame(basename_01 = 1:5, basename_02 = 5:1),
  autov = TRUE)
#> The following variables are not unique in the first 8 characters:
#>  basename_02
rm(example4)
```
