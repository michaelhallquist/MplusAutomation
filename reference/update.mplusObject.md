# Update an Mplus model object

This is a method for updating an Mplus model object. It takes an Mplus
model object as the first argument, and then optionally any sections to
update. There are two ways to update a section using a formula
interface. `~ "new stuff"` will replace a given section with the new
text. Alternately, you can add additional text using
`~ + "additional stuff"`. Combined these let you replace or add to a
section.

## Usage

``` r
# S3 method for class 'mplusObject'
update(object, quiet = TRUE, ...)
```

## Arguments

- object:

  An object of class mplusObject

- quiet:

  optional. If `TRUE`, show status messages in the console.

- ...:

  Additional arguments to pass on

## Value

An (updated) Mplus model object

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
example1 <- mplusObject(MODEL = "mpg ON wt;",
  usevariables = c("mpg", "hp"), rdata = mtcars)
x <- ~ "ESTIMATOR = ML;"
str(update(example1, rdata = iris))
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
#>  $ rdata          :'data.frame': 150 obs. of  5 variables:
#>   ..$ Sepal.Length: num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#>   ..$ Sepal.Width : num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#>   ..$ Petal.Length: num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#>   ..$ Petal.Width : num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#>   ..$ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ imputed        : logi FALSE
#>  $ quiet          : logi TRUE
#>  - attr(*, "class")= chr [1:2] "mplusObject" "list"
str(update(example1, ANALYSIS = x))
#> List of 21
#>  $ TITLE          : NULL
#>  $ DATA           : NULL
#>  $ VARIABLE       : NULL
#>  $ DEFINE         : NULL
#>  $ MONTECARLO     : NULL
#>  $ MODELPOPULATION: NULL
#>  $ MODELMISSING   : NULL
#>  $ ANALYSIS       : chr " ESTIMATOR = ML;"
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
str(update(example1, MODEL = ~ "wt ON hp;"))
#> List of 21
#>  $ TITLE          : NULL
#>  $ DATA           : NULL
#>  $ VARIABLE       : NULL
#>  $ DEFINE         : NULL
#>  $ MONTECARLO     : NULL
#>  $ MODELPOPULATION: NULL
#>  $ MODELMISSING   : NULL
#>  $ ANALYSIS       : NULL
#>  $ MODEL          : chr " wt ON hp;"
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
str(update(example1, MODEL = ~ . + "wt ON hp;"))
#> List of 21
#>  $ TITLE          : NULL
#>  $ DATA           : NULL
#>  $ VARIABLE       : NULL
#>  $ DEFINE         : NULL
#>  $ MONTECARLO     : NULL
#>  $ MODELPOPULATION: NULL
#>  $ MODELMISSING   : NULL
#>  $ ANALYSIS       : NULL
#>  $ MODEL          : chr "mpg ON wt;\n wt ON hp;"
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
str(update(example1, ANALYSIS = x, MODEL = ~ . + "wt ON hp;"))
#> List of 21
#>  $ TITLE          : NULL
#>  $ DATA           : NULL
#>  $ VARIABLE       : NULL
#>  $ DEFINE         : NULL
#>  $ MONTECARLO     : NULL
#>  $ MODELPOPULATION: NULL
#>  $ MODELMISSING   : NULL
#>  $ ANALYSIS       : chr " ESTIMATOR = ML;"
#>  $ MODEL          : chr "mpg ON wt;\n wt ON hp;"
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

## check that use variables can be updated & overridden
str(update(example1, usevariables = c("mpg", "hp", "cyl")))
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
#>  $ usevariables   : chr [1:3] "mpg" "hp" "cyl"
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

# test to make sure . in Mplus code does not cause problems
str(update(example1, ANALYSIS = x, MODEL = ~ . + "wt ON hp*.5;"))
#> List of 21
#>  $ TITLE          : NULL
#>  $ DATA           : NULL
#>  $ VARIABLE       : NULL
#>  $ DEFINE         : NULL
#>  $ MONTECARLO     : NULL
#>  $ MODELPOPULATION: NULL
#>  $ MODELMISSING   : NULL
#>  $ ANALYSIS       : chr " ESTIMATOR = ML;"
#>  $ MODEL          : chr "mpg ON wt;\n wt ON hp*.5;"
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
rm(example1, x)
```
