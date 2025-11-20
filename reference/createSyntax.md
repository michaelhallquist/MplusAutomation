# Create the Mplus input text for an mplusObject

This function takes an object of class `mplusObject` and creates the
Mplus input text corresponding to it, including data link and variable
names.

## Usage

``` r
createSyntax(object, filename, check = TRUE, add = FALSE, imputed = FALSE)
```

## Arguments

- object:

  An object of class mplusObject

- filename:

  The name of the data file as a character vector

- check:

  A logical indicating whether or not to run `parseMplus` on the created
  input file. Checks for errors like lines that are too long, or for
  missing semi-colons and gives notes.

- add:

  A logical passed on to `parseMplus` whether to add semi colons to line
  ends. Defaults to `FALSE`.

- imputed:

  A logical whether the data are multiply imputed. Defaults to `FALSE`.

## Value

A character string containing all the text for the Mplus input file.

## See also

[`prepareMplusData`](https://michaelhallquist.github.io/MplusAutomation/reference/prepareMplusData.md),
[`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.md)

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
# example mplusObject
example1 <- mplusObject(MODEL = "mpg ON wt;",
  usevariables = c("mpg", "hp"), rdata = mtcars)

# create the Mplus input text
cat(createSyntax(example1, "example1.dat"), file=stdout(), fill=TRUE)
#> All ok
#> DATA:
#> FILE = "example1.dat"; 
#> VARIABLE:
#> NAMES = mpg hp; 
#>  MISSING=.;
#>  
#> MODEL:
#> mpg ON wt;

# update the object, then create input text
cat(createSyntax(update(example1,
  TITLE = ~ "This is my title;",
  MODEL = ~ . + "\nmpg ON hp;",
  usevariables = c("mpg", "hp", "wt")), "example1.dat"),
  file=stdout(),
  fill=TRUE)
#> All ok
#> TITLE:
#>  This is my title;
#> DATA:
#> FILE = "example1.dat"; 
#> VARIABLE:
#> NAMES = mpg hp wt; 
#>  MISSING=.;
#>  
#> MODEL:
#> mpg ON wt;
#>  
#> mpg ON hp;
rm(example1)
```
