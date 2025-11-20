# Check Mplus code for missing semicolons or too long lines.

The function parses a character string containing Mplus code and checks
that every non blank line ends in either a colon or a semicolon. In
addition, it checks that every line is less than 90 characters, because
Mplus ignores everything after 90 characters on a line which can be a
source of enigmatic errors.

## Usage

``` r
parseMplus(x, add = FALSE)
```

## Arguments

- x:

  a character string containing Mplus code.

- add:

  logical indicating whether or not to add semicolons to lines that do
  not have them. Defaults to `FALSE`.

## Value

a character vector containing the input text and optionally added
semicolons.

## Details

The function is fairly basic at the moment. It works by simply removing
blank space (spaces, tabs, etc.) and then if a line does not terminate
in a colon or semicolon, it returns a note and the line number.
Optionally, it can add semicolons to any lines missing them and return
the input with added semicolons. To check for lines that are too long,
all trailing (but not before) white space is removed, and then the
number of characters is checked.

## See also

[`mplusModeler`](https://michaelhallquist.github.io/MplusAutomation/reference/mplusModeler.md)

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
# sample input
test <- "
MODEL:
  mpg ON wt hp;
  wt WITH hp
"
# check and return
cat(parseMplus(test), file=stdout(), fill=TRUE)
#> The following lines are not empty and do not end in a : or ;.
#> 4:   wt WITH hp
#> Rerun with parseMplus(add = TRUE) to add semicolons to all lines
#> 
#> MODEL:
#>   mpg ON wt hp;
#>   wt WITH hp
#> 
# add missing semicolons and return
cat(parseMplus(test, TRUE), file=stdout(), fill=TRUE)
#> The following lines are not empty and do not end in a : or ;.
#> 4:   wt WITH hp
#> added semicolons ';' to all of the above lines
#> 
#> MODEL:
#>   mpg ON wt hp;
#>   wt WITH hp;
#> 
# line that is too long for Mplus
test <- "
MODEL:
  mpg cyl disp hp drat wt qsec vs am gear PWITH cyl disp hp drat wt qsec vs am gear carb;
"
cat(parseMplus(test), file=stdout())
#> All ok
#> 
#> MODEL:
#>   mpg cyl disp hp drat wt qsec vs am gear PWITH cyl disp hp drat wt qsec vs am gear carb;
```
