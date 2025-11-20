# Convert a matrix or data frame to numeric or integer for Mplus

Primarily an internal utility function, for `prepareMplusData`.

## Usage

``` r
.convertData(df, dummyCode = NULL)
```

## Arguments

- df:

  A matrix or data frame

## Value

An error if it cannot be converted or a matrix or data frame with all
variables converted to numeric or integer classes

## Examples

``` r
if (FALSE) { # \dontrun{
df1 <- df2 <- df3 <- df4 <- mtcars

df2$cyl <- factor(df2$cyl)
df2$am <- as.logical(df2$am)

df3$mpg <- as.character(df3$mpg)

df4$vs <- as.Date(df4$vs, origin = "1989-01-01")

df5 <- as.matrix(cars)

df6 <- matrix(c(TRUE, TRUE, FALSE, FALSE), ncol = 2)

df7 <- as.list(mtcars)


MplusAutomation:::.convertData(df1)

MplusAutomation:::.convertData(df2)

MplusAutomation:::.convertData(df3)

MplusAutomation:::.convertData(df4)

MplusAutomation:::.convertData(df5)

MplusAutomation:::.convertData(df6)

MplusAutomation:::.convertData(df7)

rm(df1, df2, df3, df4, df5, df6, df7)
} # }
```
