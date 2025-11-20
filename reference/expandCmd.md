# Expand Mplus-style hyphenated variable ranges

Expands Mplus shorthand expressions that specify sequences of variables
using hyphenated ranges (e.g., `y1-y3`) into the full list of variables
(e.g., `y1 y2 y3`). This function also propagates suffixes from the
right-hand token (e.g., `@c`, `*c`, or bare `*`) to every expanded
variable (e.g., `y1-y3@1` -\> `y1@1 y2@1 y3@1`).

## Usage

``` r
expandCmd(cmd, expand_numeric = FALSE)
```

## Arguments

- cmd:

  A single character string containing Mplus syntax to expand.

- expand_numeric:

  Logical. If `TRUE`, expand pure numeric ranges (e.g., `1-3` -\>
  `1 2 3`) in list-like contexts. Default: `FALSE`.

## Value

A character string with hyphenated ranges expanded to explicit variable
lists.

## Details

By default, the function does **not** expand pure numeric ranges (e.g.,
`1-3`) to avoid confusion with arithmetic subtraction. If
`expand_numeric = TRUE`, such ranges will be expanded when they appear
in list-like contexts (whitespace/comma/semicolon/parentheses
boundaries) and the line does not contain an equals sign (to avoid
accidental expansion of arithmetic like `d = 1 - 3`).

Hyphens in `MODEL CONSTRAINT` expressions such as `a = b1-b2` are
explicitly protected and left untouched.

## Examples

``` r
if (FALSE) { # \dontrun{
expandCmd("y1-y3 y5-y6")
# "y1 y2 y3 y5 y6"

expandCmd("BY y1-y3@0.5;")
# "BY y1@0.5 y2@0.5 y3@0.5;"

expandCmd("z10-z12*2")
# "z10*2 z11*2 z12*2"

expandCmd("MODEL CONSTRAINT: a = b1-b2;")
# "MODEL CONSTRAINT: a = b1-b2;" (unchanged)

expandCmd("1 - 3", expand_numeric = TRUE)
# "1 2 3"

expandCmd("d = 1 - 3;", expand_numeric = TRUE)
# "d = 1 - 3;" (unchanged because of '=')
} # }
```
