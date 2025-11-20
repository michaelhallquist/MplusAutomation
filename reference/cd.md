# Change directory

The function takes a path and changes the current working directory to
the path. If the directory specified in the path does not currently
exist, it will be created.

## Usage

``` r
cd(base, pre, num)
```

## Arguments

- base:

  a character string with the base path to the directory. This is
  required.

- pre:

  an optional character string with the prefix to add to the base path.
  Non character strings will be coerced to character class.

- num:

  an optional character string, prefixed by `pre`. Non character strings
  will be coerced to character class.

## Value

NULL, changes the current working directory

## Details

The function has been designed to be platform independent, although it
has had limited testing. Path creation is done using `file.path`, the
existence of the directory is checked using `file.exists` and the
directory created with `dir.create`. Only the first argument, is
required. The other optional arguments are handy when one wants to
create many similar directories with a common base.

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
# an example just using the base
cd("~/testdir")

# an example using the optional arguments
base <- "~/testdir"
pre <- "test_"

cd(base, pre, 1)
cd(base, pre, 2)
} # }
```
