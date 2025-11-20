# Create Mplus code for various residual covariance structures.

This function makes it easy to write the Mplus syntax for various
residual covariance structure.

## Usage

``` r
mplusRcov(
  x,
  type = c("homogenous", "heterogenous", "cs", "toeplitz", "ar", "un"),
  r = "rho",
  e = "e",
  collapse = FALSE
)
```

## Arguments

- x:

  input character vector of variable names, ordered by time

- type:

  A character string indicating the type of residual covariance
  structure to be used. Defaults to ‘homogenous’. Current options
  include ‘homogenous’, ‘heterogenous’, ‘cs’ for compound symmetric,
  ‘toeplitz’ for banded toeplitz, ‘ar’ for autoregressive, and ‘un’ for
  unstructured.

- r:

  a character vector of the base label to name covariance parameters.
  Defaults to ‘rho’.

- e:

  a character vector of the error variance of the variable. Used to
  create constraints on the covariance parameters. Defaults to ‘e’.

- collapse:

  whether to collapse the covariance code using ‘PWITH’. Note that at
  the time of writing, Mplus does not allow more than 80 characters per
  row. Defaults to `FALSE`.

## Value

A named character vector of class ‘MplusRstructure’ with four elements:

- `all`: A character string collapsing all other sections.

- `Variances`: A character string containing all of the variances.

- `Covariances`: A character string containing all of the covariances,
  properly labelled to allow constraints and the autoregressive residual
  covariance structure.

- `Constraints`: A character string containing the ‘MODEL CONSTRAINT
  section and code needed to parameterize the residual covariance
  structure as autoregressive.’

## Details

The **homogenous** residual covariance structure estimates one
parameter: the residual variance, \\\sigma^{2}\_{e}\\. The residual
variance is assumed to be identical for all variables and all
covariances are assumed to be zero. The structure is represented in this
table.

|     |                     |                     |                     |     |                     |
|-----|---------------------|---------------------|---------------------|-----|---------------------|
|     | t1                  | t2                  | t3                  | ... | tn                  |
| t1  | \\\sigma^{2}\_{e}\\ |                     |                     | ... |                     |
| t2  | 0                   | \\\sigma^{2}\_{e}\\ |                     | ... |                     |
| t3  | 0                   | 0                   | \\\sigma^{2}\_{e}\\ | ... |                     |
| ... | ...                 | ...                 | ...                 | ... | ...                 |
| tn  | 0                   | 0                   | 0                   | ... | \\\sigma^{2}\_{e}\\ |

The **heterogenous** residual covariance structure estimates **n**
parameters, where **n** is the number of variables. A unique residual
variance is estimated for every variable. All covariances are assumed to
be zero. The structure is represented in this table.

|     |                      |                      |                      |     |                      |
|-----|----------------------|----------------------|----------------------|-----|----------------------|
|     | t1                   | t2                   | t3                   | ... | tn                   |
| t1  | \\\sigma^{2}\_{e1}\\ |                      |                      | ... |                      |
| t2  | 0                    | \\\sigma^{2}\_{e2}\\ |                      | ... |                      |
| t3  | 0                    | 0                    | \\\sigma^{2}\_{e3}\\ | ... |                      |
| ... | ...                  | ...                  | ...                  | ... | ...                  |
| tn  | 0                    | 0                    | 0                    | ... | \\\sigma^{2}\_{en}\\ |

The **compound symmetric** residual covariance structure estimates two
parameters: one for the residual variance , \\\sigma^{2}\_{e}\\, and one
for the covariance. The residual variance is assumed to be identical for
all variables and all covariances are assumed to be identical. The
structure is represented in this table.

|     |                     |                     |                     |     |                     |
|-----|---------------------|---------------------|---------------------|-----|---------------------|
|     | t1                  | t2                  | t3                  | ... | tn                  |
| t1  | \\\sigma^{2}\_{e}\\ |                     |                     | ... |                     |
| t2  | \\\rho\\            | \\\sigma^{2}\_{e}\\ |                     | ... |                     |
| t3  | \\\rho\\            | \\\rho\\            | \\\sigma^{2}\_{e}\\ | ... |                     |
| ... | ...                 | ...                 | ...                 | ... | ...                 |
| tn  | \\\rho\\            | \\\rho\\            | \\\rho\\            | ... | \\\sigma^{2}\_{e}\\ |

The **toeplitz** residual covariance structure estimates **n**
parameters, one for every band of the matrix. The residual variance ,
\\\sigma^{2}\_{e}\\, is assumed to be identical for all variables. The
covariances one step removed are all assumed identical. Likewise for all
further bands. The structure is represented in this table.

|     |                     |                     |                     |     |                     |
|-----|---------------------|---------------------|---------------------|-----|---------------------|
|     | t1                  | t2                  | t3                  | ... | tn                  |
| t1  | \\\sigma^{2}\_{e}\\ |                     |                     | ... |                     |
| t2  | \\\rho\\            | \\\sigma^{2}\_{e}\\ |                     | ... |                     |
| t3  | \\\rho\_{2}\\       | \\\rho\\            | \\\sigma^{2}\_{e}\\ | ... |                     |
| ... | ...                 | ...                 | ...                 | ... | ...                 |
| tn  | \\\rho\_{n}\\       | \\\rho\_{n - 1}\\   | \\\rho\_{n - 2}\\   | ... | \\\sigma^{2}\_{e}\\ |

The **autoregressive** residual covariance structure has two parameters:
the residual variance, \\\sigma^{2}\_{e}\\ and the correlation between
adjacent time points, \\\rho\\. The variances are constrained to be
equal for all time points. A single correlation parameter is estimated.
The \\\rho\\ is the correlation between adjacent time points such as 1
and 2 or 2 and 3. More distant relationships are assumed to have smaller
correlations, decreasing exponentially. Thus between 1 and 3, the
estimate is \\\rho^2\\. The structure is represented in this table.

|     |                     |                     |                     |     |                     |
|-----|---------------------|---------------------|---------------------|-----|---------------------|
|     | t1                  | t2                  | t3                  | ... | tn                  |
| t1  | \\\sigma^{2}\_{e}\\ |                     |                     | ... |                     |
| t2  | \\\rho\\            | \\\sigma^{2}\_{e}\\ |                     | ... |                     |
| t3  | \\\rho^2\\          | \\\rho\\            | \\\sigma^{2}\_{e}\\ | ... |                     |
| ... | ...                 | ...                 | ...                 | ... | ...                 |
| tn  | \\\rho^{n-1}\\      | \\\rho^{n-2}\\      | \\\rho^{n-3}\\      | ... | \\\sigma^{2}\_{e}\\ |

Because structural equation models generally model covariance
structures, the autoregressive residual structure must be parameterized
in terms of covariances. This is done in two parts. First, the function
returns syntax to estimate all the pairwise covariances, labelling the
parameters \\\rho\\, \\\rho^2\\, etc. so that they are constrained to be
equal. Next, it returns the syntax for the necessary model constraints
to constrain the different covariances, to decrease exponentially in
their correlations. This is done via: \$\$\rho^2 =
(\frac{\rho}{\sigma^2\_{e}})^{2}\sigma^2\_{e}\$\$ and likewise for all
later time points.

The **unstructured** residual covariance structure estimates
\\\frac{n(n + 1)}{2}\\ parameters. It is unstructured in that every
variance and covariance is freely estimated with no constraints.
However, in most cases, this results in an overparameterized model and
is unestimable. The structure is represented in this table.

|     |                      |                      |                      |     |                      |
|-----|----------------------|----------------------|----------------------|-----|----------------------|
|     | t1                   | t2                   | t3                   | ... | tn                   |
| t1  | \\\sigma^{2}\_{e1}\\ |                      |                      | ... |                      |
| t2  | \\\rho\_{1}\\        | \\\sigma^{2}\_{e2}\\ |                      | ... |                      |
| t3  | \\\rho\_{2}\\        | \\\rho\_{3}\\        | \\\sigma^{2}\_{e3}\\ | ... |                      |
| ... | ...                  | ...                  | ...                  | ... | ...                  |
| tn  | \\\rho\_{5}\\        | \\\rho\_{6}\\        | \\\rho\_{7}\\        | ... | \\\sigma^{2}\_{en}\\ |

## Author

Joshua F. Wiley <jwiley.psych@gmail.com>

## Examples

``` r
# all five structures collapsing
mplusRcov(letters[1:4], "homogenous", "rho", "e", TRUE)
#> a b c d (e);
#> a WITH b@0 c@0 d@0;
#> b WITH c@0 d@0;
#> c WITH d@0;
#> 
mplusRcov(letters[1:4], "heterogenous", "rho", "e", TRUE)
#> a b c d;
#> a WITH b@0 c@0 d@0;
#> b WITH c@0 d@0;
#> c WITH d@0;
#> 
mplusRcov(letters[1:4], "cs", "rho", "e", TRUE)
#> a b c d (e);
#> a b c PWITH b c d (rho);
#> a b PWITH c d (rho);
#> a PWITH d (rho);
#> 
mplusRcov(letters[1:4], "toeplitz", "rho", "e", TRUE)
#> a b c d (e);
#> a b c PWITH b c d (rho);
#> a b PWITH c d (rho2);
#> a PWITH d (rho3);
#> 
mplusRcov(letters[1:4], "ar", "rho", "e", TRUE)
#> a b c d (e);
#> a b c PWITH b c d (rho);
#> a b PWITH c d (rho2);
#> a PWITH d (rho3);
#> MODEL CONSTRAINT: 
#>   rho2 = ((rho/e)^2) * e;
#>   rho3 = ((rho/e)^3) * e;
#> 
mplusRcov(letters[1:4], "un", "rho", "e", TRUE)
#> a b c d;
#> a WITH b c d;
#> b WITH c d;
#> c WITH d;
#> 

# all five structures without collapsing
# useful for long names or many variables
# where a line may cross 80 characters
mplusRcov(letters[1:4], "homogenous", "rho", "e", FALSE)
#> a b c d (e);
#> a WITH b@0;
#> a WITH c@0;
#> a WITH d@0;
#> b WITH c@0;
#> b WITH d@0;
#> c WITH d@0;
#> 
mplusRcov(letters[1:4], "heterogenous", "rho", "e", FALSE)
#> a b c d;
#> a WITH b@0;
#> a WITH c@0;
#> a WITH d@0;
#> b WITH c@0;
#> b WITH d@0;
#> c WITH d@0;
#> 
mplusRcov(letters[1:4], "cs", "rho", "e", FALSE)
#> a b c d (e);
#> a WITH b (rho);
#> b WITH c (rho);
#> c WITH d (rho);
#> a WITH c (rho);
#> b WITH d (rho);
#> a WITH d (rho);
#> 
mplusRcov(letters[1:4], "toeplitz", "rho", "e", FALSE)
#> a b c d (e);
#> a WITH b (rho);
#> b WITH c (rho);
#> c WITH d (rho);
#> a WITH c (rho2);
#> b WITH d (rho2);
#> a WITH d (rho3);
#> 
mplusRcov(letters[1:4], "ar", "rho", "e", FALSE)
#> a b c d (e);
#> a WITH b (rho);
#> b WITH c (rho);
#> c WITH d (rho);
#> a WITH c (rho2);
#> b WITH d (rho2);
#> a WITH d (rho3);
#> MODEL CONSTRAINT: 
#>   rho2 = ((rho/e)^2) * e;
#>   rho3 = ((rho/e)^3) * e;
#> 
mplusRcov(letters[1:4], "un", "rho", "e", FALSE)
#> a b c d;
#> a WITH b;
#> a WITH c;
#> a WITH d;
#> b WITH c;
#> b WITH d;
#> c WITH d;
#> 
```
