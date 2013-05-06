\name{coef.mplusObject}
\alias{coef.mplusObject}
\title{Return coefficients for an mplusObject}
\usage{
  \method{coef}{mplusObject} (object,
    type = c("un", "std", "stdy", "stdyx"), ...)
}
\arguments{
  \item{object}{An object of class mplusObject}

  \item{type}{A character vector indicating the type of
  coefficients to return.  One of \dQuote{un},
  \dQuote{std}, \dQuote{stdy}, or \dQuote{stdyx}.}

  \item{\dots}{Additional arguments to pass on (not
  currently used)}
}
\value{
  A named vector of the (unstandardized) coefficients.
}
\description{
  This is a method for returning the coefficients of an
  mplusObject.
}
\examples{
\dontrun{
# simple example of a model using builtin data
# demonstrates use
test <- mplusObject(
  TITLE = "test the MplusAutomation Package;",
  MODEL = "
    mpg ON wt hp;
    wt WITH hp;",
  OUTPUT = "STANDARDIZED;",
  usevariables = c("mpg", "wt", "hp"),
  rdata = mtcars)

res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)

# example of the coef method
coef(res)
coef(res, type = "std")
coef(res, type = "stdy")
coef(res, type = "stdyx")

# remove files
unlink("mtcars.dat")
unlink("model1.inp")
unlink("model1.out")
unlink("Mplus Run Models.log")
}
}
\author{
  Joshua Wiley
}
\keyword{interface}

