\name{print.MplusRstructure}
\alias{print.MplusRstructure}
\title{Print an Mplus Residual Structure object}
\usage{
  \method{print}{MplusRstructure} (x, ...)
}
\arguments{
  \item{x}{An object of class MplusRstructure}

  \item{\dots}{Additional arguments to pass on (not
  currently used)}
}
\value{
  \code{NULL} Called for its side effect of printing the
  object to the console
}
\description{
  This is a method for printing an Mplus Residual Structure
  object.
}
\examples{
# default 'show' uses printing
mplusRcov(c("a", "b", "c"), type = "ar")

# also if calling print explicitly
print(mplusRcov(c("a", "b", "c"), type = "ar"))

# to see all aspects of the raw/original object
str(mplusRcov(c("a", "b", "c"), type = "ar"))
}
\author{
  Joshua Wiley
}
\keyword{interface}

