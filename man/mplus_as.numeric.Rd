% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilityFunctions.R
\name{mplus_as.numeric}
\alias{mplus_as.numeric}
\title{Convert Mplus Number to Numeric}
\usage{
mplus_as.numeric(vec, expect_sig = FALSE)
}
\arguments{
\item{vec}{A character vector of Mplus numbers
to convert to numeric}

\item{expect_sig}{Whether to expect significance values denoted by asterisk;
yields a 'sig' attribute that will be TRUE/FALSE}
}
\value{
A numeric vector
}
\description{
Helper function to convert strings formatted in Mplus
Fortran-style scientific notation using D to indicate double.
}
\examples{
MplusAutomation:::mplus_as.numeric("3.1D2")
}
\keyword{internal}
