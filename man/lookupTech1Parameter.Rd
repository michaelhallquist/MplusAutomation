\name{lookupTech1Parameter}
\Rdversion{1.1}
\alias{lookupTech1Parameter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lookup the matrix element for a give parameter number}
\description{
The \code{lookupTech1Parameter} function identifies the position in the Mplus model
matrices corresponding to a given parameter defined in the TECHNICAL 1 PARAMETER
SPECIFICATION OUTPUT. The goal of this function is to aid in identifying problematic parameters
often printed in the warnings and errors section of Mplus output.
}
\usage{
lookupTech1Parameter(tech1Output, paramNumber)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tech1Output}{The object corresponding to the TECH1 parameter specification from readModels.}
  \item{paramNumber}{The parameter number to lookup}
}
\value{A \code{data.frame} containing the row(s) and column(s) of TECH1 parameter specification matching
the requested \code{paramNumber}.}
\author{Michael Hallquist}

\examples{
\dontrun{models <- readModels("test1.out")
	param <- lookupTech1Parameter(models$tech1, 16)
	}
}
\seealso{
	\code{\link{readModels}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}