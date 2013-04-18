\name{splitDFByRow}
\alias{splitDFByRow}
\title{Split a data frame into a list by rows}
\usage{
  splitDFByRow(df)
}
\arguments{
  \item{df}{An object inheriting from class
  \code{data.frame}}
}
\value{
  A list where each element is a one row data frame
}
\description{
  Takes a data frame and returns a list with an element for
  each row of the data frame. This is an internal function.
}
\examples{
# small example using built in data
MplusAutomation:::splitDFByRow(mtcars)
}
\keyword{internal}

