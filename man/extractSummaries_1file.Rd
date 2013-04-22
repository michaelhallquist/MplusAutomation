\name{extractSummaries_1file}
\alias{extractSummaries_1file}
\title{Extract the summaries from one file}
\usage{
  extractSummaries_1file(outfiletext, filename, input,
    extract = c("Title", "LL", "BIC", "AIC", "AICC", "Parameters", "Observations", "BLRT", "RMSEA", "CFI", "TLI", "ChiSqModel", "aBIC", "Estimator", "SRMR", "WRMR", "ChiSqBaseline"))
}
\arguments{
  \item{outfiletext}{This is the output file in string form
  to be parsed. Passed in from extractModelSummaries.}

  \item{filename}{Name of the file being parsed. Used in
  case of bad model, prints a warning.}
}
\value{
  A list of the summaries
}
\description{
  Description: This function parses an output file for
  specific model details. It returns a list of model
  details for a single output file.
}
\examples{
# make me!!!
}
\keyword{internal}

