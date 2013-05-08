\name{extractParameters_1section}
\alias{extractParameters_1section}
\title{Extract Parameters for One Section}
\usage{
  extractParameters_1section(filename, modelSection,
    sectionName)
}
\arguments{
  \item{filename}{}

  \item{modelSection}{}

  \item{sectionName}{}
}
\value{
  A list of parameters
}
\description{
  To do: add details
}
\examples{
\dontrun{
  #a few examples of files to parse
  #mg + lc. Results in latent class pattern, not really different from
  #         regular latent class matching. See Example 7.21
  #mg + twolevel. Group is top, bw/wi is 2nd. See Example 9.11
  #lc + twolevel. Bw/wi is top, lc is 2nd. See Example 10.1.
  #               But categorical latent variables is even higher
  #test cases for more complex output: 7.21, 9.7, 9.11, 10.1
}
}
\keyword{internal}

