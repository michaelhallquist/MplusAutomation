\name{extractParameters_1chunk}
\alias{extractParameters_1chunk}
\title{Extract Parameters for One Chunk}
\usage{
  extractParameters_1chunk(filename, thisChunk,
    columnNames)
}
\arguments{
  \item{filename}{}

  \item{thisCunk}{}

  \item{columnNames}{}
}
\value{
  A data frame (or matrix?)
}
\description{
  Helper function for extractModelParameters. Used to parse
  each subsection of output within a given file and given
  results section (e.g., stdyx section) There will be many
  chunks if latent classes, multiple groups, multilevel
  features are used.
}
\keyword{internal}

