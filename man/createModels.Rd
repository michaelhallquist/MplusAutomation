\name{createModels}
\alias{createModels}
\title{Create Mplus Input Files from Template}
\usage{
  createModels(templatefile)
}
\arguments{
  \item{templatefile}{The filename (absolute or relative
  path) of an Mplus template file to be processed. Example
  \dQuote{C:/MplusTemplate.txt}}
}
\value{
  No value is returned by this function. It is solely used
  to process an Mplus template file.
}
\description{
  The \code{createModels} function processes a single Mplus
  template file and creates a group of related model input
  files. Definitions and examples for the template language
  are provided in the MplusAutomation vignette and are not
  duplicated here at the moment. See this PDF:
  \url{http://cran.r-project.org/web/packages/MplusAutomation/vignettes/Vignette.pdf}
}
\examples{
\dontrun{
  createModels("L2 Multimodel Template No iter.txt")
}
}
\author{
  Michael Hallquist
}
\keyword{interface}

