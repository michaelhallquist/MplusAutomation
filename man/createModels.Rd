\name{createModels}
\Rdversion{1.1}
\alias{createModels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Mplus Input Files from Template}
\description{
The \code{createModels} function processes a single Mplus template file and creates a group of related
model input files. Definitions and examples for the template language are provided in the MplusAutomation
vignette and are not duplicated here at the moment. See this PDF: 
\url{http://cran.r-project.org/web/packages/MplusAutomation/vignettes/Vignette.pdf}
}
\usage{
createModels(templatefile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{templatefile}{The filename (absolute or relative path) of an Mplus template file to be processed. Example "C:/MplusTemplate.txt"}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{No value is returned by this function. It is solely used to process an Mplus template file.}
\author{Michael Hallquist}
\examples{
\dontrun{
createModels("L2 Multimodel Template No iter.txt")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}