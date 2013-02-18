\name{runModels_Interactive}
\Rdversion{1.1}
\alias{runModels_Interactive}
\title{Run Mplus Models Using Graphical Interface}
\description{
This function is provides a graphical user interface to the \code{runModels} function.
It uses Tcl/Tk to display a window in which the user can specify parameters for \code{runModels},
including the directory for runs, recursing through subdirectories, displaying output on the console,
and replacing existing outfiles.  
}
\usage{
runModels_Interactive(directory = getwd(), recursive = "0", 
	showOutput = "1", replaceOutfile = "1", checkDate = "0",
	logFile="1")
}

\arguments{
  \item{directory}{optional. The starting directory that will display in the dialog window. Defaults to the 
  	current working directory.}
  \item{recursive}{optional. Whether the recursive checkbox should be checked when the window opens. "0" for
  	FALSE, "1" for TRUE.}
  \item{showOutput}{optional. Whether the show output checkbox should be checked when the window opens. "0" for
  	FALSE, "1" for TRUE.}
  \item{replaceOutfile}{optional. Whether the replace outfile checkbox should be checked when the window opens. 
  	"0" for FALSE, "1" for TRUE.}
  \item{checkDate}{optional. Whether the check modified date checkbox should be checked when the window opens.
  	"0" for FALSE, "1" for TRUE.}
  \item{logFile}{optional. Whether the log file checkbox should be checked when the window opens.
  	"0" for FALSE, "1" for TRUE.}
}
\details{
This function exists as a GUI wrapper for \code{runModels} and does not provide any distinct functionality.
}
\value{
None. Function is used to display user interface for running models.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Michael Hallquist}
\seealso{
\code{\link{runModels}}
}
%\examples{
%runModels_Interactive(directory="C:/")
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}