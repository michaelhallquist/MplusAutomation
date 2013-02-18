\name{runModels}
\Rdversion{1.1}
\alias{runModels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Run Mplus Models}
\description{
This function runs a group of Mplus models (.inp files) located within a
single directory or nested within subdirectories.
}
\usage{runModels(directory = getwd(), recursive = FALSE, showOutput = FALSE, 
	replaceOutfile = "always", logFile = "Mplus Run Models.log", Mplus_command="Mplus")}
\arguments{
  \item{directory}{the directory containing Mplus input files (.inp) to run.
  	Defaults to the current working directory. Example: "C:/Users/Michael/Mplus Runs"
  }
  \item{recursive}{optional. If \code{TRUE}, run all models nested in subdirectories
  	within \code{directory}. Defaults to \code{FALSE}.}
  \item{showOutput}{optional. If \code{TRUE}, show estimation output (TECH8)
  	in the R console. Note that if run within Rgui, output will display within R,
  	but if run via Rterm, a separate window will appear during estimation.}
  \item{replaceOutfile}{optional. Currently supports three settings: "always", which
  	runs all models, regardless of whether an output file for the model exists; "never",
  	which does not run any model that has an existing output file; and "modifiedDate", which
  	only runs a model if the modified date for the input file is more recent than
  	the output file modified date (implying there have been updates to the model).}
  \item{logFile}{optional. If non-null, specifies a file (and optionally, directory)
  	that records the settings passed into the function and the models run (or skipped)
  	during the run.}
  \item{Mplus_command}{optional. N.B.: No need to pass this parameter for most users (has intelligent
  	defaults). Allows the user to specify the name/path of the Mplus executable to be used for
  	running models. This covers situations where Mplus is not in the system's path,
  	or where one wants to test different versions of the Mplus program.}
}
\value{
None. Function is used for its side effects (running models).
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Michael Hallquist}

\seealso{
	\code{\link{runModels_Interactive}}
}
\examples{
\dontrun{
runModels("C:/Users/Michael/Mplus Runs", recursive=TRUE, showOutput=TRUE,
	replaceOutfile="modifiedDate", logFile="MH_RunLog.txt", 
	Mplus_command="C:\\Users\\Michael\\Mplus Install\\Mplus51.exe")}
}

\keyword{interface}
