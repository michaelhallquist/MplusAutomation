\name{prepareMplusData}
\Rdversion{1.1}
\alias{prepareMplusData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create tab-delimited file and Mplus input syntax from R data.frame}
\description{
The \code{prepareMplusData} function converts an R data.frame object into a tab-delimited file (without header)
to be used in an Mplus input file. The corresponding Mplus syntax, including the data file definition and
variable names, is printed to the console.
}
\usage{
prepareMplusData(df, filename, keepCols, dropCols, inpfile=FALSE,
  interactive=TRUE, overwrite=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{The R data.frame to be prepared for Mplus}
  \item{filename}{The path and filename for the tab-delimited data file for use with Mplus. Example: "C:/Mplusdata/data1.dat"}
  \item{keepCols}{A character vector specifying the variable names within \code{df} to be output to \code{filename}.}
  \item{dropCols}{A character vector specifying the variable names within \code{df} to be omitted from the data output to \code{filename}.}
  \item{inpfile}{Logical value whether the Mplus syntax should be written to the console or to an input file. Defaults to \code{FALSE}. If
  	\code{TRUE}, the file name will be the same as \code{filename} with
  	the extension changed to .inp.  Alternately, this can be a character 
  	string giving the file name to write the Mplus syntax to.}
  \item{interactive}{Logical value indicating whether file names should be selected interactively. If \code{filename} is missing 
  	and \code{interative=TRUE}, then a dialog box will pop up to select a file or a console prompt if in a non-interactive context. 
  	Defaults to \code{TRUE}.}
  \item{overwrite}{Logical value indicating whether data and input (if present) files should be overwritten.
  	Defaults to \code{TRUE} to be consistent with prior behavior. If \code{FALSE} and the file to write the data to already exists,
	it will throw an error.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{No value is returned by this function. It is solely used to convert an R data.frame to an Mplus data file.}
\author{Michael Hallquist}

\examples{
\dontrun{
library(foreign)

study5 <- read.spss("reanalysis-study-5-mt-fall-08.sav", to.data.frame=TRUE)
ASData5 <- subset(study5, select=c("ppnum", paste("as", 1:33, sep="")))

prepareMplusData(ASData5, "study5.dat")
}
}





% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}