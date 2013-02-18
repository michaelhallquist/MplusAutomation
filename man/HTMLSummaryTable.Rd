\name{HTMLSummaryTable}
\Rdversion{1.1}
\alias{HTMLSummaryTable}

\title{Create an HTML file containing a summary table of Mplus model statistics}   
\description{
Creates an HTML file containing a summary table of model fit statistics extracted using the \code{extractModelSummaries} function.
By default, the following summary statistics are included: \code{Title, LL, Parameters, AIC, AICC, BIC, RMSEA_Estimate}, 
but these are customizable using the \code{keepCols} and \code{dropCols} parameters.
}
\usage{
HTMLSummaryTable(modelList, filename=file.path(getwd(), "Model Comparison.html"), 
	keepCols, dropCols, sortBy, display=FALSE)
}
\arguments{
  \item{modelList}{A list of models (as a \code{data.frame}) returned from the \code{extractModelSummaries}
  	function.}
  \item{filename}{The name of the HTML file to be created. Can be an absolute or relative path.
  	If \code{filename} is a relative path or just the filename, then it is assumed that the file resides in
  	the working directory \code{getwd()}. Example: \code{"Mplus Summary.html"}}
  \item{keepCols}{A vector of character strings indicating which columns/variables to display in the summary.
  	Only columns included in this list will be displayed (all others excluded). By default, \code{keepCols} is:
  	\code{c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")}.
  	Example: \code{c("Title", "LL", "AIC", "CFI")}}
  \item{dropCols}{A vector of character strings indicating which columns/variables to omit from the summary.
  	Any column not included in this list will be displayed. By default, \code{dropCols} is \code{NULL}.
  	Example: \code{c("InputInstructions", "TLI")}}
  \item{sortBy}{optional. Field name (as character string) by which to sort the table.
  	Typically an information criterion (e.g., "AIC" or "BIC") is used to sort the table. Defaults to "AICC".}
  \item{display}{optional. This parameter specifies whether to display the table in a web browser
  	upon creation (\code{TRUE} or \code{FALSE}).}
}
\value{
No value is returned by this function. It is solely used to create an HTML file containing summary statistics.}
\author{Michael Hallquist}
\note{
You must choose between \code{keepCols} and \code{dropCols} because it's not sensible to use these together to include
and exclude columns. The function will error if you include both parameters.
}
\seealso{
\code{\link{extractModelSummaries}}, \code{\link{showSummaryTable}}, \code{\link{LatexSummaryTable}}

}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}