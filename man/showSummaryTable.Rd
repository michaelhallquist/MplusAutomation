\name{showSummaryTable}
\Rdversion{1.1}
\alias{showSummaryTable}

\title{Display summary table of Mplus model statistics in separate window}   
\description{
Displays a summary table of model fit statistics extracted using the \code{extractModelSummaries} function.
This function relies on the \code{showData} function from the relimp package, which displays data in a Tk-based window.
By default, the following summary statistics are included: \code{Title, LL, Parameters, AIC, AICC, BIC, RMSEA_Estimate}, 
but these are customizable using the \code{keepCols} and \code{dropCols} parameters.
}
\usage{
showSummaryTable(modelList, keepCols, dropCols, sortBy, font="Courier 9")
}
\arguments{
  \item{modelList}{A list of models (as a \code{data.frame}) returned from the \code{extractModelSummaries}
  	function.}
  \item{keepCols}{A vector of character strings indicating which columns/variables to display in the summary.
  	Only columns included in this list will be displayed (all others excluded). By default, \code{keepCols} is:
  	\code{c("Title", "LL", "Parameters", "AIC", "AICC", "BIC", "RMSEA_Estimate")}.
  	Example: \code{c("Title", "LL", "AIC", "CFI")}}
  \item{dropCols}{A vector of character strings indicating which columns/variables to omit from the summary.
  	Any column not included in this list will be displayed. By default, \code{dropCols} is \code{NULL}.
  	Example: \code{c("InputInstructions", "TLI")}}
  \item{sortBy}{optional. Field name (as character string) by which to sort the table.
  	Typically an information criterion (e.g., "AIC" or "BIC") is used to sort the table. Defaults to "AICC"}
  \item{font}{optional. The font to be used to display the summary table. Defaults to Courier 9}
}
\value{
No value is returned by this function. It is solely used to display the summary table in a separate window.}
\author{Michael Hallquist}
\note{
You must choose between \code{keepCols} and \code{dropCols} because it's not sensible to use these together to include
and exclude columns. The function will error if you include both parameters.
}
\seealso{
\code{\link{extractModelSummaries}}

\code{\link{HTMLSummaryTable}}

\code{\link{LatexSummaryTable}}

}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}