\name{extractModIndices}
\Rdversion{1.1}
\alias{extractModIndices}
\title{Extract model modification indices.}
\description{
Extracts the model modification indices from the MODEL MODIFICATION INDICES section of one
or more Mplus output files. If the \code{target} is a directory, all .out files therein will be parsed
and a single list will be returned, where the list elements are named by the output file name.  
Returned parameters typically include the pairwise relationships between variables to be freed,
the change in model chi-square (M.I.), and the expected parameter change (E.P.C.). 
}
\usage{
extractModIndices(target=getwd(), recursive=FALSE, filefilter)
}

\arguments{
  \item{target}{the directory containing Mplus output files (.out) to parse OR the single output file to
  	be parsed. May be a full path, relative path, or a filename within the working directory.
  	Defaults to the current working directory. Example: "C:/Users/Michael/Mplus Runs"}
  \item{recursive}{optional. If \code{TRUE}, parse all models nested in subdirectories
  	within \code{target}. Defaults to \code{FALSE}.}
  \item{filefilter}{a Perl regular expression (PCRE-compatible) specifying particular output
  	files to be parsed within \code{directory}. See \code{regex} or \url{http://www.pcre.org/pcre.txt}
  	for details about regular expression syntax.}
}

\value{
If \code{target} is a single file, a data.frame containing modification results for the target output file will be
returned. If \code{target} is a directory, a list will be returned, where each element contains a data.frame of
the modification indices for a single file, and the top-level elements are named after the corresponding output file name. 

The basic \code{data.frame} containing the MODEL MODIFICATION INDICES section of \code{outfile}. Variables include

\item{modV1}{The first variable in the pair to be freed according to the M.I.}
\item{operator}{The suggested relationship between \code{modV1} and \code{modV2} (e.g., WITH for freeing the covariance between \code{modV1} and \code{modV2})}
\item{modV2}{The first variable in the pair to be freed according to the M.I.}
\item{MI}{The decrease in model chi-square if the specified relationship is freely estimated}
\item{EPC}{The expected parameter estimate between \code{modV1} and \code{modV2} if freed.}
\item{Std_EPC}{The EPC value standardized using the variances of the continuous latent variables.}
\item{StdYX_EPC}{The EPC value standardized using the variances of the continuous latent variables as
	well as the variances of the background and/or outcome variables.}

}
\author{Michael Hallquist}
\seealso{
\code{\link{readModels}}, \code{\link{extractModelSummaries}}, \code{\link{extractModelParameters}}
}
\examples{
\dontrun{
ex3.14 <- extractModIndices(
	"C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/ex3.14.out")}
}

\keyword{interface}