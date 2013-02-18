\name{readModels}
\Rdversion{1.1}
\alias{readModels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read Parameters, Summary Statistics, and Savedata from Mplus Output}
\description{
Extracts the model parameters, summary statistics, and savedata from the one or more Mplus output files.
This function is essentially a wrapper around extractModelParameters, extractModelSummaries, and getSavedata_Data,
respectively. The goal is to have a single function that parses all (supported) aspects of Mplus output and to combine
these into a list object, with one element per output file identified.
}
\usage{
readModels(target=getwd(), recursive=FALSE, filefilter)
}
%- maybe also 'usage' for other objects documented here.
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
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with one element per file. Each element is composed of up to three subelements: summaries, parameters,
and savedata. If \code{target} is a single file, then the top-level elements will be the summaries, parameters, and savedata,
not a list of files.

\item{summaries}{Summary statistics from \code{extractModelSummaries}, having structure as specified by that function}
\item{parameters}{Model parameters from \code{extractModelParameters}, having structure as specified by that function}
\item{class_counts}{Latent class counts and proportions for models that include a categorical latent variable}
\item{mod_indices}{Model modification indices from \code{extractModIndices}, having structure as specified by that function}
\item{savedata_info}{File information about SAVEDATA files related to this output}
\item{savedata}{SAVEDATA file as an R \code{data.frame}, as described in \code{getSavedata_Data}}
\item{bparameters}{an \code{mcmc.list} object containing the draws from the MCMC chains for a Bayesian model that uses the 
	SAVEDATA: BPARAMETERS command}
\item{residuals}{a list containing relevant information from OUTPUT: RESIDUALS}
\item{tech1}{a list containing parameter specification and starting values from OUTPUT: TECH1}
\item{tech3}{a list containing parameter covariance and correlation matrices from OUTPUT: TECH3}
\item{tech4}{a list containing means, covariances, and correlations for latent variables from OUTPUT: TECH4}
\item{lcCondMeans}{conditional latent class means, obtained using auxiliary(e) syntax in latent class models}
\item{gh5}{a list containing data from the gh5 (graphics) file corresponding to this output. (Requires hdf5 package)}
}

\author{Michael Hallquist}
\seealso{
\code{\link{extractModelSummaries}}, \code{\link{extractModelParameters}}, \code{\link{extractModIndices}},
\code{\link{getSavedata_Fileinfo}}, \code{\link{getSavedata_Data}}, \code{\link{getSavedata_Bparams}}

}
\examples{
\dontrun{
allOutput <- readModels(
	"C:/Program Files/Mplus/Mplus Examples/User's Guide Examples", recursive=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}