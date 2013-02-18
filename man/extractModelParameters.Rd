\name{extractModelParameters}
\Rdversion{1.1}
\alias{extractModelParameters}
\title{Extract model parameters from MODEL RESULTS section.}
\description{
Extracts the model parameters from the MODEL RESULTS section of one or more Mplus output files.
If a particular output file has more than one results section (unstandardized, stdyx, stdy, and/or std),
a list will be returned. If the \code{target} is a directory, all .out files therein will be parsed
and a single list will be returned, where the list elements are named by the output file name.  
Returned parameters often include the parameter estimate, std. err, param/s.e., and two-tailed p-value.
}
\usage{
extractModelParameters(target=getwd(), recursive=FALSE, filefilter, dropDimensions=FALSE, resultType)
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
  \item{dropDimensions}{Relevant only for multi-file parsing. If \code{TRUE}, then if only one output
  	section (usually unstandardized) is present for all files in the parsed list, then eliminate
  	the second-level list (which contains elements for each output section). The result is
  	that the elements of the returned list are \code{data.frame} objects with the relevant parameters.} 
  \item{resultType}{N.B.: this parameter is deprecated and will be removed in a future version. The
  	new default is to extract all results that are present and return a list (see below for details).
  	\code{resultType} specified the results section to extract. If \code{raw}, the unstandardized 
  	estimates will be returned. "stdyx", "stdy", and "std" are the other options, which extract 
  	different standardized solutions. See the Mplus User's Guide for additional details about the 
  	differences in these standardizations.}
}

\value{
If \code{target} is a single file, a list containing unstandardized and standardized results will be
returned. If all standardized solutions are available, the list element will be named: \code{unstandardized},
\code{stdyx.standardized}, \code{stdy.standardized}, and \code{std.standardized}. If confidence intervals
are output using OUTPUT:CINTERVAL, then a list element named \code{ci.unstandardized} will be included.
Each of these list elements is a \code{data.frame} containing relevant model parameters.

If \code{target} is a directory, a list will be returned, where each element contains the results for
a single file, and the top-level elements are named after the corresponding output file name. Each
element within this list is itself a list, with elements as in the single file case above. 

The core \code{data.frame} for each MODEL RESULTS section typically has the following structure:

\item{paramHeader}{The header that begins a given parameter set. Example: "FACTOR1 BY"}
\item{param}{The particular parameter being measured (within \code{paramHeader}). Example: "ITEM1"}
\item{est}{Parameter estimate value.}
\item{se}{Standard error of the estimate}
\item{est_se}{Quotient of \code{est/se}, representing z-test/t-test in large samples}
\item{pval}{Two-tailed p-value for the \code{est_se} quotient.}

In the case of output from Bayesian estimation (ESTIMATOR=BAYES), the \code{data.frame} will contain
a different set of variables, including some of the above, as well as
\item{posterior_sd}{Posterior standard deviation of the estimate.}
\item{lower_2.5ci}{Lower 2.5 percentile of the estimate.}
\item{upper_2.5ci}{Upper 2.5 percentile (aka 97.5 percentile) of the estimate.}

Also note that the \code{pval} column for Bayesian output represents a one-tailed estimate.

In the case of output from a Monte Carlo study (MONTECARLO: and MODEL POPULATION:), the \code{data.frame} will contain
a different set of variables, including some of the above, as well as
\item{population}{Population parameter value.}
\item{average}{Average parameter estimate across replications.}
\item{population_sd}{Standard deviation of parameter value in population across replications.}
\item{average_se}{Average standard error of estimated parameter value across replications.}
\item{mse}{Mean squared error.}
\item{cover_95}{Proportion of replications whose 95\% confidence interval for the parameter includes the population value.}
\item{pct_sig_coef}{Proportion of replications for which the two-tailed significance test of the parameter is significant (p < .05).}

In the case of confidence interval output (OUTPUT:CINTERVAL), the list element \code{ci.unstandardized} will contain
a different set of variables, including some of the above, as well as
\item{low.5}{Lower 0.5\% CI estimate.}
\item{low2.5}{Lower 2.5\% CI estimate.}
\item{low5}{Lower 5\% CI estimate.}
\item{est}{Parameter estimate value.}
\item{up5}{Upper 5\% (i.e., 95\%) CI estimate.}
\item{up2.5}{Upper 2.5\% (i.e., 97.5\%) CI estimate.}
\item{up.5}{Upper 0.5\% (i.e., 99.5\%) CI estimate.}

If the model contains multiple latent classes, an additional variable, \code{LatentClass},
will be included, specifying the latent class number. Also, the Categorical Latent Variables section
will be included as \code{LatentClass} "Categorical.Latent.Variables."

If the model contains multiple groups, \code{Group} will be included.

If the model contains two-level output (between/within), \code{BetweenWithin} will be included.
}
\author{Michael Hallquist}
\seealso{
\code{\link{extractModelSummaries}}
}
\examples{
\dontrun{
ex3.14 <- extractModelParameters(
	"C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/ex3.14.out")
}
}

\keyword{interface}