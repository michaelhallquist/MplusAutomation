\name{mplus.traceplot}
\Rdversion{1.1}
\alias{mplus.traceplot}

\title{Plot the samples for each MCMC chain as a function of iterations}   
\description{
Displays a traceplot of the MCMC draws from the poster distribution of each parameter estimate for a Bayesian Mplus model.
This function requires that 1) PLOT: TYPE=PLOT2; be included in the Mplus input file, 2) a gh5 file be present corresponding
to the Mplus output file (and containing a bayesian_data section), and 3) that the hdf5 package be installed to allow
the gh5 file to be imported.

A multi-panel plot is drawn to the screen and the user is prompted to display the next plot if more than rows x columns estimates are
in the model.
}
\usage{
mplus.traceplot(mplus.model, rows=4, cols=4, parameters_only=TRUE)
}
\arguments{
  \item{mplus.model}{An Mplus model extracted by the \code{readModels} function.}
  \item{rows}{Number of rows to display per plot.}
  \item{cols}{optional. Number of columns to display per plot.}
  \item{parameters_only}{optional. If TRUE, only the unstandardized parameter estimates from the MCMC
  draws will be displayed (as opposed to standardized estimates, r-square estimates, etc.).
  The unstandardized estimates all begin with "Parameter" in the Mplus gh5 output.}
}
\value{
No value is returned by this function. It is solely used to display an MCMC chains traceplot.}
\note{
	Trace and density plots can also be obtained using the coda package and the bparameters element of the mplus.model object.
	This requires that the posterior draws be saved using SAVEDATA: BPARAMETERS syntax. See example below.}
\author{Joseph Glass, Michael Hallquist}
\seealso{
\code{\link{plot.mcmc}}
}

\examples{
\dontrun{
	myModel <- readModels("BayesModel_WithGH5MCMC.out")
	mplus.traceplot(myModel, rows=2, cols=3)
	
	#alternative using the coda package
	library(coda)
	plot(myModel$bparameters$valid_draw)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}