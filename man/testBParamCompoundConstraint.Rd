\name{testBParamCompoundConstraint}
\Rdversion{1.1}
\alias{testBParamCompoundConstraint}

\title{Test inequality-constrained hypothesis for two or more parameters based on iterations of MCMC chains}   
\description{
Tests an inequality-constrained hypothesis (van de Schoot, Hoijtink, Hallquist, & Boelen, in press) based on
draws from the posterior distribution of the model parameters, which provides information about the proportion
of the distribution that is in agreement with a given hypothesis. This function is used for more complex hypotheses
about three or more parameters, whereas testBParamConstraint tests a simple two-parameter hypothesis.

This function accepts a bparameters object containing iterations of the MCMC chains (rows) for each model parameter (columns)
and prints out the number and proportion of draws that are consistent with the requested hypothesis test.

The \code{test} argument is evaluated directly as \code{R} code, with the \code{bparams} object attached so that
variable names are available directly in the environment. Because the goal is to evaluate the test for each
draw from the posterior distribution, remember to use vector-based logic operators, not boolean operators. That is,
stick to \code{&} or \code{|} for joining tests of parameters, rather than \code{&&} or \code{||} since the latter will
return a single TRUE/FALSE, which is irrelevant.

An example test in R logic would be \code{"(STAITOT.ON.CG > STAITOT.ON.UCG) & (BDIM.ON.CG > BDIM.ON.UCG)"}.
}
\usage{
testBParamCompoundConstraint(bparams, test)
}
\arguments{
  \item{bparams}{An object containing draws from the posterior distribution (class \code{mplus.model} or \code{mplus.bparameters}).
  	Obtained by SAVEDATA:BPARAMETERS in Mplus and \code{\link{getSavedata_Bparams}} or \code{\link{readModels}} in \code{MplusAutomation}.}
  \item{test}{The \code{R} code defining the parameter test of three or more parameters. Example:
    \code{"(STAITOT.ON.CG > STAITOT.ON.UCG) & (BDIM.ON.CG > BDIM.ON.UCG)"}.}
}
\value{
No value is returned by this function. Instead, two summary tables are printed to the screen containing the number and proportion
of draws consistent with the hypothesis.
}
\author{Michael Hallquist}
\seealso{
\code{\link{testBParamConstraint}}
}
\examples{
\dontrun{
#using bparameters directly
btest <- getSavedata_Bparams("model vb1_simpel_b.out"))
testBParametersCompoundConstraint(btest, 
"(STDYX_STAITOT.ON.CG > STDYX_STAITOT.ON.UCG) & (STDYX_BDIM.ON.CG > STDYX_BDIM.ON.UCG)")

#or using readModels
btest <- readModels("model vb1_simpel_b.out"))
testBParametersCompoundConstraint(btest, 
"(STDYX_STAITOT.ON.CG > STDYX_STAITOT.ON.UCG) & (STDYX_BDIM.ON.CG > STDYX_BDIM.ON.UCG)")

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}