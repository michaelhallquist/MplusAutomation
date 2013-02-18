\name{testBParamConstraint}
\Rdversion{1.1}
\alias{testBParamConstraint}

\title{Test inequality-constrained hypothesis for two parameters based on iterations of MCMC chains}   
\description{
Tests a simple inequality-constrained hypothesis (van de Schoot, Hoijtink, Hallquist, & Boelen, in press) based on
draws from the posterior distribution of the model parameters, which provides information about the proportion
of the distribution that is in agreement with a given hypothesis. This function is used for simple hypothesis
for two parameters, whereas testBParamCompoundConstraint gives full access to multiple parameters and R's logic
syntax.

This function accepts a bparameters object containing iterations of the MCMC chains (rows) for each model parameter (columns)
and prints out the number and proportion of draws that are consistent with the requested hypothesis test.

The \code{coef1}, \code{operator}, and \code{coef2} arguments are appended in sequence, so that the hypothesis test is
constructed from left-to-right. e.g., \code{testBParamConstraint(bparamsDF, "MGM.TRT1", ">", "MGM.EX2")}.
}
\usage{
testBParamConstraint(bparams, coef1, operator, coef2)
}
\arguments{
  \item{bparams}{An object containing draws from the posterior distribution (class \code{mplus.model} or \code{mplus.bparameters}).
  	Obtained by SAVEDATA:BPARAMETERS in Mplus and \code{\link{getSavedata_Bparams}} or \code{\link{readModels}} in \code{MplusAutomation}.}
  \item{coef1}{The name of the first parameter to be compared. Example: \code{"MGM.TRT1"}}
  \item{operator}{A logical operator to compare the two parameters. Should be one of \code{>=, >, <, or <=}.
  	Example: \code{">="}}
  \item{coef2}{The name of the first parameter to be compared. Example: \code{"MGM.EX2"}}
}
\value{
No value is returned by this function. Instead, two summary tables are printed to the screen containing the number and proportion
of draws consistent with the hypothesis.
}
\author{Michael Hallquist}
\seealso{
\code{\link{testBParamCompoundConstraint}}
}
\examples{
\dontrun{
#using bparameters directly
btest <- getSavedata_Bparams("model vb1_simpel_b.out"))
testBParametersConstraint(btest, "STDYX_STAITOT.ON.CG", ">", "STDYX_STAITOT.ON.UCG")

#or using readModels
btest <- readModels("model vb1_simpel_b.out"))
testBParametersConstraint(btest, "STDYX_STAITOT.ON.CG", ">", "STDYX_STAITOT.ON.UCG")

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}