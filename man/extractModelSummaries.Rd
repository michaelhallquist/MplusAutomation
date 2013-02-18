\name{extractModelSummaries}
\Rdversion{1.1}
\alias{extractModelSummaries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract summary statistics from a single output file or from a group of Mplus models within a directory}
\description{
Parses a group of Mplus model output files (.out extension) for model fit statistics.
At this time, the details extracted are fixed and include: \code{Filename, InputInstructions, Title, Estimator, 
LL, BIC, aBIC, AIC, AICC, Parameters, Observations, CFI, TLI, RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB,
RMSEA_pLT05, ChiSqM_Value, ChiSqM_DF, ChiSq_PValue, BLRT_KM1LL, BLRT_PValue, BLRT_Numdraws)}. The 
infrastructure is in place to allow for user-specified selection of summary statistics in future versions.
}
\usage{
extractModelSummaries(target = getwd(), recursive = FALSE, filefilter)
}

\arguments{
  \item{target}{the directory containing Mplus output files (.out) to parse OR the single output file to
  	be parsed. Defaults to the current working directory. Example: "C:/Users/Michael/Mplus Runs"}
  \item{recursive}{optional. If \code{TRUE}, parse all models nested in subdirectories
  	within \code{directory}. Defaults to \code{FALSE}.}
  \item{filefilter}{a Perl regular expression (PCRE-compatible) specifying particular output
  	files to be parsed within \code{directory}. See \code{regex} or \url{http://www.pcre.org/pcre.txt}
  	for details about regular expression syntax.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns a \code{data.frame} containing model fit statistics for all output files within \code{directory}.
The \code{data.frame} contains some of the following variables (depends on model type):

%%  \item{comp1 }{Description of 'comp1'}
\item{Title}{Title for the model, specified by the TITLE: command}
\item{Filename}{Filename of the output file}
%\item{InputInstructions}{A string containing the full input syntax for the model}
\item{Estimator}{Estimator used for the model (e.g., ML, MLR, WLSMV, etc.)}
\item{LL}{Log-likelihood of the model}
\item{BIC}{Bayesian Information Criterion}
\item{aBIC}{Sample-Size-Adjusted BIC (Sclove, 1987)}
\item{AIC}{Akaike's Information Criterion}
\item{AICC}{Corrected AIC, based on Sugiura (1978) and recommended by Burnham & Anderson (2002)}
\item{DIC}{Deviance Information Criterion. Available in ESTIMATOR=BAYES output.}
\item{Parameters}{Number of parameters estimated by the model}
\item{pD}{Estimated number of parameters in Bayesian output}
\item{Observations}{The number of observations for the model (does not suppport multiple-groups analysis at this time)}
\item{CFI}{Confirmatory Fit Index}
\item{TLI}{Tucker-Lewis Index}
\item{RMSEA_Estimate}{Point estimate of root mean squared error of approximation}
\item{RMSEA_90CI_LB}{Lower bound of the 90\% Confidence Interval around the RMSEA estimate.}
\item{RMSEA_90CI_UB}{Upper bound of the 90\% Confidence Interval around the RMSEA estimate.}
\item{RMSEA_pLT05}{Probability that the RMSEA estimate falls below .05, indicating good fit.}
\item{ChiSqM_Value}{Model chi-squared value}
\item{ChiSqM_DF}{Model chi-squared degrees of freedom}
\item{ChiSqM_PValue}{Model chi-squared p value}
\item{ObsRepChiSqDiff_95CI_LB}{Lower bound of 95\% confidence interval for the difference between observed and replicated chi-square values}
\item{ObsRepChiSqDiff_95CI_UB}{Upper bound of 95\% confidence interval for the difference between observed and replicated chi-square values}
\item{PostPred_PValue}{Posterior predictive p-value}
\item{BLRT_KM1LL}{Log-likelihood of the K-1 model (one less class) for the Bootstrapped Likelihood Ratio Test (TECH14).}
\item{BLRT_PValue}{P-value of the Bootstrapped Likelihood Ratio Test (TECH14) testing whether the K class model is significantly better than K-1}
\item{BLRT_Numdraws}{The number of bootstrapped samples used in the Bootstrapped Likelihood Ratio Test}
\item{SRMR}{Standardized root mean square residual}
\item{SRMR.Between}{For TYPE=TWOLEVEL output, standardized root mean square residual for between level}
\item{SRMR.Within}{For TYPE=TWOLEVEL output, standardized root mean square residual for within level}
\item{WRMR}{Weighted root mean square residual}
\item{ChiSqBaseline_Value}{Baseline (unstructured) chi-squared value}
\item{ChiSqBaseline_DF}{Baseline (unstructured) chi-squared degrees of freedom}
\item{ChiSqBaseline_PValue}{Baseline (unstructured) chi-squared p value}
\item{NumFactors}{For TYPE=EFA output, the number of factors}
\item{T11_KM1Starts}{TECH11: Number of initial stage random starts for k-1 model}
\item{T11_KM1Final}{TECH11: Number of final stage optimizations for k-1 model}
\item{T11_KM1LL}{TECH11: Log-likelihood of the K-1 model used for the Vuong-Lo-Mendell-Rubin LRT}
\item{T11_VLMR_2xLLDiff}{TECH11: 2 * Log-likelihood Difference of K-class vs. K-1-class model for the Vuong-Lo-Mendell-Rubin LRT} 
\item{T11_VLMR_ParamDiff}{TECH11: Difference in number of parameters between K-class and K-1-class model for the Vuong-Lo-Mendell-Rubin LRT}
\item{T11_VLMR_Mean}{TECH11: Vuong-Lo-Mendell-Rubin LRT mean}
\item{T11_VLMR_SD}{TECH11: Vuong-Lo-Mendell-Rubin LRT standard deviation}
\item{T11_VLMR_PValue}{TECH11: Vuong-Lo-Mendell-Rubin LRT p-value}
\item{T11_LMR_Value}{TECH11: Lo-Mendell-Rubin Adjusted LRT value}
\item{T11_LMR_PValue}{TECH11: Lo-Mendell-Rubin Adjusted LRT p-value}
}
\author{Michael Hallquist}

\seealso{
\code{\link{regex}}, \code{\link{runModels}}, \code{\link{readModels}}
}
\examples{
\dontrun{
allExamples <- extractModelSummaries(
	"C:/Program Files/Mplus/Mplus Examples/User's Guide Examples")}
}
\keyword{interface}