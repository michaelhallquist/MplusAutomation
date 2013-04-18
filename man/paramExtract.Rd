\name{paramExtract}
\alias{paramExtract}
\title{Extract parameters from a data frame of Mplus estimates}
\usage{
  paramExtract(x,
    type = c("directed", "undirected", "expectation", "variability"))
}
\arguments{
  \item{x}{A data frame (specifically the type returned by
  \code{readModels}) containing parameters. Should be
  specific such as unstandardized and the data frame must
  have a column called \sQuote{paramHeader}.}

  \item{type}{A character string indicating the types of
  parameters to be returned.  Options currently include
  \sQuote{directed}, \sQuote{undirected},
  \sQuote{expectation}, and \sQuote{variability}. Directed
  paths include both regression of one variable \code{ON}
  another, as well as indicator variables (which are
  assumed caused by the underlying latent variable).  Note
  that the latter case includes variables in growth models.
  Undirected paths currently only include covariances,
  indicated by the \code{WITH} syntax in Mplus. Expectation
  paths are the unconditional or conditional expectations
  of variables.  In other words those parameters related to
  the first moments. For independent variables, these are
  the means, \eqn{E(X)} and the conditional means or
  intercepts, \eqn{E(X | f(\theta))}{E(X | f(theta))} where
  \eqn{f(\theta)}{f(theta)} is the model, some function of
  the parameters, \eqn{\theta}{theta}. Finally
  \sQuote{variability} refers to both variances and
  residual variances, corresponding to the second moments.
  As with the expectations, variances are unconditional for
  variables that are not predicted or conditioned on any
  other variable in the model whereas residual variances
  are conditional on the model. Note that \R uses fuzzy
  matching so that each of these can be called via
  shorthand, \sQuote{d}, \sQuote{u}, \sQuote{e}, and
  \sQuote{v}.}
}
\value{
  A subset data frame with the parameters of interest.
}
\description{
  This is a simple convenience function designed to
  facilitate looking at specific parameter types by easily
  return a subset of a data frame with those types only. It
  is designed to follow up the results returned from the
  \code{\link{readModels}} function.
}
\examples{
\dontrun{
  test <- mplusObject(
    TITLE = "test the MplusAutomation Package and my Wrapper;",
    MODEL = "
      mpg ON wt hp;
      wt WITH hp;",
    usevariables = c("mpg", "wt", "hp"),
    rdata = mtcars)

  res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)

  # store just the unstandardized parameters in 'd'
  d <- res$results$parameters$unstandardized
  # extract just directed parameters
  paramExtract(d, "directed")
  # extract other types of parameters using shorthand
  paramExtract(d, "u")
  paramExtract(d, "e")
  paramExtract(d, "v")
}
}
\author{
  Joshua Wiley
}
\seealso{
  \code{\link{readModels}}
}
\keyword{utils}

