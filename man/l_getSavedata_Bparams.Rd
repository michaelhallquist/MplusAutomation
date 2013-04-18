\name{l_getSavedata_Bparams}
\alias{l_getSavedata_Bparams}
\title{Internal function to load the draws from the Bayesian model posterior distribution}
\usage{
  l_getSavedata_Bparams(outfile, outfiletext, fileInfo,
    discardBurnin = TRUE)
}
\arguments{
  \item{outfile}{The Mplus output file to be used.}

  \item{outfiletext}{The contents of the Mplus output file}

  \item{fileInfo}{The file info}

  \item{discardBurnin}{Logical value whether to discard the
  burnin iterations or not. Defaults to \code{TRUE}.}
}
\value{
  A list of class \code{mcmc} and \code{mplus.bparameters}
}
\description{
  To do: add details.
}
\keyword{internal}

