\name{mplusModeler}
\alias{mplusModeler}
\title{Create, run, and read Mplus models.}
\usage{
  mplusModeler(object, dataout, modelout, run = 0L,
    check = FALSE, varwarnings = TRUE, ...)
}
\arguments{
  \item{object}{An object of class mplusObject}

  \item{dataout}{the name of the file to output the data to
  for Mplus.}

  \item{modelout}{the name of the output file for the
  model. This is the file all the syntax is written to,
  which becomes the Mplus input file. Typically ends in
  .inp.}

  \item{run}{an integer indicating how many models should
  be run. Defaults to zero.  If zero, the data and model
  input files are all created, but the model is not run.
  This can be useful for seeing how the function works and
  what setup is done. If one, a basic model is run. If
  greater than one, the model is bootstrapped with
  \code{run} replications as well as the basic model.}

  \item{check}{logical whether the body of the Mplus syntax
  should be checked for missing semicolons using the
  \code{\link{parseMplus}} function. Defaults to
  \code{FALSE}.}

  \item{varwarnings}{A logical whether warnings about
  variable length should be left, the default, or removed
  from the output file.}

  \item{\ldots}{additional arguments passed to the
  \code{\link[MplusAutomation]{prepareMplusData}}
  function.}
}
\value{
  An Mplus model object, with results.  If \code{run = 1},
  returns an invisible list of results from the run of the
  Mplus model (see
  \code{\link[MplusAutomation]{readModels}} from the
  MplusAutomation package). If \code{run = 0}, the function
  returns a list with two elements, \sQuote{model} and
  \sQuote{boot} that are both \code{NULL}.  if \code{run >=
  1},returns a list with two elements, \sQuote{model} and
  \sQuote{boot} containing the regular Mplus model output
  and the boot object, respectively.  In all cases, the
  Mplus data file and input files are created.
}
\description{
  This is a convenience wrapper to automate many of the
  usual steps required to run an Mplus model. It relies in
  part on functions from the MplusAutomation package.
}
\details{
  Combined with functions from the MplusAutomation package,
  this function is designed to make it easy to fit Mplus
  models from R and to ease many of the usual frustrations
  with Mplus. For example, Mplus has very specific formats
  it accepts data in, but also very little data management
  facilities. Using \R data management is easy. This
  function is designed to make using data from \R in Mplus
  models easy. It is also common to want to fit many
  different models that are slight variants. This can be
  tedius in Mplus, but using \R you can create one basic
  set of input, store it in a vector, and then just modify
  that (e.g., using regular expressions) and pass it to
  Mplus. You can even use loops or the \code{*apply}
  constructs to fit the same sort of model with little
  variants.
}
\examples{
\dontrun{
# simple example of a model using builtin data
# demonstrates use
test <- mplusObject(
  TITLE = "test the MplusAutomation Package and my Wrapper;",
  MODEL = "
    mpg ON wt hp;
    wt WITH hp;",
  usevariables = c("mpg", "wt", "hp"),
  rdata = mtcars)

 res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)

 # remove files
 unlink("mtcars.dat")
 unlink("model1.inp")
 unlink("model1.out")

 # similar example using a robust estimator for standard errors
 test2 <- update(test, ANALYSIS = ~ "ESTIMATOR = MLR;")

 res2 <- mplusModeler(test2, "mtcars.dat", modelout = "model2.inp", run = 1L)
 unlink("mtcars.dat")
 unlink("model2.inp")
 unlink("model3.out")

 # now use the built in bootstrapping methods
 # note that these work, even when Mplus will not bootstrap
 res3 <- mplusModeler(test2, "mtcars.dat", modelout = "model3.inp", run = 10L)

 unlink("mtcars.dat")
 unlink("model3.inp")
 unlink("model3.out")
 unlink("Mplus Run Models.log")
}
}
\author{
  Joshua Wiley
}
\seealso{
  \code{\link{runModels}} and \code{\link{readModels}}
}

