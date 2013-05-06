#' Print an Mplus Residual Structure object
#'
#' This is a method for printing an Mplus Residual Structure object.
#'
#' @param x An object of class MplusRstructure
#' @param \dots Additional arguments to pass on (not currently used)
#' @return \code{NULL} Called for its side effect of printing the object to the console
#' @export
#' @method print MplusRstructure
#' @author Joshua Wiley
#' @keywords interface
#' @examples
#' # default 'show' uses printing
#' mplusRcov(c("a", "b", "c"), type = "ar")
#'
#' # also if calling print explicitly
#' print(mplusRcov(c("a", "b", "c"), type = "ar"))
#'
#' # to see all aspects of the raw/original object
#' str(mplusRcov(c("a", "b", "c"), type = "ar"))
print.MplusRstructure <- function(x, ...) {
  cat(x$all, fill=TRUE)
}

#' Summarize an mplusObject
#'
#' This is a method for summarizing an mplusObject.
#'
#' @param object An object of class mplusObject
#' @param verbose Logical whether to print verbose output. Defaults to \code{FALSE}.
#' @param \dots Additional arguments to pass on (not currently used)
#' @return \code{NULL} Called for its side effect of printing a model summary to the console
#' @export
#' @method summary mplusObject
#' @author Joshua Wiley
#' @keywords interface
#' @examples
#' \dontrun{
#' # simple example of a model using builtin data
#' # demonstrates use
#' test <- mplusObject(
#'   TITLE = "test the MplusAutomation Package;",
#'   MODEL = "
#'     mpg ON wt hp;
#'     wt WITH hp;",
#'   usevariables = c("mpg", "wt", "hp"),
#'   rdata = mtcars)
#'
#'  res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)
#'
#' # example of the summary method
#' summary(res)
#'
#' # example of verbose output
#' summary(res, verbose=TRUE)
#'
#' # remove files
#' unlink("mtcars.dat")
#' unlink("model1.inp")
#' unlink("model1.out")
#' unlink("Mplus Run Models.log")
#' }
summary.mplusObject <- function(object, verbose=FALSE, ...) {
  stopifnot(!is.null(object$results))

  x <- object$results$summaries

  if(!verbose) {

    cat(gsub("(.*)(;)", "\\1 \n\n", x$Title))
    cat(sprintf("Estimated using %s \n", x$Estimator))

    cat(sprintf("Number of obs: %s, number of (free) parameters: %s \n\n",
      x$Observations, x$Parameters))

    cat(with(x, sprintf("Model: Chi2(df = %s) = %s, p = %s \n",
      ChiSqM_DF, ChiSqM_Value, ChiSqM_PValue)))

    cat(with(x, sprintf("Baseline model: Chi2(df = %s) = %s, p = %s \n\n",
      ChiSqBaseline_DF, ChiSqBaseline_Value, ChiSqBaseline_PValue)))

    cat("Fit Indices: \n\n")
    cat(with(x, sprintf("CFI = %s, TLI = %s, SRMR = %s \n", CFI, TLI, SRMR)))

    cat(with(x, sprintf("RMSEA = %s, 90%% CI [%s, %s], p < .05 = %s \n",
      RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB, RMSEA_pLT05)))

    cat(with(x, sprintf("AIC = %s, BIC = %s \n", AIC, BIC)))
  } else if(verbose) {
    invisible(lapply(names(x), function(n) {
      cat(sprintf("%s: %s \n", n, x[[n]]))
    }))
  }
}

#' Return coefficients for an mplusObject
#'
#' This is a method for returning the coefficients of an mplusObject.
#'
#' @param object An object of class mplusObject
#' @param type A character vector indicating the type of coefficients to return.
#'   One of \dQuote{un}, \dQuote{std}, \dQuote{stdy}, or \dQuote{stdyx}.
#' @param \dots Additional arguments to pass on (not currently used)
#' @return A named vector of the (unstandardized) coefficients.
#' @export
#' @method coef mplusObject
#' @author Joshua Wiley
#' @keywords interface
#' @examples
#' \dontrun{
#' # simple example of a model using builtin data
#' # demonstrates use
#' test <- mplusObject(
#'   TITLE = "test the MplusAutomation Package;",
#'   MODEL = "
#'     mpg ON wt hp;
#'     wt WITH hp;",
#'   OUTPUT = "STANDARDIZED;",
#'   usevariables = c("mpg", "wt", "hp"),
#'   rdata = mtcars)
#'
#' res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)
#'
#' # example of the coef method
#' coef(res)
#' coef(res, type = "std")
#' coef(res, type = "stdy")
#' coef(res, type = "stdyx")
#'
#' # remove files
#' unlink("mtcars.dat")
#' unlink("model1.inp")
#' unlink("model1.out")
#' unlink("Mplus Run Models.log")
#' }
coef.mplusObject <- function(object, type = c("un", "std", "stdy", "stdyx"), ...) {
  type <- match.arg(type)
  stopifnot(!is.null(object$results))

  p <- switch(type,
    un = object$results$parameters$unstandardized,
    std = object$results$parameters$std.standardized,
    stdy = object$results$parameters$stdy.standardized,
    stdyx = object$results$parameters$stdyx.standardized)

  n <- paste(p[, "paramHeader"], p[, "param"], sep = ":")

  est <- p[, "est"]
  names(est) <- n

  return(est)
}


#' Plot coefficients for an mplusObject
#'
#' This is a method for plotting the coefficients of an mplusObject.
#'
#' @param x An object of class mplusObject
#' @param y Not currently used
#' @param type A character vector indicating the type of coefficients to return.
#'   One of \dQuote{un}, \dQuote{std}, \dQuote{stdy}, or \dQuote{stdyx}. Defaults to \dQuote{stdyx}.
#' @param \dots Additional arguments to pass on (not currently used)
#' @return Nothing.  Called for its side effect of plotting the coefficients.
#' @export
#' @method plot mplusObject
#' @importFrom lattice dotplot
#' @author Joshua Wiley
#' @keywords interface
#' @examples
#' \dontrun{
#' # simple example of a model using builtin data
#' # demonstrates use
#' test <- mplusObject(
#'   TITLE = "test the MplusAutomation Package;",
#'   MODEL = "
#'     mpg ON wt hp;
#'     wt WITH hp;",
#'   OUTPUT = "STANDARDIZED;",
#'   usevariables = c("mpg", "wt", "hp"),
#'   rdata = mtcars)
#'
#' res <- mplusModeler(test, "mtcars.dat", modelout = "model1.inp", run = 1L)
#'
#' # example of the coef method
#' plot(res)
#'
#' # remove files
#' unlink("mtcars.dat")
#' unlink("model1.inp")
#' unlink("model1.out")
#' unlink("Mplus Run Models.log")
#' }
plot.mplusObject <- function(x, y, type = c("stdyx", "un", "std", "stdy"), ...) {
  type <- match.arg(type)
  stopifnot(!is.null(x$results))

  if (type == "stdyx" & is.null(x$results$parameters$stdyx.standardized)) {
    warning("No standardized estimates, using unstandardized")
    type <- "un"
  }

  p <- switch(type,
    un = x$results$parameters$unstandardized,
    std = x$results$parameters$std.standardized,
    stdy = x$results$parameters$stdy.standardized,
    stdyx = x$results$parameters$stdyx.standardized)

  sections <- c("directed", "undirected", "expectation", "variability")
  res <- lapply(sections, function(type) {
    tmp <- paramExtract(p, type = type)
    n <- paste(tmp[, "paramHeader"], tmp[, "param"], sep = ":")
    data.frame(Name = n, Estimate = tmp[, "est"], Section = paste("Type:", type),
       stringsAsFactors=FALSE)
  })
  res <- do.call(rbind, res)

  res$Section <- factor(res$Section)
  res <- res[order(res$Section), ]
  res$Name <- factor(res$Name, levels = unique(res$Name))

  dotplot(Estimate ~ Name | Section, data = res, ylab="\n",
    scales=list(relation="free", x = list(rot=45)))
}
