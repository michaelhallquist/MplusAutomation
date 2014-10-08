#' Print an Mplus Residual Structure object
#'
#' This is a method for printing an Mplus Residual Structure object.
#'
#' @param x An object of class MplusRstructure
#' @param \dots Additional arguments to pass on (not currently used)
#' @return \code{NULL} Called for its side effect of printing the object to the console
#' @family Mplus-Formatting
#' @export
#' @method print MplusRstructure
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
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
#' @family Mplus-Formatting
#' @export
#' @method summary mplusObject
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
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

#' Return coefficients for an mplus.model object
#'
#' This is a method for returning the coefficients of an mplus.model object.
#' It works directly on an object stored from \code{readModels} such as:
#' \code{object <- readModels("/path/to/model/model.out")}.
#'
#' @param object An object of class mplusObject
#' @param type A character vector indicating the type of coefficients to return.
#'   One of \dQuote{un}, \dQuote{std}, \dQuote{stdy}, or \dQuote{stdyx}.
#' @param params A character vector indicating what type of parameters to
#'   extract.  Any combination of \dQuote{regression}, \dQuote{loading},
#'   \dQuote{undirected}, \dQuote{expectation}, \dQuote{variability}, and
#'   \dQuote{new}.  A
#'   single one can be passed or multiple.  By default, all are used and
#'   all parameters are returned.
#' @param \dots Additional arguments to pass on (not currently used)
#' @param raw A logical defaulting to \code{FALSE} indicating whether to
#'   parse and return coefficients based on the type (regression, etc.) and
#'   relabel using an arrow notation, or to return the raw coefficients in a named
#'   vector.
#' @return Either a data frame of class \sQuote{mplus.model.coefs}, or in
#'   the case of multiple group models, a list of class \sQuote{mplus.model.coefs},
#'   where each element of the list is a data frame of class \sQuote{mplus.model.coefs},
#'   or a named vector of coefficients, if \code{raw=TRUE}.
#' @seealso \code{\link{readModels}}
#' @family Mplus-Formatting
#' @export
#' @method coef mplus.model
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
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
#' # example of the coef method on an mplud.model object
#' # note that res$results holds the results of readModels()
#' coef(res$results)
#' coef(res$results, type = "std")
#' coef(res$results, type = "stdy")
#' coef(res$results, type = "stdyx")
#'
#' # there is also a method for mplusObject class
#' coef(res)
#'
#' # remove files
#' unlink("mtcars.dat")
#' unlink("model1.inp")
#' unlink("model1.out")
#' unlink("Mplus Run Models.log")
#' }
coef.mplus.model <- function(object, type = c("un", "std", "stdy", "stdyx"),
  params = c("regression", "loading", "undirected", "expectation", "variability", "new"),
  ..., raw=FALSE) {
  type <- match.arg(type)

  stopifnot(!is.null(object$parameters))

  p <- switch(type,
    un = object$parameters$unstandardized,
    std = object$parameters$std.standardized,
    stdy = object$parameters$stdy.standardized,
    stdyx = object$parameters$stdyx.standardized)

  if (raw) {
    n <- paste(p[, "paramHeader"], p[, "param"], sep = ":")
    est <- p[, "est"]
    names(est) <- n
    return(est)
  }

  p2 <- lapply(params, function(i) {
    paramExtract(p, params = i)
  })

  p2 <- p2[!unlist(lapply(p2, is.null))]

  p2 <- lapply(p2, function(res) {
    names <- switch(attr(res, "type"),
      regression = paste0(gsub("\\.ON", "<-", res[, "paramHeader"]), res[, "param"]),
      loading = paste0(res[, "param"], "<-", gsub("\\.BY|\\.\\|", "", res[, "paramHeader"])),
      undirected = paste0(gsub("\\.WITH", "<->", res[, "paramHeader"]), res[, "param"]),
      expectation = paste0(res[, "param"], "<-", gsub("\\.Means|\\.Intercepts|\\.Thresholds", "", res[, "paramHeader"])),
      variability = paste0(res[, "param"], "<->", res[, "param"]),
      new = res[, 'param'])
    cbind(Label = names, res, Section = attr(res, "type"))
  })

  out <- do.call(rbind.data.frame, p2)

  extralabels <- rep("", nrow(out))

  if ("LatentClass" %in% colnames(out)) {
    extralabels <- paste0(extralabels, " C_", out[, "LatentClass"])
  }

  if ("BetweenWithin" %in% colnames(out)) {
    extralabels <- paste0(extralabels, " ", substr(out[, "BetweenWithin"], 1, 1))
  }

  extralabels <- gsub("(\\s)(.*)$", "\\2", extralabels)

  out$Label <- paste(extralabels, out$Label, sep = " ")

  estimate <- "est"
  if ("se" %in% colnames(out)) {
    se <- "se"
  } else if ("posterior_sd" %in% colnames(out)) {
    colnames(out)[which(colnames(out) == "posterior_sd")] <- "se"
    se <- "se"
  }

  pvalue <- "pval"

  if ("Group" %in% colnames(out)) {

    out <- split(out[, c("Label", estimate, se, pvalue)],
                 out[, "Group"])
    out <- lapply(out, function(x) {
      class(x) <- c("mplus.model.coefs", "data.frame")
      return(x)
    })
    class(out) <- c("mplus.model.coefs.list", "list")
  } else {
    out <- out[, c("Label", estimate, se, pvalue)]
    class(out) <- c("mplus.model.coefs", "data.frame")
  }

  return(out)
}

#' Extract coefficients from an mplusObject
#'
#' Method that calls \code{coef.mplus.model}.
#' See further documentation there.
#'
#' @rdname coef.mplus.model
#' @export
coef.mplusObject <- function(object, ...) {
  coef(object$results, ...)
}

#' Extract function to make Mplus output work with the \pkg{texreg} package
#'
#' This is a method for extracting output in a format
#' suitable for the \pkg{texreg} package.  Uses \code{coef} for most the work.
#'
#' @param model An Mplus model object.  This typically comes either from
#'   \code{\link{readModels}} directly, or indirectly via
#'   \code{\link{mplusModeler}}.  The results will have different classes,
#'   but extract methods are defined for both.
#' @param summaries A character vector which summaries to include.
#'   Defaults to \dQuote{none}.
#' @param ... Additional arguments passed to \code{\link{coef.mplus.model}}.
#' @return A \code{texreg} object, or for multiple group models,
#'   a list of \code{texreg} objects.
#' @seealso \code{\link{readModels}}
#' @family Mplus-Formatting
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @export
#' @import texreg methods
#' @keywords interface
#' @name extract
#' @rdname extract
#' @aliases extract.mplus.model
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
#' extract(res$results)
#' # there is also a method for mplusObject class
#' extract(res)
#'
#' # load the texreg package
#' # to use pretty printing via screenreg
#' # uncomment to run these examples
#' # library(texreg)
#' # screenreg(res)
#' # screenreg(res, type = 'stdyx')
#'
#' # screenreg(res, type = 'un', params = 'regression',
#' #   single.row=TRUE)
#' # screenreg(res, type = 'un', params = 'regression', summaries = 'CFI',
#' #   single.row=TRUE)
#'
#' # remove files
#' unlink("mtcars.dat")
#' unlink("model1.inp")
#' unlink("model1.out")
#' unlink("Mplus Run Models.log")
#' }
extract.mplus.model <- function(model, summaries = "none", ...) {
  if (summaries[1] != "none") {
    stopifnot(all(summaries %in% colnames(model$summaries)))

    knownsummaries <- c("Title","Mplus.version", "AnalysisType", "DataType", "Filename", "Estimator",
              "Observations", "Parameters", "ChiSqM_Value", "ChiSqM_DF", "ChiSqM_PValue",
              "ChiSqM_ScalingCorrection", "ChiSqBaseline_Value", "ChiSqBaseline_DF",
              "ChiSqBaseline_PValue", "LL", "UnrestrictedLL", "LLCorrectionFactor",
              "UnrestrictedLLCorrectionFactor", "CFI", "TLI", "AIC", "BIC",
              "aBIC", "RMSEA_Estimate", "RMSEA_90CI_LB", "RMSEA_90CI_UB", "RMSEA_pLT05",
              "WRMR", "ObsRepChiSqDiff_95CI_LB", "ObsRepChiSqDiff_95CI_UB",
              "PostPred_PValue", "DIC", "pD", "SRMR.Within", "SRMR.Between",
              "Entropy", "AICC", "ChiSqM_Mean", "ChiSqM_SD", "ChiSqM_NumComputations",
              "LL_Mean", "LL_SD", "LL_NumComputations", "UnrestrictedLL_Mean",
              "UnrestrictedLL_SD", "UnrestrictedLL_NumComputations", "CFI_Mean",
              "CFI_SD", "CFI_NumComputations", "TLI_Mean", "TLI_SD", "TLI_NumComputations",
              "AIC_Mean", "AIC_SD", "AIC_NumComputations", "BIC_Mean", "BIC_SD",
              "BIC_NumComputations", "aBIC_Mean", "aBIC_SD", "aBIC_NumComputations",
              "RMSEA_Mean", "RMSEA_SD", "RMSEA_NumComputations", "SRMR_Mean",
              "SRMR_SD", "SRMR_NumComputations", "SRMR.Within_Mean", "SRMR.Within_SD",
              "SRMR.Within_NumComputations", "SRMR.Between_Mean", "SRMR.Between_SD",
              "SRMR.Between_NumComputations", "SRMR", "NumFactors")

    knownsummaries.decimals <- rep(TRUE, length(knownsummaries))

    names(knownsummaries) <- names(knownsummaries.decimals) <- knownsummaries

    knownsummaries.decimals[c("Title", "Mplus.version", "AnalysisType", "DataType", "Filename",
      "Observations", "Parameters", "ChiSqM_DF", "ChiSqBaseline_DF")] <- FALSE

    use.decimals <- rep(TRUE, length(summaries))
    use.decimals[which(summaries %in% knownsummaries)] <- knownsummaries.decimals[summaries[which(summaries %in% knownsummaries)]]

    summary.values <- as.numeric(model$summaries[, summaries])
  } else {
    summaries <- character(0)
    summary.values <- numeric(0)
    use.decimals <- logical(0)
  }

  params <- coef(model, ...)
  estimate <- "est"
  se <- "se"
  pvalue <- "pval"

  if (inherits(params, "mplus.model.coefs")) {
    tr <- createTexreg(
      coef.names = as.character(params$Label),
      coef = params[, estimate],
      se = params[, se],
      pvalues = params[, pvalue],
      gof.names = summaries,
      gof = summary.values,
      gof.decimal = use.decimals)
  } else if (inherits(params, "mplus.model.coefs.list")) {
    tr <- lapply(params, function(params.i) {
      createTexreg(
        coef.names = as.character(params.i$Label),
        coef = params.i[, estimate],
        se = params.i[, se],
        pvalues = params.i[, pvalue],
        gof.names = summaries,
        gof = summary.values,
        gof.decimal = use.decimals)
    })
  }

  return(tr)
}


#' @rdname extract
#' @export
extract.mplusObject <- function(model, summaries = "none", ...) {
  extract(model$results, summaries = summaries, ...)
}

#' @rdname extract
#' @export
setMethod("extract", signature = className("mplus.model", "MplusAutomation"),
  definition = extract.mplus.model)

#' @rdname extract
#' @export
setMethod("extract", signature = className("mplusObject", "MplusAutomation"),
  definition = extract.mplusObject)


## m <- list(
##   Mpath = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex3.11.out"),
##   Mpathcat = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex3.14.out"),
##   Mpathbootci = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex3.16.out"),
##   Mcfa = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex5.1.out"),
##   Mcfamimic = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex5.8.out"),
##   Mmgcfa = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex5.16.out"),
##   #Mbayescfa = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex5.32.out"),
##   Mlgm = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex6.1.out"),
##   Mlgmcat = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex6.6.out"),
##   Mlgmran = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex6.12.out"),
##   Mmglgm = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex6.18.out"),
##   Mcox = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex6.20.out"),
##   Mmixture = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex7.1.out"),
##   Mlca = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex7.7.out"),
##   Mmixturecfa = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex7.17.out"),
##   Mirtmix = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex7.27.out"),
##   Mgmm = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex8.1.out"),
##   #Mmslta = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex8.15.out"),
##   Mmlm = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex9.1b.out"),
##   Mmlmpath = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex9.4.out"),
##   Mmlmcfa = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex9.6.out"),
##   Mmlmsem = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex9.10.out"),
##   Mmgmlmsem = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex9.11.out"),
##   #M3levelreg = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex9.20.out"),
##   #Mcrossclassbayes = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex9.25.out"),
##   M2levelmix = readModels("C:/Program Files/Mplus/Mplus Examples/User's Guide Examples/Outputs/ex10.1.out"))

## library(texreg)
## screenreg(list(extract(mtmp1, summaries = c("Observations", "LL", "CFI", "SRMR")),
##  extract(mtmp2, summaries = c("Observations", "CFI"))), single.row=TRUE,
##  custom.model.names = c("Path Analysis", "SEM"))
## screenreg(extract(mtmp3, summaries = c("CFI")),
##  single.row=TRUE, custom.model.names = c("Multiple Group Model"))


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
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
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

  sections <- c("regression", "loading", "undirected", "expectation", "variability")
  res <- lapply(sections, function(params) {
    tmp <- paramExtract(p, params = params)
    if (!nrow(tmp)) return(NULL)
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
