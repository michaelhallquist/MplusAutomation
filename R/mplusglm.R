#' Internal Function for Multinomial Regression in Mplus
#'
#' @param dv A character string with the variable name for the
#'   dependent (outcome) variable.
#' @param iv A character vector with the variable name(s) for the
#'   independent (predictor/explanatory) variable(s).
#' @param data A dataset.
#' @param idvar Optional. A character string indicating the name
#'  of the ID variable. Not currently used but may be used in future.
#' @param integration An integer indicating the number of Monte Carlo
#'   integration points to use. Defaults to 1000.
#' @param processors An integer indicating the number of processors to
#'   use. Passed to Mplus. Defaults to 2.
#' @param OR A logical value whether odds ratios should be returned.
#'   Defaults to \code{TRUE}.
#' @param pairwise A logical value indicating whether all pairwise
#'   tests should be computed. Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{mplusModeler()}.
#' @return A list of results and Mplus model object.
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @importFrom data.table as.data.table :=
#' @examples
#' \dontrun{
#'
#' set.seed(1234)
#' tmpd <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   x3 = cut(rnorm(200),
#'            breaks = c(-Inf, -.7, .7, Inf),
#'            labels = c("a", "b", "c")))
#' tmpd$y <- cut(rnorm(200, sd = 2) + tmpd$x1 + tmpd$x2 + I(tmpd$x3 == "b"),
#'               breaks = c(-Inf, -.5, 1, Inf),
#'               labels = c("L", "M", "H"))
#'
#' tmpres <- MplusAutomation:::.mplusMultinomial(
#'   dv = "y",
#'   iv = c("x1", "x2"),
#'   data = tmpd,
#'   pairwise = TRUE)
#' tmpres2 <- MplusAutomation:::.mplusMultinomial(
#'   dv = "y",
#'   iv = c("x1", "x2"),
#'   data = tmpd,
#'   pairwise = FALSE)
#' tmpres3 <- MplusAutomation:::.mplusMultinomial(
#'   dv = "y",
#'   iv = c("x1@0", "x2@0"),
#'   data = tmpd,
#'   pairwise = FALSE)
#'
#' }
.mplusMultinomial <- function(dv, iv, data, idvar = "",
                              integration = 1000, processors = 2,
                              OR = TRUE, pairwise = TRUE, ...) {
  ivclean <- gsub("^(.+)(@.*)$", "\\1", iv)

  m <- mplusObject(
    TITLE = "Class Prediction;",
    VARIABLE = gsub("%dv%", dv,
                    gsub("%ivclean%", paste(ivclean, collapse = "\n"),
                         gsub("%idvar%", ifelse(
                                           nzchar(idvar),
                                           sprintf("IDVARIABLE = %s;", idvar),
                                           ""), "
    USEVARIABLES = %dv%
      %ivclean%
      ;
    NOMINAL = %dv%;
    %idvar%
    "))),
    ANALYSIS = gsub("%procs%", processors, gsub("%integration%", integration, "
      ESTIMATOR = MLR;
      ALGORITHM = INTEGRATION;
      INTEGRATION = MONTECARLO (%integration%);
      PROCESSORS = %procs%;
    ")),
    MODEL = gsub("%dv%", dv,
                 gsub("%iv%", paste(iv, collapse = "\n"),
                      gsub("%ivclean%",
                           paste(ivclean, collapse = "\n"), "
    %dv% ON
      %iv%
      ;

    [%ivclean%]; ! deal with missing
    "))),
    OUTPUT = "
      STANDARDIZED;
      CINTERVAL;",
    usevariables = names(data),
    rdata = data,
    autov = FALSE)

  allldv <- levels(data[[dv]])
  tabres <- vector("list", length = length(allldv))

  for (i in if(isTRUE(pairwise)) seq_along(allldv) else length(allldv)) {
    data[[dv]] <- factor(data[[dv]], levels = c(allldv[-i], allldv[i]))
    ldv <- levels(data[[dv]])
    res <- mplusModeler(object = update(m, rdata = data),
                        dataout = make.names(paste0(dv, substr(paste(ivclean, collapse = "_"), 1, 30), ".dat")),
                        run = 1L, ...)

    tab <- cbind(coef(res), confint(res)[, -1])
    labs <- do.call(rbind, strsplit(tab$Label, "<-"))
    labs <- gsub("\\s", "", labs)
    tab$LHS <- labs[, 1]
    tab$RHS <- labs[, 2]
    usedex <- grepl(sprintf("^\\s%s.*#[0-9]+<-.*", substr(dv, 1, 8)),
                    tab$Label, ignore.case = TRUE)
    tab <- tab[usedex, ]
    stopifnot(all(grepl("#", tab$LHS)))
    tab$Ref <- ldv[length(ldv)]
    tab$Comparator <- ldv[as.integer(gsub("^(.+)#([0-9]+)$", "\\2", tab$LHS))]
    tabres[[i]] <- tab
  }

  #create local variables to avoid R CMD check complaints
  UID <- RHS <- Ref <- Comparator <- Type <- Res <- est <- pval <- LowerCI <- UpperCI <- NULL
  
  Table <- do.call(rbind, tabres)
  uTable <- as.data.table(Table)
  uTable[, UID := paste0(RHS, paste(sort(c(Ref, Comparator)), collapse = "")),
          by = 1:NROW(uTable)]
  uTable <- uTable[!duplicated(UID)]

  uTable[, Type := paste0("Ref = ", Ref, ", Comp = ", Comparator)]
  if (OR) {
    uTable[, Res := sprintf("%0.2f, %0.4f [%0.2f, %0.2f]", exp(est), pval, exp(LowerCI), exp(UpperCI))]
  } else {
    uTable[, Res := sprintf("%0.2f, %0.4f [%0.2f, %0.2f]", est, pval, LowerCI, UpperCI)]
  }

  wTable <- reshape(
    uTable[, .(Res, Type, RHS)],
    timevar = "Type",
    idvar = "RHS",
    direction = "wide")

  list(
    Table = Table,
    UniqueTable = uTable,
    WideTable = wTable,
    Model = res)
}



#' Function to fit GLMs in Mplus
#'
#' The purpose of this function is to make it (relatively) easy to fit
#' (most) generalized linear models in Mplus. Fitting GLMs in Mplus
#' offers advantages such as using full information maximum likelihood
#' for missing data, robust estimators (default used is MLR),
#' and standard errors adjusted for clustering (planned; not currently
#' available via \code{mplusGLM()}. The overarching aim of this function
#' is to make most GLMs as easy to fit in Mplus as they are in R.
#'
#' Note that although there are benefits to fitting GLMs in Mplus.
#' Caution also is warranted. Using full information maximum likelihood
#' for missing data requires a number of assumptions. These may be (badly)
#' violated. \code{mplusGLM()} requires the analyst to check these as
#' appropriate.
#'
#' Currently, \code{mplusGLM()} only supports multinomial outcomes.
#' More outcomes are planned in the future including binary,
#' continuous/normal, and count outcomes.
#'
#' @param formula An R formula class object as used in \code{glm()}.
#'   Note that currently, only basic formula are accepted. On the fly
#'   recoding, arthimetic, and on the fly interactions do not currently
#'   work.
#' @param data A dataset.
#' @param idvar Optional. A character string indicating the name
#'  of the ID variable. Not currently used but may be used in future.
#' @param ... Additional arguments passed to helper functions.
#'   For example \code{.mplusMultinomial()}.
#' @return A list of results and Mplus model object.
#' @export
#' @author Joshua F. Wiley <jwiley.psych@@gmail.com>
#' @importFrom stats model.matrix update get_all_vars
#' @examples
#' \dontrun{
#' set.seed(1234)
#' tmpd <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   x3 = cut(rnorm(200),
#'            breaks = c(-Inf, -.7, .7, Inf),
#'            labels = c("a", "b", "c")))
#' tmpd$y <- cut(rnorm(200, sd = 2) + tmpd$x1 + tmpd$x2 + I(tmpd$x3 == "b"),
#'               breaks = c(-Inf, -.5, 1, Inf),
#'               labels = c("L", "M", "H"))
#' test <- mplusGLM(y ~ x1 + x2 + x3, data = tmpd)
#' }
mplusGLM <- function(formula, data, idvar = "", ...) {
  mf <- get_all_vars(formula = formula, data = data)
  dv <- names(mf)[1]
  iv <- names(mf)[-1]
  if (nzchar(idvar)) {
    mf[[idvar]] <- data[[idvar]]
  }

  mf <- as.data.frame(mf)

  if (!inherits(mf[[dv]], "factor") || is.ordered(mf[[dv]])) {
    stop("Currently only supports nominal outcomes")
  }
  uniquerecords <- c(-999988887777, -999900008888, -888877776666)

  ivraw <- as.list(iv)
  ivexpanded <- vector("list", length(iv))
  for (i in seq_along(iv)) {
    v <- iv[i]
    x <- mf[[v]]
    if (isTRUE(is.factor(x)) | isTRUE(is.character(x))) {
      x2 <- factor(x)
      lx <- levels(x2)
      x2 <- as.character(x2)
      j <- 1
      while (isTRUE(any(uniquerecords[j] %in% unique(x2)))) {
        j <- j + 1
        if (isTRUE(j > 3)) stop("Cannot find a unique value for missing data")
      }
      x2[is.na(x2)] <- uniquerecords[j]
      x2 <- factor(x2, levels = c(uniquerecords[j], lx))
      x3 <- model.matrix(~ x2)
      x3[is.na(x), ] <- NA
      x3 <- as.data.frame(x3)[, -(1:2), drop = FALSE]
      names(x3) <- gsub("^x2", v, gsub("\\.", "_", make.names(names(x3))))
      mf <- cbind(
        mf[, -which(names(mf) == v), drop = FALSE],
        x3)
      ivexpanded[[i]] <- names(x3)
    } else {
      ivexpanded[[i]] <- v
    }
  }

  res1 <- .mplusMultinomial(
    dv = dv,
    iv = unlist(ivexpanded),
    data = mf,
    idvar = idvar,
    pairwise = TRUE, ...)
  k1 <- res1$Model$results$summaries$Parameters
  cf1 <- res1$Model$results$summaries$LLCorrectionFactor
  ll1 <- res1$Model$results$summaries$LL

  res.tests <- do.call(rbind, lapply(seq_along(iv), function(i) {
    tmpivexpanded <- ivexpanded
    tmpivexpanded[[i]] <- paste0(tmpivexpanded[[i]], "@0")

  res0 <- .mplusMultinomial(
    dv = dv,
    iv = unlist(tmpivexpanded),
    data = mf,
    idvar = idvar,
    pairwise = FALSE, ...)

    k0 <- res0$Model$results$summaries$Parameters
    dfDiff <- (k1 - k0)
    cf0 <- res0$Model$results$summaries$LLCorrectionFactor
    ll0 <- res0$Model$results$summaries$LL
    cf <- (k0 * cf0 - k1 * cf1) / (k0 - k1)

    if (isTRUE(cf > 0)) {
      ChiSqDiff <- -2 * (ll0 - ll1) / cf
      pDiff <- pchisq(ChiSqDiff, dfDiff, lower.tail = FALSE)
    } else {
      ChiSqDiff  <- pDiff <- NA_real_
    }

    sum <- sprintf("%s: Chi-square = %0.3f, df = %d, p %s",
                   iv[i],
                   ChiSqDiff, dfDiff,
                   ifelse(pDiff < .0001, "< .0001",
                          paste0("= ",
                                 gsub("^0\\.", ".", format(round(pDiff, 4),
                                                           digits = 4,
                                                           nsmall = 4,
                                                           scientific = FALSE)))))

    data.frame(
      Variable = iv[i],
      summary = sum,
      ChiSqDiff = ChiSqDiff,
      dfDiff = dfDiff,
      pDiff = pDiff,
      stringsAsFactors = FALSE)
  }))

  list(
    Full = res1,
    Tests = res.tests)
}
