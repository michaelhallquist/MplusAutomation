##' Long data two wide latent growth mixture model
##'
##' This function streamlines the process of converting long data into a format
##' that Mplus can use for latent growth mixture models in wide form.  It makes
##' use of continuous time scores, and these time scores must be supplied as
##' variables in the R dataset.  For the conversion to wide form, it is assumed
##' that although assessments may have happened in continuous time, a discrete
##' number of assessments (likely ismilar for all participants) were collected.
##'
##' One valuable feature of this function is that it makes it possible to feed
##' any continuous time scores to Mplus for mixture modelling.  For example,
##' continuous linear time is straightforward, but so to are quadratic
##' time models or piecewise models.  Using facilities in R, spline models
##' are also comparatively easy to specify.
##'
##' @param data A data frame in long format (i.e., multiple rows per ID).
##' @param idvar A character string of the variable name in the dataset
##'   that is the ID variable.
##' @param assessmentvar A character string of the variable name in the dataset that
##'   indicates the particular assessment point for each timepoint.
##' @param dv A character string of the dependent variable name.
##' @param timevars A character vector of the time variables.
##'   Can be a single variable or more than one.  By allowing more than one variable,
##'   it is easy to include linear; linear and quadratic; it is also possible to
##'   calculate splines in R and pass these. The variable names should be 7 characters
##'   or fewer, each.
##' @param misstrick A logical value whether to set values of the DV where a time variable
##'   is missing to missing as well.  Defaults to \code{TRUE}.
##' @param k An integer indicating the number of distinct classes to test.
##'   Currently must be greater than 0 and less than 10.
##' @param title A character string giving a title for the model
##' @param base A character string providing a base name for model outputs,
##'   that is combined with the number of classes.
##' @param run A logical value whether or not to run the models or only
##'   create the data and input files, but not run them.
##' @param processors An integer value indicating the number of processors to use.
##' @param starts A character string passed to Mplus providing the number of
##'   random starts and iterations
##' @param newdata A data frame of new values to use for generating predicted
##'   trajectories by class.
##' @param cov A character string indicating the random covariance structure to use
##' @param model An optional argument, can pass an existing model, the output from
##'   mplusModeler()..
##' @import data.table
##' @export
##' @examples
##' ## Make me!
long2LGMM <- function(data, idvar, assessmentvar, dv, timevars, misstrick = TRUE, k = 1L,
                      title = "Trajectory Model", base = "trajmodel_", run = FALSE,
                      processors = 1L, starts = "500 100", newdata,
                      cov = c("un", "independent", "intercept", "zero"), model) {

  stopifnot(is.data.frame(data))

  if (any(nchar(timevars)>7)) {
    stop("Please specify time variables with names 7 or fewer characters")
  }

  stopifnot(is.integer(k))
  stopifnot(k > 0 && k < 10)
  stopifnot(is.integer(processors))

  if (!missing(newdata) & !identical(newdata, FALSE)) {
    stopifnot(all(timevars %in% names(newdata)))
    stopifnot(is.data.frame(newdata))
    newdata <- newdata
    predict <- TRUE
  }

  ## ensure data table object and select only relevant variables
  data <- data[, c(idvar, assessmentvar, dv, timevars)]

  ## assessment labels, used for wide data
  assesslabs <- unique(data[[assessmentvar]])
  j <- length(assesslabs)

  expandd <- expand.grid(ID = unique(data[[idvar]]), Assessment = assesslabs)
  names(expandd) <- c(idvar, assessmentvar)

  ## expand long data to be 'complete'
  data <- merge(data, expandd, by = c(idvar, assessmentvar), all = TRUE)

  ## fix missing relying on the fact that if missing DV it will not be used
  ## but if missing IV the whole case will be excluded (from Linda Muthen)
  ## therefore if time is missing but not DV, set DV to missing
  ## if DV is missing, set time to any (non missing) value so those whole cases
  ## are not excluded from analysis
  if (misstrick) {
    missing_time <- rowSums(is.na(data[, timevars, drop=FALSE])) > 0

    data[[dv]] <- ifelse(missing_time, NA, data[[dv]])

    for (i in seq_along(timevars)) {
      data[[timevars[i]]] <- ifelse(is.na(data[[dv]]), -99, data[[timevars[i]]])
    }
  }

  ## reshape the data to wide form
  dataw <- reshape(data,
                   timevar = assessmentvar,
                   idvar = idvar,
                   direction = "wide",
                   sep="")

  w.dv <- paste0(dv, assesslabs)
  w.timevars <- lapply(timevars, paste0, assesslabs)
  wu.timevars <- unlist(w.timevars)

  dataw <- dataw[, c(idvar, w.dv, unlist(w.timevars))]

  overall.model <- paste0(paste(unlist(lapply(seq_along(timevars), function(i) {
    sprintf("i%s s%s | %s-%s at %s-%s;%s",
            i, i, w.dv[1], w.dv[j],
            w.timevars[[i]][1], w.timevars[[i]][j],
            ifelse(i > 1, sprintf("\n[i%s@0]; i%s@0;", i, i), ""))
  })), collapse = "\n"), sprintf("\n[%s-%s@0];", w.dv[1], w.dv[j]))

  cov <- match.arg(cov)
  covtype <- switch(cov,
                    un = "un",
                    independent = "heterogenous",
                    intercept = "intercept",
                    zero = "zero")

  class.model <- paste(
    if(k==1) "" else "%c#%k%%",
    sprintf("%s-%s (e%%k%%);", w.dv[1], w.dv[j]),
    if (covtype == "zero") {
      paste(paste0(c("i1", paste0("s", seq_along(timevars))), "@0;"), collapse = "\n")
    } else if (covtype == "intercept") {
      paste(paste0(paste0("s", seq_along(timevars)), "@0;"), collapse = "\n")
    } else {
      mplusRcov(c("i1", paste0("s", seq_along(timevars))), covtype, collapse = FALSE)$all
    },
    sep = "\n")

  class.model.complete <- paste(sapply(1:k, function(i) gsub("%k%", i, class.model)), collapse = "\n")

  if (missing(model)) {

    m <- mplusModeler(mplusObject(
      TITLE = title,
      VARIABLE = sprintf(
        gsub("%k%", k,
             "%sCLASSES = c(%k%); \nIDVARIABLE = %s; \nTSCORES = %s - %s;"),
        if(k==1) "!" else "",
        idvar,
        wu.timevars[1],
        wu.timevars[length(wu.timevars)]),

      ANALYSIS = sprintf(
        gsub("%starts%", starts, gsub("%k%", processors,
          "TYPE = RANDOM %s; \nESTIMATOR = MLR; \n%sSTARTS = %starts%; \nPROCESSORS = %k%;")),
        if(k==1) "" else "MIXTURE",
        if(k==1) "!" else ""),

      MODEL = sprintf(
        if(k==1) "%s\n%s" else "%%OVERALL%%\n%s\n%s",
        overall.model,
        class.model.complete),
      OUTPUT = "STDYX; CINTERVAL;",
      SAVEDATA = if(k==1) NULL else sprintf(gsub("%k%", k,
        "FILE =  %s%k%_probs.txt ; \nSAVE = cprob; \nFORMAT = free;"),
        base),
    PLOT = sprintf("TYPE = PLOT3;\n SERIES = %s;",
                   paste(paste0(w.dv, " (", seq_along(assesslabs), ")"), collapse = "\n")),
    usevariables = colnames(dataw),
    rdata = dataw), paste0(base, k, ".dat"), run = run)
  } else {
    m <- model
  }

  ## extract the intercept and slope parameters
  x <- as.data.table(coef(m$results, param = "exp"))

  if (k==1) {
    prefix <- ""
  } else {
    prefix <- "C_[1-9]"
  }

  ## only get I and S parameter means
  x <- x[grepl(paste0(prefix, " [IS][1-9]<-Means"), Label)]
  ## exclude the 'redundant' intercepts
  x <- x[!grepl(paste0(prefix, " [I][2-9]<-Means"), Label)]

  if (k==1) {
    x[, Class := 1L]
  } else {
    x[, Class := as.integer(gsub("C_([1-9]).*", "\\1", Label))]
  }

  x[, Type := gsub(paste0(prefix, " ([IS][1-9])<-Means"), "\\1", Label)]
  x[, Type := ifelse(Type == "I1", "Intercept", Type)]

  if (k==1) {
    classcounts <- data.frame(
      class = 1L,
      count = m$results$summaries$Observations,
      proportion = 1)
  } else {
    classcounts <- m$results$class_counts$mostLikel
  }

  x <- merge(x, classcounts, by.x = "Class", by.y = "class")

  if (predict) {
    for (i in 1:k) {

      b <- as.matrix(x[Class == i & Type != "Intercept"][order(Type), est])
      X <- as.matrix(newdata[, timevars, with = FALSE])

      newdata[, (paste0(dv, "_", i)) :=
                  as.numeric(x[Class == i & Type == "Intercept", est] + X %*% b)]
    }
  } else {
    newdata <- NULL
  }

  return(list(
    Model = m,
    estimates = x,
    predictions = newdata))
}


##' Train a variety of latent growth mixture model
##'
##' This function iterates through a grid of values to train LGMMs,
##' optionally using a local or remote cluster.
##'
##' @param data A data frame or data table in long format (i.e., multiple rows per ID).
##' @param idvar A character string of the variable name in the dataset
##'   that is the ID variable.
##' @param assessmentvar A character string of the variable name in the dataset that
##'   indicates the particular assessment point for each timepoint.
##' @param newdata A data frame of new values to use for generating predicted
##'   trajectories by class or \code{FALSE} if no predictions to be made
##'   (the default).
##' @param tuneGrid A dataframe, data.table, or list.  It should have names for
##'   the needed arguments for \code{long2LGMM()}.
##' @export
##' @examples
\dontrun{
##' ## This example is not run by default because even with very limitted number of
##' ## random starts and iterations, it can take several minutes
##' setwd(tempdir())
##'
##' set.seed(1234)
##' dat <- do.call(rbind, lapply(1:2, function(i) {
##'   do.call(rbind, lapply(1:200, function(x) {
##'     d <- data.frame(
##'       ID = as.integer(paste0(i, x)),
##'       Class = i,
##'       Assessment = as.character(1:8),
##'       t1 = runif(8, 0, 12),
##'       stringsAsFactors = FALSE)
##'     u <- rnorm(1, 0, i - .5)
##'     b1 <- rnorm(1, 0, .02)
##'     b2 <- rnorm(1, 0, .02)
##'     d$y <- 5*i + u + rnorm(8, (1 + b1) * d$t1 - ((.3 + b2) * d$t1^2), sd = 1)
##'     return(d)
##'   }))
##' }))
##'
##' dat$t2 <- dat$t1^2
##'
##' newdat <- data.frame(
##'   ID = 11L,
##'   t1 = seq(0, 12, by = .1),
##'   t2 = seq(0, 12, by = .1)^2)
##'
##' tuneGrid <- expand.grid(
##'   dv = "y",
##'   timevars = list("t1", c("t1", "t2")),
##'   starts = "2 1",
##'   cov = c("independent", "zero"),
##'   k = 1L:2L,
##'   processors = 1L, run = TRUE,
##'   misstrick = TRUE, stringsAsFactors = FALSE)
##'
##' tuneGrid$title <- paste0(
##'   c("linear", "quadratic")[sapply(tuneGrid$timevars, length)],
##'   "_",
##'   sapply(tuneGrid$cov, function(x) if(nchar(x)==4) substr(x, 1, 4) else substr(x, 1, 3)),
##'   "_",
##'   tuneGrid$k)
##'
##' tuneGrid$base <- paste0(
##'   c("linear", "quad")[sapply(tuneGrid$timevars, length)],
##'   "_",
##'   sapply(tuneGrid$cov, function(x) if(nchar(x)==4) substr(x, 1, 4) else substr(x, 1, 3)))
##'
##' bigAllRes <- trainLGMM(
##'   data = dat,
##'   idvar = "ID",
##'   assessmentvar = "Assessment",
##'   newdata = newdat,
##'   tuneGrid = tuneGrid,
##'   ncores = 1L)
##' }
trainLGMM <- function(data, idvar, assessmentvar, newdata = FALSE, tuneGrid, cl, ncores = 1L) {
  stopifnot(is.integer(ncores))

  if (ncores > 1L) {

    if (missing(cl)) {
      cl <- makeCluster(ncores)
      on.exit(stopCluster(cl))
    }

    clusterEvalQ(cl, {
      library(MplusAutomation)
    })

    e <- environment()

    clusterExport(cl,
                  c("data", "idvar", "assessmentvar",
                    "newdata", "tuneGrid",
                    "long2LGMM"),
                  envir = e)

    bigRes <- parLapplyLB(cl, 1:nrow(tuneGrid), function(i) {
      tryCatch(long2LGMM(
        data = data,
        idvar = idvar,
        assessmentvar = assessmentvar,
        dv = tuneGrid$dv[i],
        timevars = tuneGrid$timevars[[i]],
        misstrick = tuneGrid$misstrick[i],
        k = tuneGrid$k[i],
        title = paste0(tuneGrid$title[i], tuneGrid$k[i]),
        base = tuneGrid$base[i],
        run = tuneGrid$run[i],
        processors = tuneGrid$processors[i],
        starts = tuneGrid$starts[i],
        newdata = newdata,
        cov = tuneGrid$cov[i]), error = function(e) e)
    })
  } else if (ncores == 1L) {
    bigRes <- lapply(1:nrow(tuneGrid), function(i) {
      tryCatch(long2LGMM(
        data = data,
        idvar = idvar,
        assessmentvar = assessmentvar,
        dv = tuneGrid$dv[i],
        timevars = tuneGrid$timevars[[i]],
        misstrick = tuneGrid$misstrick[i],
        k = tuneGrid$k[i],
        title = paste0(tuneGrid$title[i], tuneGrid$k[i]),
        base = tuneGrid$base[i],
        run = tuneGrid$run[i],
        processors = tuneGrid$processors[i],
        starts = tuneGrid$starts[i],
        newdata = newdata,
        cov = tuneGrid$cov[i]), error = function(e) e)
    })
  }
  return(bigRes)
}


