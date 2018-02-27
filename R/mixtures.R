#' Create a summary table of Mplus mixture models
#'
#' Creates a summary table of model fit statistics and relevant diagnostic
#' information for a list of mixture models. Default statistics reported are in
#' line with published guidelines (see Jung & Wickrama, 2008; Nylund et al.,
#' 2007): \code{c("Title", "Classes", "Warnings", "AIC", "BIC", "aBIC",
#' "Entropy", "T11_VLMR_PValue", "T11_LMR_PValue", "BLRT_PValue", "min_N",
#' "max_N", "min_prob", "max_prob")}. The table is customizable using the
#' \code{keepCols} parameter, which is passed through to \link{SummaryTable}.
#' @param modelList A list of models returned from the
#' \code{extractModelSummaries} function.
#' @param keepCols A vector of character strings indicating which
#' columns/variables to display in the summary. Only columns included in this
#' list will be displayed (all others excluded). By default, \code{keepCols} is:
#' \code{c("Title", "Classes", "Warnings", "AIC", "BIC", "aBIC","Entropy",
#' "T11_VLMR_PValue", "T11_LMR_PValue", "BLRT_PValue", "min_N", "max_N",
#' "min_prob", "max_prob")}.
#' @return An object of class data.frame.
#' @author Caspar J. van Lissa
#' @note This function is partially a wrapper around SummaryTable, with
#' enhancements for summarizing mixture models.
#' @seealso \code{\link{SummaryTable}}
#' @export
#' @keywords mixture mplus
#' @examples
#' createMixtures(classes = 1:4, filename_stem = "iris", rdata = iris)
#' runModels(filefilter = "iris")
#' results <- readModels(filefilter = "iris")
#' mixtureSummaryTable(results)
#' createMixtures(classes = 1:2, filename_stem = "iris", rdata = iris,
#'                OUTPUT = "tech11 tech14;")
#' runModels(filefilter = "iris", replaceOutfile = "modifiedDate")
#' results <- readModels(filefilter = "iris")[c(1:2)]
#' mixtureSummaryTable(results)
mixtureSummaryTable <- function(modelList,
                                keepCols = c(
                                  "Title",
                                  "Classes",
                                  "Warnings",
                                  "AIC",
                                  "BIC",
                                  "aBIC",
                                  "Entropy",
                                  "T11_VLMR_PValue",
                                  "T11_LMR_PValue",
                                  "BLRT_PValue",
                                  "min_N",
                                  "max_N",
                                  "min_prob",
                                  "max_prob"
                                )) {
  # Check if modelList is a list of (mixture) models
  if (!inherits(modelList, "mplus.model.list"))
    if (!all(sapply(modelList, function(x) {
      inherits(x, "mplus.model")
    })))
      stop("mixtureSummaryTable requires a list of mixture models as its first argument.")
  # Check if all models in the list are mixture models
  mixtures <- sapply(modelList, function(x) {
    !is.null(x$input$analysis[["type"]])
  })
  mixtures[mixtures] <- sapply(modelList[mixtures], function(x) {
    x$input$analysis$type == "mixture"
  })
  if (!any(mixtures))
    stop("mixtureSummaryTable requires a list of mixture models as its first argument.")
  if (any(!mixtures))
    warning(
      "Some output files were excluded because they are not mixture models; specifically: ",
      paste(names(mixtures)[which(!mixtures)], collapse = ", ")
    )
  
  # Remove models which are not type "mixture"
  modelList <- modelList[which(mixtures)]
  # In the unlikely case that the user deleted keepCols, set to default
  if (is.null(keepCols)) {
    keepCols <-
      c(
        "Title",
        "Classes",
        "AIC",
        "BIC",
        "aBIC",
        "Entropy",
        "T11_VLMR_PValue",
        "T11_LMR_PValue",
        "BLRT_PValue",
        "min_N",
        "max_N",
        "min_prob",
        "max_prob"
      )
  }
  # Extract class diagnostics
  model_summaries <- data.frame(
    sapply(modelList, function(x) {
      x$summaries$Filename
    }),
    sapply(modelList, function(x) {
      ifelse(
        is.null(x$class_counts$modelEstimated),
        NA,
        nrow(x$class_counts$modelEstimated)
      )
    }),
    t(sapply(modelList, function(x) {
      tryCatch(
        range(x$class_counts$mostLikely[, 2]),
        warning = function(x) {
          c(NA, NA)
        }
      )
    })),
    t(sapply(modelList, function(x) {
      tryCatch(
        range(diag(
          x$class_counts$classificationProbs.mostLikely
        )),
        warning = function(x) {
          c(NA, NA)
        }
      )
    })),
    row.names = NULL
  )
  names(model_summaries) <-
    c("Filename",
      "Classes",
      "min_N",
      "max_N",
      "min_prob",
      "max_prob")
  # Extract model summaries
  summarytable_keepCols <- unique(c("Filename",
                                    keepCols[which(!keepCols %in% c("min_N", "max_N", "min_prob", "max_prob"))]))
  if (length(summarytable_keepCols > 0)) {
    model_summaries <-
      merge(
        model_summaries,
        SummaryTable(
          modelList = modelList,
          keepCols = summarytable_keepCols,
          type = "none"
        ),
        by = "Filename"
      )
  }
  
  model_summaries <-
    model_summaries[order(model_summaries[["Classes"]]),
                    keepCols[which(keepCols %in% names(model_summaries))],
                    drop = FALSE]
  if (any(!(
    c("T11_VLMR_PValue", "T11_LMR_PValue", "BLRT_PValue") %in% names(model_summaries)
  ))) {
    warning(
      "To determine the correct number of classes, it is recommended to examine the BLRT, LMR, or VLMR tests. These can be obtained by specifying 'OUTPUT = \"TECH11 TECH14;\"' when calling createMixtures.",
      call. = FALSE
    )
  }
  
  not_replicated <- which(sapply(modelList, function(x) {
    length(grep(
      "BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",
      paste(unlist(x$warnings), collapse = "")
    ))
  }) > 0)
  if (length(not_replicated) > 0) {
    warning(
      "The best loglikelihood value was not replicated in some models. The solution may not be trustworthy due to local maxima. Increase the number of random starts. The problematic models were:\n",
      paste(names(not_replicated), collapse = "\n"),
      call. = FALSE
    )
  }
  modelList$iris_4_class.out$errors
  not_terminated <- which(sapply(modelList, function(x) {
    length(grep(
      "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY",
      paste(unlist(x$errors), collapse = "")
    ))
  }) > 0)
  if (length(not_terminated) > 0) {
    warning(
      "Model estimation did not terminate normally due to an error in the computation. Change your model and/or starting values. The problematic models were:\n",
      paste(names(not_terminated), collapse = "\n"),
      call. = FALSE
    )
  }
  
  return(model_summaries)
}

#' Create latent profile plots
#'
#' Creates a profile plot for a single object of class 'mplus.model', or a
#' faceted plot of profile plots for an object of class 'mplus.model.list'.
#' @param modelList A list object of Mplus models, or a single Mplus model
#' @param coefficients Which type of coefficients to plot on the y-axis; default
#' is 'unstandardized'. Options include: c('stdyx.standardized',
#' 'stdy.standardized', 'std.standardized')
#' @param parameter Which parameter to plot (from Mplus parameter estimates).
#' Defaults to 'Means'.
#' @param ci What confidence interval should the errorbars span? Defaults to
#' a 95\% confidence interval. Set to NULL to remove errorbars.
#' @param bw Logical. Should the plot be black and white (for print), or color?
#' @param rawdata Should raw data be plotted in the background? Setting this to
#' TRUE might result in long plotting times. Requires including the Mplus syntax
#' 'SAVEDATA: FILE IS "filename"; SAVE = cprobabilities' in the Mplus input.
#' @param alpha_range The minimum and maximum values of alpha (transparancy) for
#' the raw data. Minimum should be 0; lower maximum values of alpha can help
#' reduce overplotting.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @keywords plot mixture mplus
#' @examples
#' createMixtures(classes = 1:4, filename_stem = "cars",
#'                model_overall = "wt ON drat;",
#'                model_class_specific = "wt;  qsec;",
#'                rdata = mtcars,
#'                usevariables = c("wt", "qsec", "drat"),
#'                OUTPUT = "standardized")
#' runModels(replaceOutfile = "modifiedDate")
#' cars_results <- readModels(filefilter = "cars")
#' plotMixtures(cars_results, rawdata = TRUE)
#' \dontrun{
#' plotMixtures(cars_results, variables = "wt")
#' }
#' \dontrun{
#' plotMixtures(cars_results, coefficients = "stdyx.standardized")
#' }
plotMixtures <- function(modelList,
                         variables = NULL,
                         coefficients = c("unstandardized",
                                          "stdyx.standardized",
                                          "stdy.standardized",
                                          "stdy.standardized"),
                         parameter = c("Means", "Intercepts"),
                         ci = .95,
                         bw = FALSE,
                         rawdata = FALSE,
                         alpha_range = c(0, .1))
{
  # Check if mplusModel is of class mplus.model
  if (!(inherits(modelList, "mplus.model") |
        all(sapply(modelList, function(x) {
          inherits(x, "mplus.model")
        })))) {
    stop(
      "plotMixtures requires an object of class 'mplus.model' or a list of mplus.models as its first argument."
    )
  }
  if (inherits(modelList, "mplus.model")) {
    modelList <- list(Model_1 = modelList)
  }
  # Check if mplusModel is a mixture model
  if (inherits(modelList, "mplus.model")) {
    modelList <- list(Model_1 = modelList)
  }
  mixtures <- sapply(modelList, function(x) {
    !is.null(x$input$analysis[["type"]])
  })
  mixtures[which(mixtures)] <-
    sapply(modelList[which(mixtures)], function(x) {
      x$input$analysis$type == "mixture"
    })
  if (!any(mixtures))
    stop(
      "plotMixtures requires a list of mixture models, or one mixture model, as its first argument."
    )
  if (any(!mixtures))
    warning(
      "Some output files were excluded because they are not mixture models; specifically: ",
      paste(names(mixtures)[which(!mixtures)], collapse = ", "),
      call. = FALSE
    )
  # Remove models which are not type "mixture"
  modelList <- modelList[which(mixtures)]
  # Check if all models were run on the same dataset
  if (length(unique(sapply(modelList, function(x) {
    x$input$data$file
  }))) > 1) {
    stop("Models were not all run on the same data file.")
  }
  # Check if any models have missing columns (result of nonconvergence)
  missing_cols <-
    sapply(modelList, function(x) {
      length(names(x$parameters[[coefficients]]))
    })
  missing_cols <- which(missing_cols != max(missing_cols))
  if (length(missing_cols) > 0) {
    warning(
      "Some models had missing columns in the coefficients section. This likely indicates a convergence problem. These models were dropped: ",
      paste(names(modelList)[missing_cols], collapse = ", "),
      call. = FALSE
    )
    modelList <- modelList[-missing_cols]
  }
  # Prepare plot data
  # Get coefficients
  if (length(coefficients) > 1)
    coefficients <- coefficients[1]
  missing_coefficients <-
    which(sapply(modelList, function(x) {
      is.null(x$parameters[[coefficients]])
    }))
  if (length(missing_coefficients > 0)) {
    warning(
      "Some models were missing the ",
      coefficients,
      " coefficients. Please request these coefficients from Mplus.",
      call. = FALSE,
      immediate. = TRUE
    )
    modelList <- modelList[-missing_coefficients]
  }
  if (length(modelList) < 1)
    stop("No data left to plot.", call. = FALSE)
  plotdat <-
    lapply(modelList, function(x) {
      x$parameters[[coefficients]]
    })
  # Bind into one df with identifying variable
  plotdat <- do.call(rbind, lapply(names(modelList), function(x) {
    data.frame(Title = modelList[[x]]$input$title, plotdat[[x]])
  }))
  
  # Select the requested parameter
  plotdat <- plotdat[plotdat$paramHeader %in% parameter, ]
  # Drop useless stuff
  plotdat <-
    subset(
      plotdat,
      subset = !grepl("#", param),
      select = -c(paramHeader, est_se, pval)
    )
  
  # Select only requested variables, or else, all variables
  if (is.null(variables)) {
    variables <- unique(plotdat$param)
  } else {
    plotdat <- plotdat[tolower(plotdat$param) %in% tolower(variables), ]
  }
  # Get some classy names
  names(plotdat)[which(names(plotdat) %in% c("param", "est", "LatentClass"))] <-
    c("Variable", "Value", "Class")
  plotdat$Variable <- factor(tolower(plotdat$Variable))
  # Basic plot
  if (bw) {
    classplot <-
      ggplot2::ggplot(NULL,
                      aes(
                        x = Variable,
                        y = Value,
                        group = Class,
                        linetype = Class,
                        shape = Class
                      ))
  } else {
    classplot <-
      ggplot2::ggplot(
        NULL,
        aes(
          x = Variable,
          y = Value,
          group = Class,
          linetype = Class,
          shape = Class,
          colour = Class
        )
      )
  }
  
  if (rawdata) {
    if (coefficients != "unstandardized") {
      warning(
        "Plotting raw data only makes sense when inspecting the unstandardized coefficients.",
        call. = FALSE
      )
    } else {
      missing_savedata <-
        sapply(modelList, function(x) {
          is.null(x[["savedata"]])
        })
      if (any(missing_savedata)) {
        warning(
          "Could not plot raw data because",
          ifelse(all(missing_savedata), "all models", "some models"),
          " were missing savedata. Make sure to specify 'SAVEDATA = FILE IS filename.dat; SAVE = cprobabilities;' in the Mplus syntax.",
          call. = FALSE
        )
      } else {
        # Check if all variables (except CPROBs) are identical across models
        var_names <-
          sapply(modelList, function(x) {
            names(x$savedata)[-c(which(names(x$savedata) == "C"), grep("^CPROB", names(x$savedata)))]
          })
        if (!is.matrix(var_names)) {
          var_names <- table(unlist(var_names))
          warning(
            "Savedata variables are not identical across Mplus output files. Dropped the following variables: ",
            paste(names(var_names)[which(var_names != max(var_names))], collapse = ", ")
          )
          var_names <-
            matrix(names(var_names)[which(var_names == max(var_names))], ncol = 1)
        }
        
        raw.data <-
          lapply(modelList, function(x) {
            subset(x$savedata,
                   select = c(
                     names(x$savedata)[which(levels(plotdat$Variable) %in% tolower(names(x$savedata)))],
                     grep("^CPROB", names(x$savedata), value = TRUE)
                   ))
          })
        
        raw.data <- lapply(raw.data, function(x) {
          names(x) <- gsub("^CPROB", "Probability.", names(x))
          subset(reshape(
            x,
            direction = "long",
            varying = grep("^Probability", names(x)),
            timevar = "Class"
          ),
          select = -id)
        })
        
        raw.data <-
          do.call(rbind, lapply(names(modelList), function(x) {
            data.frame(Title = modelList[[x]]$input$title, raw.data[[x]])
          }))
        
        
        
        names(raw.data)[which(!names(raw.data) %in% c("Title", "Class", "Probability"))] <-
          paste0("Value.", names(raw.data)[which(!names(raw.data) %in% c("Title", "Class", "Probability"))])
        raw.data <- reshape(
          raw.data,
          direction = "long",
          varying = grep("^Value", names(raw.data)),
          timevar = "Variable"
        )[, c("Title", "Class", "Probability", "Variable", "Value")]
        
        raw.data$Variable <- factor(tolower(raw.data$Variable))
        if (!all(levels(raw.data$Variable) %in% levels(plotdat$Variable))) {
          try_to_order <-
            pmatch(levels(raw.data$Variable),
                   levels(plotdat$Variable))
          if (any(is.na(try_to_order)))
            stop(
              "Could not match Mplus parameter names to savedata variable names. This typically happens when variable names exceed 8 characters."
            )
          warning(
            "Mplus parameter names were not identical to savedata variable names. This typically happens when variable names exceed 8 characters. Tried to match savedata names to Mplus parameter names."
          )
          levels(raw.data$Variable) <-
            levels(plotdat$Variable)[try_to_order]
        }
        raw.data$Class <- ordered(raw.data$Class)
        
        #raw.data$Probability <- ((raw.data$Probability - min(raw.data$Probability))/max(raw.data$Probability))
        
        classplot <- classplot +
          geom_jitter(
            data = raw.data,
            width = .2,
            aes(
              x = Variable,
              y = Value,
              shape = Class,
              alpha = Probability
            )
          ) +
          scale_alpha_continuous(range = alpha_range, guide = FALSE)
      }
    }
  }
  classplot <- classplot + geom_point(data = plotdat) +
    geom_line(data = plotdat) +
    theme_bw()
  # Add errorbars
  if (!is.null(ci)) {
    ci <- qnorm(.5 * (1 - ci))
    classplot <-
      classplot + geom_errorbar(data = plotdat, aes(ymin = (Value - (ci * se)), ymax =
                                                      (Value + (ci * se))), width = .2)
  }
  # If modelList is really a list, facet_wrap the plots by input file
  if (length(modelList) > 1) {
    classplot <- classplot + facet_wrap(~ Title)
  }
  return(classplot)
}

#' Plot growth mixture models
#'
#' Plots latent and observed trajectories for a list of growth mixture models,
#' specified using Mplus' '|' notation (e.g., i s | ).
#' @param modelList A list object of Mplus models, or a single Mplus model. This
#' function additionally requires the models to be growth mixture models.
#' @param bw Logical. Should the plot be black and white (for print), or color?
#' @param rawdata Logical. Should raw data (observed trajectories) be plotted in the
#' background? Setting this to TRUE might result in long plotting times.
#' Requires including the Mplus syntax 'SAVEDATA: FILE IS "filename"; SAVE =
#' cprobabilities' in the Mplus input.
#' @param estimated Logical. Should the Mplus estimates growth trajectories be
#' displayed? Defaults to TRUE.
#' @param poly Logical. Should polynomial smooth lines be displayed for each
#' group? Defaults to FALSE. If set to TRUE, the order of the polynomial is
#' determined by taking the number of observed timepoints - 1 (e.g., 3 time
#' points results in a quadratic polynomial). The data are weighted according
#' to their posterior class probabilities. Note that rawdata must be TRUE in
#' order to obtain polynomial smooth lines, because these are calculated on the
#' raw data.
#' @param alpha_range Numeric vector. The minimum and maximum values of alpha (transparancy) for
#' the raw data. Minimum should be 0; lower maximum values of alpha can help
#' reduce overplotting.
#' @param linear_time Character. Indicate the name of the coefficient which encodes linear
#' time. Commonly specified as 'S' (e.g., i s | ), but your syntax might differ.
#' @param time_scale Numeric vector. In case some of the loadings of the growth model are freely
#' estimated, provide the correct time scale here (e.g., c(0, 1, 2)).
#' @param jitter_lines Numeric. Indicate the amount (expressed in fractions of a
#' standard deviation of all observed data) by which the observed trajectories
#' should be vertically jittered. Like alpha_range, this parameter helps control
#' overplotting.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @keywords internal
#' @examples
#' createMixtures(classes = 1:4, filename_stem = "cars",
#'                model_class_specific = "wt;  qsec;",
#'                rdata = mtcars,
#'                usevariables = c("wt", "qsec"),
#'                OUTPUT = "standardized")
#' runModels(replaceOutfile = "modifiedDate")
#' cars_results <- readModels(filefilter = "cars")
#' plotMixtures(cars_results, rawdata = TRUE)
#' \dontrun{
#' library(RCurl)
#' myfile <- getURL('http://statmodel.com/usersguide/chap8/ex8.2.dat',
#'                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
#' mydat <- read.table(textConnection(myfile), header = FALSE)[,-6]
#' createMixtures(classes = 1:3, filename_stem = "ex8.2",
#'                model_overall = "i s | V1@0 V2@1 V3@2 V4@3;  i s on V5;",
#'                rdata = mydat,
#'                OUTPUT = "tech11 tech14;", usevariables = c("V1", "V2", "V3",
#'                                                            "V4", "V5"))
#' runModels(replaceOutfile = "modifiedDate")
#' mplus_output <- readModels(filefilter = "ex8.2")
#' plotGrowthMixtures(mplus_output, estimated = TRUE, rawdata = TRUE)
#' plotGrowthMixtures(mplus_output, estimated = FALSE, rawdata = TRUE,
#'                   poly = TRUE)
#'
#' createMixtures(classes = 1:3, filename_stem = "ex8.2_free",
#'                model_overall = "i s | V1@0 V2* V3* V4@3;  i s on V5;",
#'                rdata = mydat,
#'                OUTPUT = "tech11 tech14;", usevariables = c("V1", "V2", "V3",
#'                                                            "V4", "V5"))
#' runModels(replaceOutfile = "modifiedDate")
#' mplus_output_free <- readModels(filefilter = "ex8.2_free")
#' plotMixtureDensities(mplus_output_free, variables = c("V1", "V2", "V3",
#'                                                       "V4"))
#' plotGrowthMixtures(mplus_output_free, estimated = TRUE, rawdata = TRUE,
#'                   bw = TRUE, time_scale = c(0, 1, 2, 3),
#'                   alpha_range = c(0, .05))
#'
#' plotGrowthMixtures(mplus_output, estimated = FALSE, rawdata = TRUE,
#'                   poly = TRUE, bw = TRUE, time_scale = c(0, 1, 2, 3))
#' }
plotGrowthMixtures <-
  function(modelList,
           bw = FALSE,
           rawdata = FALSE,
           estimated = TRUE,
           poly = FALSE,
           alpha_range = c(0, .1),
           linear_time = "S",
           time_scale = NULL,
           jitter_lines = NULL,
           coefficients = "unstandardized") {
    # Check if mplusModel is of class mplus.model
    if (!(inherits(modelList, "mplus.model") |
          all(sapply(modelList, function(x) {
            inherits(x, "mplus.model")
          })))) {
      stop(
        "plotGrowthMixtures requires an object of class 'mplus.model' or a list of mplus.models as its first argument."
      )
    }
    if (inherits(modelList, "mplus.model")) {
      modelList <- list(Model_1 = modelList)
    }
    mixtures <- sapply(modelList, function(x) {
      !is.null(x$input$analysis[["type"]])
    })
    mixtures[which(mixtures)] <-
      sapply(modelList[which(mixtures)], function(x) {
        x$input$analysis$type == "mixture"
      })
    if (!any(mixtures))
      stop(
        "plotMixtures requires a list of mixture models, or one mixture model, as its first argument."
      )
    if (any(!mixtures))
      warning(
        "Some output files were excluded because they are not mixture models; specifically: ",
        paste(names(mixtures)[which(!mixtures)], collapse = ", "),
        call. = FALSE
      )
    # Remove models which are not type "mixture"
    modelList <- modelList[which(mixtures)]
    # Check if all models were run on the same dataset
    if (length(unique(sapply(modelList, function(x) {
      x$input$data$file
    }))) > 1) {
      stop("Models were not all run on the same data file.")
    }
    # Check if any models have missing columns (result of nonconvergence)
    missing_cols <- sapply(modelList, function(x) {
      length(names(x))
    })
    missing_cols <- which(missing_cols != max(missing_cols))
    if (length(missing_cols) > 0) {
      warning(
        "Some models had missing columns in the coefficients section. This likely indicates a convergence problem. These models were dropped: ",
        paste(names(modelList)[missing_cols], collapse = ", "),
        call. = FALSE
      )
      modelList <- modelList[-missing_cols]
    }
    # Prepare plot data
    # Get coefficients
    modelList[[3]]$parameters$stdyxstandardized <-
      modelList[[2]]$parameters$unstandardized
    coefficients <- "unstandardized"
    missing_coefficients <- sapply(modelList, function(x) {
      is.null(x$parameters[[coefficients]])
    })
    if (any(missing_coefficients)) {
      warning(
        "Some models were missing the ",
        coefficients,
        " coefficients. Please request these coefficients from Mplus for models: ",
        paste(names(modelList)[which(missing_coefficients)], collapse = ", "),
        call. = FALSE,
        immediate. = TRUE
      )
      modelList <- modelList[-which(missing_coefficients)]
    }
    if (length(modelList) < 1)
      stop("No data left to plot.", call. = FALSE)
    
    plotdat <-
      lapply(modelList, function(x) {
        x$parameters[[coefficients]]
      })
    
    # Get matrix of loadings
    loadings <-
      lapply(plotdat, function(x) {
        x[grep("\\|", x[x$LatentClass == 1, ]$paramHeader), c("paramHeader", "est", "param", "est_se")]
      })
    # Set time scale
    if (is.null(time_scale) &
        !all(unlist(lapply(loadings, function(x) {
          x$est_se == 999
        })))) {
      stop(
        "Factor loadings freely estimated. Please specify the correct time scale in the argument time_scale."
      )
    }
    growth_variables <-
      table(unlist(lapply(loadings, function(x) {
        x$param
      })))
    if (any(growth_variables != max(growth_variables))) {
      stop("Different variables used for latent growth analyses across models.")
    }
    loadings <- lapply(loadings, function(x) {
      x$paramHeader <- gsub("\\.\\|$", "", x$paramHeader)
      x$paramHeader <-
        ordered(x$paramHeader, levels = unique(x$paramHeader))
      x
    })
    loadings <- lapply(loadings, function(x) {
      sapply(levels(x$paramHeader), function(i) {
        x[x$paramHeader == i, ]$est
      })
    })
    
    # Drop useless stuff
    plotdat <- lapply(names(plotdat), function(x) {
      subset(
        plotdat[[x]],
        select = c(param, est, se, LatentClass),
        subset = (param %in% colnames(loadings[[x]])) &
          (paramHeader %in% c("Means", "Intercepts"))
      )
    })
    
    predicted_trajectories <-
      lapply(1:length(plotdat), function(x) {
        data.frame(t(sapply(unique(plotdat[[x]]$LatentClass), function(class) {
          c(as.numeric(class), colSums(plotdat[[x]][plotdat[[x]]$LatentClass == class,]$est * t(loadings[[x]])))
        })))
      })
    
    predicted_trajectories <-
      lapply(1:length(predicted_trajectories), function(x) {
        names(predicted_trajectories[[x]]) <-
          c("Class", if (is.null(time_scale)) {
            paste0("Value.", loadings[[x]][, linear_time])
          } else {
            paste0("Value.", time_scale)
          })
        reshape(
          predicted_trajectories[[x]],
          direction = "long",
          varying = names(predicted_trajectories[[x]])[-1],
          timevar = "Time"
        )[, c("Class", "Time", "Value")]
      })
    
    predicted_trajectories <-
      do.call(rbind, lapply(1:length(predicted_trajectories), function(x) {
        data.frame(Title = modelList[[x]]$input$title, predicted_trajectories[[x]])
      }))
    predicted_trajectories$Class <-
      ordered(predicted_trajectories$Class)
    predicted_trajectories$Time <-
      as.numeric(predicted_trajectories$Time)
    line_plot <- ggplot(NULL)
    if (!rawdata &
        poly)
      warning("In order to estimate polynomial smooth lines, rawdata must be set to TRUE.")
    if (rawdata) {
      if (any(sapply(modelList, function(x) {
        is.null(x[["savedata"]])
      }))) {
        warning(
          "Could not plot raw data because some models were missing savedata. Make sure to specify 'SAVEDATA = FILE IS filename.dat; SAVE = cprobabilities;' in the Mplus syntax.",
          call. = FALSE
        )
      } else {
        # Check if all variables (except CPROBs) are identical across models
        var_names <-
          sapply(modelList, function(x) {
            names(x$savedata)[-c(which(names(x$savedata) == "C"), grep("^CPROB", names(x$savedata)))]
          })
        if (!is.matrix(var_names)) {
          var_names <- table(unlist(var_names))
          warning(
            "Savedata variables are not identical across Mplus output files. Dropped the following variables: ",
            paste(names(var_names)[which(var_names != max(var_names))], collapse = ", ")
          )
          var_names <-
            matrix(names(var_names)[which(var_names == max(var_names))], ncol = 1)
        }
        
        raw.data <-
          lapply(modelList, function(x) {
            subset(x$savedata,
                   select = c(
                     names(growth_variables),
                     grep("^CPROB", names(x$savedata), value = TRUE)
                   ))
          })
        
        raw.data <-
          lapply(raw.data, function(x) {
            names(x) <- gsub("^CPROB", "Probability.", names(x))
            reshape(
              x,
              direction = "long",
              varying =
                grep("^Probability", names(x), value = TRUE),
              timevar = "Class",
              idvar = "ID"
            )
          })
        
        
        raw.data <- lapply(raw.data, function(x) {
          names(x)[-which(names(x) %in% c("Class", "Probability", "ID"))] <-
            paste0("Value.", names(x)[-which(names(x) %in% c("Class", "Probability", "ID"))])
          reshape(
            x,
            direction = "long",
            varying =
              grep("^Value", names(x), value = TRUE),
            timevar = "Time"
          )[, c("ID", "Time", "Value", "Class", "Probability")]
        })
        raw.data <-
          do.call(rbind, lapply(names(modelList), function(x) {
            data.frame(Title = modelList[[x]]$input$title, raw.data[[x]])
          }))
        
        raw.data$Time <- factor(raw.data$Time)
        
        if (is.null(time_scale)) {
          levels(raw.data$Time) <- loadings[[1]][, linear_time]
          
        } else {
          levels(raw.data$Time) <- time_scale
        }
        raw.data$Time <-
          as.numeric(levels(raw.data$Time))[raw.data$Time]
        raw.data$Class <- ordered(raw.data$Class)
        raw.data$ID <-
          paste(raw.data$Title, raw.data$Class, raw.data$ID, sep = "")
        
        if (bw) {
          line_plot <- line_plot + geom_path(
            data = if (!is.null(jitter_lines)) {
              data.frame(raw.data[,-6],
                         Value = raw.data$Value +
                           rnorm(nrow(raw.data),
                                 sd = (
                                   jitter_lines * sd(raw.data$Value, na.rm = TRUE)
                                 )))
            } else {
              raw.data
            },
            aes(
              x = Time,
              y = Value,
              group = ID,
              linetype = Class,
              alpha = Probability
            )
          ) +
            scale_alpha_continuous(range = alpha_range, guide = FALSE)
        } else {
          line_plot <- line_plot + geom_path(
            data = raw.data,
            aes(
              x = Time,
              y = Value,
              group = ID,
              linetype = Class,
              colour = Class,
              alpha = Probability
            )
          ) +
            scale_alpha_continuous(range = alpha_range, guide = FALSE)
        }
        if (poly) {
          if (bw) {
            line_plot <- line_plot +
              stat_smooth(
                data = raw.data,
                aes(
                  x = Time,
                  y = Value,
                  linetype = Class,
                  weight = Probability
                ),
                method = "lm",
                formula = y ~ poly(x, (nrow(
                  loadings[[1]]
                ) - 1)),
                size = 1,
                colour = "black"
              )
          } else {
            line_plot <- line_plot +
              stat_smooth(
                data = raw.data,
                aes(
                  x = Time,
                  y = Value,
                  colour = Class,
                  linetype = Class,
                  weight = Probability
                ),
                method = "lm",
                formula = y ~ poly(x, (nrow(
                  loadings[[1]]
                ) - 1)),
                size = 1
              )
          }
          
        }
      }
    }
    
    if (estimated) {
      if (bw) {
        line_plot <- line_plot +
          geom_point(
            data = predicted_trajectories,
            aes(
              x = Time,
              y = Value,
              group = Class,
              shape = Class
            ),
            size = 2
          ) +
          geom_line(
            data = predicted_trajectories,
            aes(
              x = Time,
              y = Value,
              group = Class,
              linetype = Class
            ),
            size = 1
          )
      } else {
        line_plot <- line_plot +
          geom_point(
            data = predicted_trajectories,
            aes(
              x = Time,
              y = Value,
              group = Class,
              shape = Class,
              colour = Class
            ),
            size = 2
          ) +
          geom_line(
            data = predicted_trajectories,
            aes(
              x = Time,
              y = Value,
              group = Class,
              linetype = Class,
              colour = Class
            ),
            size = 1
          )
      }
      
    }
    if (length(modelList) > 1)
      line_plot <- line_plot + facet_wrap(~ Title)
    line_plot <- line_plot + theme_bw() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0))
    return(line_plot)
  }

#' Create density plots for mixture models
#'
#' Creates a density plot for a single object of class 'mplus.model', or a
#' faceted plot of density plots for an object of class 'mplus.model.list'. For
#' each variable, a Total density plot will be shown, along with separate
#' density plots for each latent class, where cases are weighted by the
#' posterior probability of being assigned to that class.
#' @param modelList A list object of Mplus models, or a single Mplus model
#' @param variables Which variables to plot. If NULL, plots all variables that
#' are present in all Mplus models.
#' @param bw Logical. Whether to make a black and white plot (for print) or a
#' color plot. Defaults to FALSE, because these density plots are hard to read
#' in black and white.
#' @param conditional Logical. Whether to show a conditional density plot
#' (surface area is divided amongst the latent classes), or a classic density
#' plot (surface area of the total density plot is equal to one, and is
#' subdivided amongst the classes).
#' @param alpha Numeric (0-1). Only used when bw and conditional are FALSE. Sets
#' the transparency of geom_density, so that classes with a small number of
#' cases remain visible.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @note This function returns warnings, indicating that sum(weights) != 1.
#' These can be ignored. The sum of the "Total" density per variable per model
#' is equal to 1, and the sum of all of the posterior probabilities is equal to
#' 1. This results in a normal density plot for the "Total", which is subdivided
#' by the latent classes, in proportion to the posterior probabilities of
#' participants being assigned to those clases.
#' @import ggplot2
#' @export
#' @keywords mixture models mplus
#' @examples
#' createMixtures(classes = 1:4, filename_stem = "iris", rdata = iris)
#' runModels(filefilter = "iris")
#' results <- readModels(filefilter = "iris")
#' plotMixtureDensities(results)
#' \dontrun{
#' plotMixtureDensities(results, variables = "PETAL_LE")
#' }
#' \dontrun{
#' plotMixtureDensities(results, bw = TRUE)
#' }
#' \dontrun{
#' plotMixtureDensities(results, bw = FALSE, conditional = TRUE)
#' }
#' \dontrun{
#' plotMixtureDensities(results[[2]], variables = "PETAL_LE")
#' }
plotMixtureDensities <-
  function(modelList,
           variables = NULL,
           bw = FALSE,
           conditional = FALSE,
           alpha = .2) {
    # Check if mplusModel is of class mplus.model
    if (!(inherits(modelList, "mplus.model") |
          all(sapply(modelList, function(x) {
            inherits(x, "mplus.model")
          })))) {
      stop(
        "plotMixtureDensities requires an object of class 'mplus.model' or a list of mplus.models as its first argument."
      )
    }
    
    # Check if mplusModel is a mixture model
    if (inherits(modelList, "mplus.model")) {
      modelList <- list(Model_1 = modelList)
    }
    mixtures <- sapply(modelList, function(x) {
      !is.null(x$input$analysis[["type"]])
    })
    mixtures[mixtures] <- sapply(modelList[mixtures], function(x) {
      x$input$analysis$type == "mixture"
    })
    if (!any(mixtures))
      stop(
        "plotMixtureDensities requires a list of mixture models, or one mixture model, as its first argument."
      )
    if (any(!mixtures))
      warning(
        "Some output files were excluded because they are not mixture models; specifically: ",
        paste(names(mixtures)[which(!mixtures)], collapse = ", ")
      )
    # Remove models which are not type "mixture"
    modelList <- modelList[which(mixtures)]
    missing_savedata <-
      sapply(modelList, function(x) {
        is.null(x[["savedata"]])
      })
    if (any(missing_savedata)) {
      if (sum(missing_savedata) == length(modelList)) {
        stop(
          "All models are missing savedata. Make sure to specify 'SAVEDATA = FILE IS filename.dat; SAVE = cprobabilities;' in the Mplus syntax.",
          call. = FALSE
        )
      } else{
        warning(
          "Some models are missing savedata and were dropped from the analysis. Make sure to specify 'SAVEDATA = FILE IS filename.dat; SAVE = cprobabilities;' in the Mplus syntax.",
          call. = FALSE
        )
        modelList <- modelList[-which(missing_savedata)]
      }
    }
    # Check if all models were run on the same dataset
    if (length(unique(sapply(modelList, function(x) {
      x$input$data$file
    }))) > 1) {
      stop("Models were not all run on the same data file.")
    }
    # Check if all variables (except CPROBs) are identical across models
    raw.data <- lapply(modelList, function(x) {
      x$savedata
    })
    var_names <-
      sapply(modelList, function(x) {
        names(x$savedata)[-c(which(names(x$savedata) == "C"), grep("^CPROB", names(x$savedata)))]
      })
    if (!class(var_names) == "matrix") {
      var_names <- table(unlist(var_names))
      warning(
        "Savedata variables are not identical across Mplus output files. Dropped the following variables: ",
        paste(names(var_names)[which(var_names != max(var_names))], collapse = ", ")
      )
      var_names <-
        matrix(names(var_names)[which(var_names == max(var_names))], ncol = 1)
    }
    # If no variables have been specified, use all variables
    if (is.null(variables)) {
      variables <- var_names[, 1]
      variables <- variables[!variables %in% "C"]
    } else {
      variables <- variables[which(variables %in% var_names[, 1])]
    }
    raw.data <-
      lapply(modelList, function(x) {
        x$savedata[, which(names(x$savedata) %in% c(grep("^CPROB", names(x$savedata), value = TRUE), variables))]
      })
    raw.data <- lapply(raw.data, function(x) {
      if (length(grep("^CPROB", names(x))) == 1) {
        names(x) <- gsub("^CPROB1", "Probability.Total", names(x))
        x
      } else {
        names(x) <- gsub("^CPROB", "Probability.", names(x))
        data.frame(x, Probability.Total = 1)
      }
      
    })
    
    for (i in names(raw.data)) {
      raw.data[[i]][, grep("^Probability", names(raw.data[[i]]))] <-
        lapply(raw.data[[i]][grep("^Probability", names(raw.data[[i]]))], function(x) {
          x / length(x)
        })
    }
    
    
    raw.data <- lapply(raw.data, function(x) {
      reshape(
        x,
        direction = "long",
        varying =
          grep("^Probability", names(x), value = TRUE),
        timevar = "Class",
        idvar = "ID"
      )
    })
    
    raw.data <-
      do.call(rbind, lapply(names(modelList), function(x) {
        data.frame(Title = modelList[[x]]$input$title, raw.data[[x]])
      }))
    
    names(raw.data)[-which(names(raw.data) %in% c("Title", "Class", "Probability", "ID"))] <-
      paste0("Value.", names(raw.data)[-which(names(raw.data) %in% c("Title", "Class", "Probability", "ID"))])
    raw.data <- reshape(
      raw.data,
      direction = "long",
      varying =
        grep("^Value", names(raw.data), value = TRUE),
      timevar = "Variable"
    )[, c("Title", "Variable", "Value", "Class", "Probability")]
    
    raw.data$Variable <- factor(raw.data$Variable)
    raw.data$Class <- factor(raw.data$Class)
    raw.data$Class <-
      ordered(raw.data$Class, levels = c("Total", levels(raw.data$Class)[-length(levels(raw.data$Class))]))
    # Plot figure
    if (bw) {
      if (conditional) {
        raw.data <- raw.data[-which(raw.data$Class == "Total"),]
        density_plot <-
          ggplot(raw.data,
                 aes(Value, ..count.., fill = Class, weight = Probability)) +
          geom_density(position = "fill") + scale_fill_grey(start = 0.2, end = 0.8)
      } else{
        density_plot <-
          ggplot(raw.data,
                 aes(Value, linetype = Class, weight = Probability)) +
          geom_density()
      }
    } else{
      if (conditional) {
        raw.data <- raw.data[-which(raw.data$Class == "Total"),]
        density_plot <-
          ggplot(raw.data,
                 aes(Value, ..count.., fill = Class, weight = Probability)) +
          geom_density(position = "fill")
      } else{
        density_plot <-
          ggplot(raw.data,
                 aes(
                   Value,
                   fill = Class,
                   colour = Class,
                   weight = Probability
                 )) +
          geom_density(alpha = alpha)
      }
    }
    if (length(modelList) > 1) {
      if (length(variables) > 1) {
        density_plot <- density_plot +
          facet_grid(Title ~ Variable)
      } else {
        density_plot <- density_plot +
          facet_grid( ~ Title)
      }
    } else {
      if (length(variables) > 1) {
        density_plot <- density_plot +
          facet_grid( ~ Variable)
      }
    }
    density_plot <- density_plot +
      theme_bw() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0))
    suppressWarnings(print(density_plot))
    return(invisible(density_plot))
  }

#' Create syntax for a batch of mixture models
#'
#' Dynamically creates syntax for a batch of mixture models, with intelligent
#' defaults. This function is a wrapper around \code{mplusObject}, and
#' additional arguments can be passed to this function using \code{...}.
#' In all arguments to \code{mplusObject}, a double space (\dQuote{  }) is
#' replaced with a newline character. This can be used to obtain nicely
#' formatted Mplus syntax.
#' In the arguments \code{model_class_specific} and \code{SAVEDATA}, the
#' character string \dQuote{\{C\}} is substituted with the correct class number.
#' The character string \dQuote{\{filename_stem\}} is substituted with the
#' filename stem, for example, to name savedata in line with the input files.
#'
#' @param classes A vector of integers, indicating which class solutions to
#' generate. Defaults to 1L. E.g., \code{classes = 1:6},
#' \code{classes = c(1:4, 6:8)}.
#' @param filename_stem Character. A stem for the automatically generated filenames of
#' the syntax and data files.
#' @param model_overall Character. Mplus syntax for the overall model (across
#' classes).
#' @param model_class_specific Character. Mplus syntax for the class-specific
#' model. The character string \dQuote{\{C\}} is substituted with the correct
#' class number, for example to make unique parameter labels for each class.
#' @param rdata Data.frame. An R dataset to be used for the model.
#' @param SAVEDATA Character. Syntax for Mplus' savedata option. Highly
#' recommended when conducting mixture models. The default option will often be
#' adequate.
#' @param ... Additional arguments, passed to \link{mplusObject}, such as syntax
#' for other Mplus options.
#' @return None. Function is used for its side effects (generating syntax).
#' @author Caspar J. van Lissa
#' @seealso \code{\link{mplusObject}}, \code{\link{mplusModeler}}
#' @export
#' @keywords mixture models mplus
#' @examples
#' createMixtures(classes = 1:4, filename_stem = "iris", rdata = iris)
createMixtures <- function(classes = 1L,
                           filename_stem = NULL,
                           model_overall = NULL,
                           model_class_specific = NULL,
                           rdata = NULL,
                           usevariables = NULL,
                           SAVEDATA = "FILE IS {filename_stem}_{C}.dat;  SAVE = cprobabilities;",
                           ...) {
  if (hasArg(MODEL))
    warning(
      "MODEL argument was dropped: createMixtures constructs its own MODEL argument from model_overall and model_class_specific."
    )
  Args <- match.call()
  if (!hasArg(usevariables) & !hasArg(DEFINE)) {
    warning(
      "No usevariables provided, or variables defined. All variables in rdata were used.",
      call. = FALSE
    )
    Args[["usevariables"]] <- names(rdata)
  }
  Args[["MODEL"]] <-
    paste(c("%OVERALL%\n", model_overall, "\n"), collapse = "")
  if (hasArg(ANALYSIS)) {
    Args[["ANALYSIS"]] <-
      paste0("TYPE = mixture;\n", Args[["ANALYSIS"]])
  } else {
    Args[["ANALYSIS"]] <- "TYPE = mixture;\n"
  }
  Args[["SAVEDATA"]] <- SAVEDATA
  char_args <- which(sapply(Args, is.character))
  Args[char_args] <-
    lapply(Args[char_args], function(x) {
      gsub("  ", "\n", x)
    })
  
  n_classes <- length(classes)
  mplusObject_Args <- as.list(Args[-c(1, which(
    names(Args) %in% c(
      "classes",
      "filename_stem",
      "model_overall",
      "model_class_specific"
    )
  ))])
  
  # Create mplusObject template
  base_object <- do.call(mplusObject, mplusObject_Args)
  base_object$SAVEDATA <-
    gsub("\\{filename_stem\\}", filename_stem, base_object$SAVEDATA)
  # Expand template for requested classes
  input_list <- lapply(classes, function(num_classes) {
    base_object$VARIABLE <-
      paste0(base_object$VARIABLE, paste(c("CLASSES = c(", num_classes, ");\n"), collapse = ""))
    if (!is.null(model_class_specific)) {
      base_object$MODEL <- paste0(base_object$MODEL,
                                  do.call(paste0, lapply(1:num_classes, function(this_class) {
                                    gsub("\\{C\\}", this_class, paste(c(
                                      "%c#", this_class, "%\n", Args[["model_class_specific"]], "\n\n"
                                    ),
                                    collapse = ""))
                                  })))
    }
    if (!is.null(SAVEDATA)) {
      base_object$SAVEDATA <-
        gsub("\\{C\\}", num_classes, base_object$SAVEDATA)
    }
    base_object$TITLE <-
      paste(c(base_object$TITLE, num_classes, "classes"), collapse = " ")
    
    base_object
  })
  
  invisible(suppressMessages(lapply(1:n_classes, function(class) {
    mplusModeler(
      object = input_list[[class]],
      dataout = if (is.null(filename_stem)) {
        "data.dat"
      } else {
        paste(c("data_", filename_stem, ".dat"), collapse = "")
      },
      modelout = paste0(paste(
        c(filename_stem, classes[[class]], "class"), collapse = "_"
      ), ".inp"),
      run = 0L,
      check = FALSE,
      varwarnings = TRUE,
      Mplus_command = "Mplus",
      writeData = "ifmissing",
      hashfilename = TRUE
    )
  })))
  
}
