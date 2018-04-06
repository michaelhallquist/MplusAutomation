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
#' \dontrun{
#' createMixtures(classes = 1:3, filename_stem = "iris", rdata = iris)
#' runModels(filefilter = "iris")
#' results <- readModels(filefilter = "iris")
#' mixtureSummaryTable(results)
#' createMixtures(classes = 1:3, filename_stem = "iris", rdata = iris,
#'                OUTPUT = "tech11 tech14;")
#' runModels(filefilter = "iris", replaceOutfile = "modifiedDate")
#' results <- readModels(filefilter = "iris")[c(1:2)]
#' mixtureSummaryTable(results)
#' }
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
  # Check if modelList is of class mplus.model
  if (!(inherits(modelList, "mplus.model") |
        all(sapply(modelList, function(x) {
          inherits(x, "mplus.model")
        })))) {
    stop("mixtureSummaryTable requires a list of mixture models as its first argument.")
  }
  if (inherits(modelList, "mplus.model")) {
    modelList <- list(Model_1 = modelList)
  }
      
  # Check if all models in the list are mixture models
  mixtures <- sapply(modelList, function(x) {
    !is.null(x$input$analysis[["type"]])
  })
  mixtures[mixtures] <- sapply(modelList[mixtures], function(x) {
    tolower(x$input$analysis$type) == "mixture"
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
#' @param modelList A list of Mplus mixture models, or a single mixture model
#' @param variables A character vectors with the names of the variables
#' (included in the Mplus output) to be plotted.
#' @param coefficients Which type of coefficients to plot on the y-axis; default
#' is 'unstandardized'. Options include: c('stdyx.standardized',
#' 'stdy.standardized', 'std.standardized')
#' @param parameter Which parameter to plot (from Mplus parameter estimate
#' headings included in the output).
#' Defaults to c('Means', 'Intercepts').
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
#' @export
#' @import ggplot2
#' @keywords plot mixture mplus
#' @examples
#' \dontrun{
#' createMixtures(classes = 1:4, filename_stem = "cars",
#'                model_overall = "wt ON drat;",
#'                model_class_specific = "wt;  qsec;",
#'                rdata = mtcars,
#'                usevariables = c("wt", "qsec", "drat"),
#'                OUTPUT = "standardized")
#' runModels(replaceOutfile = "modifiedDate")
#' cars_results <- readModels(filefilter = "cars")
#' plotMixtures(cars_results, rawdata = TRUE)
#' }
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
  coefficients <- coefficients[1]
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
      subset(x$parameters[[coefficients]], x$parameters[[coefficients]]$paramHeader %in% parameter)
    })
  # Bind into one df with identifying variable
  plotdat <- do.call(rbind, lapply(names(modelList), function(x) {
    data.frame(Title = modelList[[x]]$input$title, plotdat[[x]])
  }))
  
  # Drop useless stuff
  plotdat <-
    subset(
      plotdat,
      subset = !grepl("#", plotdat$param),
      select = -match(c("paramHeader", "est_se", "pval"), names(plotdat))
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
  plotdat$Variable <- factor(plotdat$Variable)
  levels(plotdat$Variable) <- paste0(toupper(substring(levels(plotdat$Variable), 1, 1)), tolower(substring(levels(plotdat$Variable), 2)))
  # Basic plot
  if (bw) {
    classplot <-
      ggplot(NULL,
                      aes_string(
                        x = "Variable",
                        y = "Value",
                        group = "Class",
                        linetype = "Class",
                        shape = "Class"
                      ))
  } else {
    classplot <-
      ggplot(
        NULL,
        aes_string(
          x = "Variable",
          y = "Value",
          group = "Class",
          linetype = "Class",
          shape = "Class",
          colour = "Class"
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
        
        rawdata <-
          lapply(modelList, function(x) {
            replacethese <- pmatch(tolower(names(x$savedata)), tolower(substring(levels(plotdat$Variable), first = 1, last = 8)))
            names(x$savedata)[which(!is.na(replacethese))] <- levels(plotdat$Variable)[na.omit(replacethese)]
            subset(x$savedata,
                   select = c(
                     names(x$savedata)[which(names(x$savedata) %in% levels(plotdat$Variable))],
                     grep("^CPROB", names(x$savedata), value = TRUE)
                   ))
          })
        # Check if all variables (except CPROBs) are identical across models
        var_names <-
          sapply(rawdata, function(x) {
            names(x)[-c(which(names(x) == "C"), grep("^CPROB", names(x)))]
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
        
        if(!any(levels(plotdat$Variable) %in% var_names[,1])){
          warning("None of the requested variables were found in the Mplus savedata.")
        } else{
          if(any(!(levels(plotdat$Variable) %in% var_names[,1]))){
            warning("Some of the requested variables were not found in the Mplus savedata.")
          }

        rawdata <- lapply(rawdata, function(x) {
          names(x) <- gsub("^CPROB", "Probability.", names(x))
          reshape(
            x,
            direction = "long",
            varying = grep("^Probability", names(x)),
            timevar = "Class"
          )[ , 1:(length(names(x)[-grep("^Probability", names(x))])+2)]
        })
        
        rawdata <-
          do.call(rbind, lapply(names(modelList), function(x) {
            data.frame(Title = modelList[[x]]$input$title, rawdata[[x]])
          }))
        
        names(rawdata)[which(!names(rawdata) %in% c("Title", "Class", "Probability"))] <-
          paste0("Value.", names(rawdata)[which(!names(rawdata) %in% c("Title", "Class", "Probability"))])
        rawdata <- reshape(
          rawdata,
          direction = "long",
          varying = grep("^Value", names(rawdata)),
          timevar = "Variable"
        )[, c("Title", "Class", "Probability", "Variable", "Value")]
        
        rawdata$Variable <- factor(rawdata$Variable)
        levels(rawdata$Variable) <- paste0(toupper(substring(levels(rawdata$Variable), 1, 1)), tolower(substring(levels(rawdata$Variable), 2)))
        rawdata$Class <- ordered(rawdata$Class)
        classplot <- classplot +
          geom_jitter(
            data = rawdata,
            width = .2,
            aes_string(
              x = "Variable",
              y = "Value",
              shape = "Class",
              alpha = "Probability"
            )
          ) +
          scale_alpha_continuous(range = alpha_range, guide = FALSE)
        }
      }
    }
  }
  classplot <- classplot + geom_point(data = plotdat) +
    geom_line(data = plotdat) +
    theme_bw()
  # Add errorbars
  if (!is.null(ci)) {
    ci <- stats::qnorm(.5 * (1 - ci))
    plotdat$error_min <- apply(plotdat[, c("Value", "se")], 1, function(x){
      x[1]-(ci*x[2])
    })
    plotdat$error_max <- apply(plotdat[, c("Value", "se")], 1, function(x){
      x[1]+(ci*x[2])
    })
    classplot <-
      classplot + geom_errorbar(data = plotdat,
                                aes_string(ymin = "error_min", 
                                           ymax = "error_max"),
                                width = .2)
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
#' @param alpha_range Numeric vector. The minimum and maximum values of alpha
#' (transparancy) for the raw data. Minimum should be 0; lower maximum values of
#' alpha can help reduce overplotting.
#' @param growth_variables Character vector. Indicate the names of the latent 
#' variables for the growth trajectory to plot. If NULL (default), all latent
#' growth variables are used. Use this option to plot one trajectory when a
#' model contains multiple latent growth trajectories.
#' @param time_scale Numeric vector. In case some of the loadings of the growth model are freely
#' estimated, provide the correct time scale here (e.g., c(0, 1, 2)).
#' @param jitter_lines Numeric. Indicate the amount (expressed in fractions of a
#' standard deviation of all observed data) by which the observed trajectories
#' should be vertically jittered. Like alpha_range, this parameter helps control
#' overplotting.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @export
#' @import ggplot2
#' @keywords internal
#' @examples
#' \dontrun{
#' mydat <- read.table("http://statmodel.com/usersguide/chap8/ex8.2.dat", header = FALSE)[,-6]
#' createMixtures(classes = 1:3, filename_stem = "ex8.2",
#'                model_overall = "i s | V1@0 V2@1 V3@2 V4@3;  i s on V5;",
#'                rdata = mydat,
#'                OUTPUT = "tech11 tech14;", usevariables = c("V1", "V2", "V3",
#'                                                            "V4", "V5"))
#' runModels(replaceOutfile = "modifiedDate")
#' mplus_output <- readModels(filefilter = "ex8.2")
#' plotGrowthMixtures(mplus_output, estimated = TRUE, rawdata = TRUE, time_scale = c(0, 1, 2, 3))
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
           growth_variables = NULL,
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
        tolower(x$input$analysis$type) == "mixture"
      })
    
    if (any(!mixtures)){
      if (!any(mixtures))
        stop(
          "plotMixtures requires a list of mixture models, or one mixture model, as its first argument."
        )
      warning(
        "Some output files were excluded because they are not mixture models; specifically: ",
        paste(names(modelList)[which(!mixtures)], collapse = ", "),
        call. = FALSE
      )
      modelList <- modelList[which(mixtures)]
    }
    # Remove models which are not type "mixture"
    
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
    
    # Check if all models are growth models
    is_growth_model <- sapply(modelList, function(x){
      any(grepl("\\|$", x$parameters[[coefficients]]$paramHeader))
    })
    if (any(!is_growth_model)){
      if (!any(is_growth_model))
        stop(
          "plotMixtures requires a list of growth models, or one growth model, as its first argument."
        )
      warning(
        "Some output files were excluded because they are not growth models; specifically: ",
        paste(names(modelList)[which(!is_growth_model)], collapse = ", "),
        call. = FALSE
      )
      # Remove models which are not growth models
      modelList <- modelList[which(is_growth_model)]
    }
    
    
    # Prepare plot data
    # Get coefficients
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
        tmp <- subset(
          x,
          subset = if (is.null(growth_variables)) {
            grepl("\\|$", x$paramHeader)
          } else {
            x$paramHeader %in% paste0(toupper(growth_variables), ".|")
          },
          select = c("paramHeader", "est", "param", "est_se", "LatentClass")
        )
        tmp$paramHeader <- gsub("\\.\\|$", "", tmp$paramHeader)
        tmp
      })
    
    if(is.null(growth_variables)) growth_variables <- unique(unlist(lapply(loadings, function(x){x$paramHeader})))
    
    # Set time scale
    if (is.null(time_scale) &
        !all(unlist(lapply(loadings, function(x) {
          x$est_se == 999
        })))) {
      stop(
        "Factor loadings freely estimated. Please specify the correct time scale in the argument time_scale."
      )
    }
    
    observed_variables <-
      table(unlist(lapply(loadings, function(x) {
        x$param
      })))
    if (any(observed_variables != max(observed_variables))) {
      stop("Different variables used for latent growth analyses across models.")
    }
    
    loadings <- lapply(loadings, function(x) {
      array(x$est, dim=c(length(unique(x$param)), length(unique(x$paramHeader)), length(unique(x$LatentClass))))
    })
    
    num_loadings <- sapply(loadings, nrow)
    if(!all(num_loadings == max(num_loadings))){
      stop("Different models appear to be on different time scales (not the same number of loadings for latent growth variables.")
    } else {
      if(is.null(time_scale)) time_scale <- 0:(max(num_loadings)-1)
    }
    
    # Extract estimates
    estimates <- lapply(plotdat, function(x) {
      subset(
        x,
        select = c("param", "est", "LatentClass"),
        subset = (x$param %in% toupper(growth_variables)) &
          (x$paramHeader %in% c("Means", "Intercepts")))
    })
    
    estimates <- lapply(estimates, function(x) {
      array(sapply(x$est, rep, length(time_scale)), dim=c(length(time_scale), length(unique(x$param)), length(unique(x$LatentClass))))
    })

    predicted_trajectories <- lapply(1:length(plotdat), function(x){
      loadings[[x]] * estimates[[x]]
    })
    
    predicted_trajectories <- lapply(predicted_trajectories, apply, 3, rowSums)
    
    predicted_trajectories <- unlist(lapply(predicted_trajectories, matrix))
    #Time <- rep(time_scale, length(predicted_trajectories)/length(time_scale))                                     
    Time <- time_scale
    classes <- sapply(loadings, function(x){ dim(x)[3]})
    Class <- unlist(sapply(classes, function(x){
      sort(rep(1:x, length(time_scale)))
    }), use.names = FALSE)
    Title <- unlist(mapply(FUN = function(Title, Times){
      rep(Title, Times*length(time_scale))
    }, Title = sapply(names(loadings), function(x){
      trimws(modelList[[x]]$input$title)
    }), Times = classes))
    
    predicted_trajectories <- data.frame(Time = Time, 
                                         Value = predicted_trajectories, 
                                         Class = ordered(Class),
                                         Title = Title, row.names = NULL)
    
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
        rawdata <-
          lapply(modelList, function(x) {
            replacethese <- pmatch(tolower(names(x$savedata)), tolower(substring(names(observed_variables), first = 1, last = 8)))
            names(x$savedata)[which(!is.na(replacethese))] <- names(observed_variables)[na.omit(replacethese)]
            subset(x$savedata,
                   select = c(
                     names(x$savedata)[which(names(x$savedata) %in% names(observed_variables))],
                     grep("^CPROB", names(x$savedata), value = TRUE)
                   ))
          })
        # Check if all variables (except CPROBs) are identical across models
        var_names <-
          sapply(rawdata, function(x) {
            names(x)[-c(which(names(x) == "C"), grep("^CPROB", names(x)))]
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
        
        rawdata <-
          lapply(rawdata, function(x) {
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
        
        rawdata <- lapply(rawdata, function(x) {
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
        rawdata <-
          do.call(rbind, lapply(names(modelList), function(x) {
            data.frame(Title = trimws(modelList[[x]]$input$title), rawdata[[x]])
          }))
        
        rawdata$Time <- factor(rawdata$Time)
        levels(rawdata$Time) <- time_scale
        
        rawdata$Time <-
          as.numeric(levels(rawdata$Time))[rawdata$Time]
        rawdata$Class <- ordered(rawdata$Class)
        rawdata$ID <-
          paste(rawdata$Title, rawdata$Class, rawdata$ID, sep = "")
        if (!is.null(jitter_lines)) {
          rawdata$Value <- rawdata$Value + stats::rnorm(nrow(rawdata), sd = (jitter_lines * stats::sd(rawdata$Value, na.rm = TRUE)))
        }
        if (bw) {
          line_plot <- line_plot + geom_path(
            data = rawdata,
            aes_string(
              x = "Time",
              y = "Value",
              group = "ID",
              linetype = "Class",
              alpha = "Probability"
            )
          ) +
            scale_alpha_continuous(range = alpha_range, guide = FALSE)
        } else {
          line_plot <- line_plot + geom_path(
            data = rawdata,
            aes_string(
              x = "Time",
              y = "Value",
              group = "ID",
              linetype = "Class",
              colour = "Class",
              alpha = "Probability"
            )
          ) +
            scale_alpha_continuous(range = alpha_range, guide = FALSE)
        }
        if (poly) {
          if (bw) {
            line_plot <- line_plot +
              stat_smooth(
                data = rawdata,
                aes_string(
                  x = "Time",
                  y = "Value",
                  linetype = "Class",
                  weight = "Probability"
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
                data = rawdata,
                aes_string(
                  x = "Time",
                  y = "Value",
                  colour = "Class",
                  linetype = "Class",
                  weight = "Probability"
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
            aes_string(
              x = "Time",
              y = "Value",
              group = "Class",
              shape = "Class"
            ),
            size = 2
          ) +
          geom_line(
            data = predicted_trajectories,
            aes_string(
              x = "Time",
              y = "Value",
              group = "Class",
              linetype = "Class"
            ),
            size = 1
          )
      } else {
        line_plot <- line_plot +
          geom_point(
            data = predicted_trajectories,
            aes_string(
              x = "Time",
              y = "Value",
              group = "Class",
              shape = "Class",
              colour = "Class"
            ),
            size = 2
          ) +
          geom_line(
            data = predicted_trajectories,
            aes_string(
              x = "Time",
              y = "Value",
              group = "Class",
              linetype = "Class",
              colour = "Class"
            ),
            size = 1
          )
      }
      
    }
    if (length(modelList) > 1)
      line_plot <- line_plot + facet_wrap(~ Title)
    
    line_plot <- line_plot + theme_bw() +
      scale_x_continuous(expand = c(0, 0), breaks = time_scale, labels = time_scale) +
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
#' @param facet_labels Named character vector, the names of which should 
#' correspond to the facet labels one wishes to rename, and the values of which
#' provide new names for these facets. For example, to rename variables, in the
#' example with the 'iris' data below, one could specify: 
#' \code{facet_labels = c("Pet_leng" = "Petal length")}.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @note This function returns warnings, indicating that sum(weights) != 1.
#' These can be ignored. The sum of the "Total" density per variable per model
#' is equal to 1, and the sum of all of the posterior probabilities is equal to
#' 1. This results in a normal density plot for the "Total", which is subdivided
#' by the latent classes, in proportion to the posterior probabilities of
#' participants being assigned to those clases.
#' @export
#' @import ggplot2
#' @keywords mixture models mplus
#' @examples
#' \dontrun{
#' createMixtures(classes = 1:3, filename_stem = "iris", rdata = iris)
#' runModels(filefilter = "iris")
#' results <- readModels(filefilter = "iris")
#' plotMixtureDensities(results)
#' }
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
           alpha = .2,
           facet_labels = NULL) {
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
    rawdata <- lapply(modelList, function(x) {
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
      variables <- variables[which(toupper(variables) %in% var_names[, 1])]
    }
    rawdata <-
      lapply(modelList, function(x) {
        x$savedata[, which(names(x$savedata) %in% c(grep("^CPROB", names(x$savedata), value = TRUE), toupper(variables)))]
      })
    rawdata <- lapply(rawdata, function(x) {
      if (length(grep("^CPROB", names(x))) == 1) {
        names(x) <- gsub("^CPROB1", "Probability.Total", names(x))
        x
      } else {
        names(x) <- gsub("^CPROB", "Probability.", names(x))
        data.frame(x, Probability.Total = 1)
      }
      
    })
    
    for (i in names(rawdata)) {
      rawdata[[i]][, grep("^Probability", names(rawdata[[i]]))] <-
        lapply(rawdata[[i]][grep("^Probability", names(rawdata[[i]]))], function(x) {
          x / length(x)
        })
    }
    
    
    rawdata <- lapply(rawdata, function(x) {
      reshape(
        x,
        direction = "long",
        varying =
          grep("^Probability", names(x), value = TRUE),
        timevar = "Class",
        idvar = "ID"
      )
    })
    
    rawdata <-
      do.call(rbind, lapply(names(modelList), function(x) {
        data.frame(Title = trimws(modelList[[x]]$input$title), rawdata[[x]])
      }))
    
    variable_names <- which(!(names(rawdata) %in% c("Title", "Class", "Probability", "ID")))
    
    names(rawdata)[variable_names] <- sapply(names(rawdata)[variable_names], function(x){
      paste(c("Value.", toupper(substring(x, 1, 1)), tolower(substring(x, 2))), collapse = "")})
    
    rawdata <- reshape(
      rawdata,
      direction = "long",
      varying =
        grep("^Value", names(rawdata), value = TRUE),
      timevar = "Variable"
    )[, c("Title", "Variable", "Value", "Class", "Probability")]
    
    rawdata$Variable <- factor(rawdata$Variable)
    rawdata$Class <- factor(rawdata$Class)
    rawdata$Class <-
      ordered(rawdata$Class, levels = c("Total", levels(rawdata$Class)[-length(levels(rawdata$Class))]))

    # Plot figure
    if (bw) {
      if (conditional) {
        rawdata <- rawdata[-which(rawdata$Class == "Total"),]
        density_plot <-
          ggplot(rawdata,
                 aes_string(x = "Value", y = "..count..", fill = "Class", weight = "Probability")) +
          geom_density(position = "fill") + scale_fill_grey(start = 0.2, end = 0.8)
      } else{
        density_plot <-
          ggplot(rawdata,
                 aes_string(x = "Value", linetype = "Class", weight = "Probability")) +
          geom_density()
      }
    } else{
      if (conditional) {
        rawdata <- rawdata[-which(rawdata$Class == "Total"),]
        density_plot <-
          ggplot(rawdata,
                 aes_string(x = "Value", y = "..count..", fill = "Class", weight = "Probability")) +
          geom_density(position = "fill")
      } else{
        density_plot <-
          ggplot(rawdata,
                 aes_string(x = "Value",
                   fill = "Class",
                   colour = "Class",
                   weight = "Probability"
                 )) +
          geom_density(alpha = alpha)
      }
    }
    # Relabel facets
    label_facets <- c(levels(rawdata$Variable), levels(rawdata$Title))
    names(label_facets) <- label_facets
    if(!is.null(facet_labels)){
      label_facets[which(tolower(names(label_facets)) %in% tolower(names(facet_labels)))] <- facet_labels[which(tolower(names(facet_labels)) %in% tolower(names(label_facets)))]
    }
    # Facet the plot
    if (length(modelList) > 1) {
      if (length(variables) > 1) {

        density_plot <- density_plot + 
          facet_grid(Title ~ Variable, labeller = labeller(Title = label_facets, Variable = label_facets))

      } else {
        density_plot <- density_plot +
          facet_grid( ~ Title, labeller = labeller(Title = label_facets))
      }
    } else {
      if (length(variables) > 1) {
        density_plot <- density_plot +
          facet_grid( ~ Variable, labeller = labeller(Variable = label_facets))
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
#' @param model_class_specific Character vector. Mplus syntax for the 
#' class-specific model(s) of one or more categorical latent variables. Each 
#' element of \code{model_class_specific} is used as the class-specific syntax
#' of a different categorical latent variable. This allows one to easily specify
#' latent transition analyses (see second example). The character string
#' \dQuote{\{C\}} is substituted with the correct class number, for example to
#' set unique parameter labels for each class, or to specify equality
#' constraints.
#' @param rdata Data.frame. An R dataset to be used for the model.
#' @param usevariables Character vector, specifying the names of variables in 
#' the rdata object which should be included in the Mplus data file and model.
#' @param OUTPUT Character. Syntax for Mplus' OUTPUT option. Highly
#' recommended when determining the appropriate number of latent classes. TECH11
#' is required to obtain the VLMR-test; TECH14 is required for the BLR-test.
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
#' \dontrun{
#' createMixtures(classes = 1:3, filename_stem = "iris", rdata = iris)
#' }
#' \dontrun{
#' data <- read.table("http://statmodel.com/usersguide/chap8/ex8.13.dat")[,c(1:10)]
#' names(data) <- c("u11", "u12", "u13", "u14", "u15", "u21", "u22", "u23", "u24", "u25")
#' createMixtures(
#' classes = 2,
#' filename_stem = "dating",
#' model_overall = "c2 ON c1;",
#' model_class_specific = c(
#' "[u11$1] (a{C});  [u12$1] (b{C});  [u13$1] (c{C});  [u14$1] (d{C});  [u15$1] (e{C});",
#' "[u21$1] (a{C});  [u22$1] (b{C});  [u23$1] (c{C});  [u24$1] (d{C});  [u25$1] (e{C});"
#' ),
#' rdata = data,
#' ANALYSIS = "PROCESSORS IS 2;  LRTSTARTS (0 0 40 20);  PARAMETERIZATION = PROBABILITY;",
#' VARIABLE = "CATEGORICAL = u11-u15 u21-u25;"
#' )
#' }
createMixtures <- function(classes = 1L,
                           filename_stem = NULL,
                           model_overall = NULL,
                           model_class_specific = NULL,
                           rdata = NULL,
                           usevariables = NULL,
                           OUTPUT = "TECH11 TECH14;",
                           SAVEDATA = "FILE IS {filename_stem}_{C}.dat;  SAVE = cprobabilities;",
                           ...) {
  Args <- c(
    list(
      rdata = rdata,
      usevariables = usevariables,
      OUTPUT = OUTPUT
    ),
    list(...)
  )
  if (hasArg("MODEL")) {
    warning(
      "MODEL argument was dropped: createMixtures constructs its own MODEL argument from model_overall and model_class_specific."
    )
    Args$MODEL <- NULL
  }
  if (!is.null(SAVEDATA)) {
    Args$SAVEDATA <- gsub("\\{filename_stem\\}",
                         filename_stem, SAVEDATA)
  } else {
    Args$SAVEDATA <- NULL
  }
  if (!hasArg(usevariables) & !hasArg("DEFINE")) {
    message("No usevariables provided, or variables defined. All variables in rdata were used.")
    Args[["usevariables"]] <- names(rdata)
  }
  if (any(sapply(Args[["usevariables"]], nchar) > 8)) {
    warning(
      "Some variable names exceed 8 characters and will be truncated by Mplus. This can cause problems when plotting mixture models. Please rename variables."
    )
  }
  if (any(sapply(Args[["usevariables"]], grepl, "\\."))) {
    Args[["usevariables"]] <- gsub("\\.", "_", Args[["usevariables"]])
    warning("Some variable names contain periods ('.'). These were replaced with underscores.")
  }
  
  Args[["MODEL"]] <-
    paste(c("%OVERALL%\n", model_overall, "\n\n"), collapse = "")
  if (hasArg("ANALYSIS")) {
    Args[["ANALYSIS"]] <-
      paste0("TYPE = mixture;\n", Args[["ANALYSIS"]])
  } else {
    Args[["ANALYSIS"]] <- "TYPE = mixture;\n"
  }
  
  char_args <- which(sapply(Args, is.character))
  Args[char_args] <-
    lapply(Args[char_args], function(x) {
      gsub("  ", "\n", x)
    })
  
  if(!is.null(model_class_specific)){
    model_class_specific <- gsub("  ", "\n", model_class_specific)
    n_latentvars <- length(model_class_specific)
  } else {
    n_latentvars <- 1
  }
  n_classes <- length(classes)
  
  # Create mplusObject template
  base_object <- do.call(mplusObject, Args)
  
  # Expand template for requested classes
  input_list <- lapply(classes, function(num_classes) {
    base_object$VARIABLE <-
      paste0(base_object$VARIABLE, paste(c(
        "CLASSES = ",
        paste(
          "c",
          1:n_latentvars,
          "(",
          num_classes,
          ")",
          sep = "",
          collapse = " "
        ),
        ";\n"
      ), collapse = ""))
    if (!is.null(model_class_specific)) {
      expand_class_specific <- ""
      for (var_num in 1:n_latentvars) {
        if(!is.null(model_class_specific[var_num])){
          if(n_latentvars > 1){
            expand_class_specific <-
              paste0(expand_class_specific,
                     paste0("MODEL c", var_num, ":\n"))
          }
          for (this_class in 1:num_classes) {
            expand_class_specific <-
              paste0(expand_class_specific,
                     gsub("\\{C\\}", this_class, paste(
                       c(
                         "%c",
                         var_num,
                         "#",
                         this_class,
                         "%\n",
                         model_class_specific[var_num],
                         "\n\n"
                       ),
                       collapse = ""
                     )))
          }
        }
      }
      base_object$MODEL <-
        paste0(base_object$MODEL,
               expand_class_specific)
    }
    if (!is.null(base_object[["SAVEDATA"]])) {
      base_object$SAVEDATA <-
        gsub("\\{C\\}", num_classes, base_object$SAVEDATA)
    }
    base_object$TITLE <-
      paste(c(base_object$TITLE, num_classes, "classes"), collapse = " ")
    
    base_object
  })
  
  # Write files
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

#' Plot latent transition model
#'
#' Plots latent transition probabilities and classification probabilities for
#' a single latent transition model (a model with multiple categorical latent
#' variables, regressed on one another). Stroke thickness of nodes represents
#' the proportion of cases most likely assigned to that class, with wider 
#' strokes representing greater probability. Edge thickness and transparency
#' represent the probability of making a particular transition (left to right),
#' with thicker/darker edges representing greater probability.
#' @param mplusModel A single Mplus model object, returned by . This
#' function additionally requires the model to be a mixture model with multiple
#' categorical latent variables.
#' @param node_stroke Integer. Base stroke thickness for nodes. Set to 
#' \code{NULL} to give each node the same stroke thickness.
#' @param max_edge_width Integer. Maximum width of edges.
#' @param node_labels Character vector, defaults to \code{"variable.class"}, 
#' which labels each node by the name of the variable, and the number of the 
#' class it represents. Set to \code{"class"} to display only class numbers, or
#' provide a named character vector where the names correspond to original class
#' labels, and the values correspond to their substitute values.
#' @param x_labels Character vector, defaults to \code{"variable"}, which labels
#' the x-axis with the names of the categorical latent variables. Set to
#' \code{NULL} to remove axis labels, or provide a named character vector where
#' the names correspond to original x-axis labels, and the values correspond to
#' their substitute values.
#' @return An object of class 'ggplot'.
#' @author Caspar J. van Lissa
#' @export
#' @import ggplot2
#' @importFrom stats complete.cases setNames
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- read.table("http://statmodel.com/usersguide/chap8/ex8.13.dat")[,c(1:10)]
#' names(data) <- c("u11", "u12", "u13", "u14", "u15", "u21", "u22", "u23", "u24", "u25")
#' createMixtures(
#' classes = 2,
#' filename_stem = "dating",
#' model_overall = "c2 ON c1;",
#' model_class_specific = c(
#' "[u11$1] (a{C});  [u12$1] (b{C});  [u13$1] (c{C});  [u14$1] (d{C});  [u15$1] (e{C});",
#' "[u21$1] (a{C});  [u22$1] (b{C});  [u23$1] (c{C});  [u24$1] (d{C});  [u25$1] (e{C});"
#' ),
#' rdata = data,
#' ANALYSIS = "PROCESSORS IS 2;  LRTSTARTS (0 0 40 20);  PARAMETERIZATION = PROBABILITY;",
#' VARIABLE = "CATEGORICAL = u11-u15 u21-u25;"
#' )
#' runModels(filefilter = "dating")
#' results <- readModels(filefilter = "dating")
#' plotLTA(results)
#' }
plotLTA <-
  function(mplusModel,
           node_stroke = 2,
           max_edge_width = 2,
           node_labels = "variable.class",
           x_labels = "variable") {
    
    # Check if mplusModel is of class mplus.model
    if (!(inherits(mplusModel, "mplus.model"))) {
      stop(
        "plotLTA requires an object of class 'mplus.model' as its first argument."
      )
    }
    if(is.null(mplusModel$input$analysis[["type"]]))
      stop(
        "plotLTA requires a mixture model as its first argument."
      )
    if(!tolower(mplusModel$input$analysis$type) == "mixture")
      stop(
        "plotLTA requires a mixture model as its first argument."
      )
    if(!mplusModel$summaries$NCategoricalLatentVars > 1)
      stop(
      "plotLTA requires a mixture model with multiple categorical latent variables as its first argument."
    )
    
    # Remove models which are not type "mixture"
    edges <- mplusModel$class_counts$transitionProbs
    
    all_classes <- unique(c(edges$from, edges$to))
    latent_variables <- unique(gsub("\\..+$", "", all_classes))
    
    edges$x <-
      as.numeric(factor(gsub("\\..+$", "", as.character(edges$from)), levels = latent_variables))
    edges$xend <-
      as.numeric(factor(gsub("\\..+$", "", as.character(edges$to)), levels = latent_variables))
    edges$y <-
      as.numeric(gsub("^.+\\.", "", as.character(edges$from)))
    edges$yend <-
      as.numeric(gsub("^.+\\.", "", as.character(edges$to)))
    
    nodes <-
      rbind(edges[, c(1, 4, 6)], setNames(edges[, c(2, 5, 7)], names(edges)[c(1, 4, 6)]))
    nodes <- nodes[!duplicated(nodes),]
    names(nodes)[1] <- "nodeID"
    
    n_prop <- mplusModel$class_counts$modelEstimated
    if (!is.null(node_stroke)) {
      n_prop$proportion <-
        node_stroke * n_prop$proportion * inverse.rle(list(
          lengths = rle(n_prop$variable)$lengths,
          values = rle(n_prop$variable)$lengths
        ))
    } else {
      n_prop$proportion <- 1
    }
    
    n_prop$nodeID <- paste(n_prop$variable, n_prop$class, sep = ".")
    nodes <- merge(nodes, n_prop, by = "nodeID")
    
    if (!node_labels %in% c("variable.class", "class")) {
      nodes$nodeID[which(nodes$nodeID %in% names(node_labels))] <-
        node_labels
    } else {
      if(node_labels == "class")
        nodes$nodeID <- gsub(".+?\\.", "", nodes$nodeID)
    }
    nodesize <- max(max(sapply(nodes$nodeID, nchar)) * 3.880556, 6)
    p <- ggplot(NULL)
    p <- p + geom_segment(
      data = edges,
      aes_string(
        x = "x",
        y = "y",
        xend = "xend",
        yend = "yend",
        size = "probability",
        alpha = "probability"
      )
    )
    p <-
      p + geom_point(
        data = nodes,
        shape = 21,
        size = nodesize,
        colour = "black",
        fill = "white",
        aes_string(x = "x", y = "y", stroke = "proportion")
      )
    p <-
      p + geom_text(data = nodes, aes_string(x = "x", y = "y", label = "nodeID"))
    p <-  p +
      scale_y_continuous(expand = c(.1, .1)) +
      theme(
        text = element_text(size=11, colour = "black"),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank()
      ) + scale_size_continuous(range = c(1, max_edge_width))
    if (!is.null(x_labels)) {
      uselabels <- unique(nodes$variable)
      if (!x_labels == "variable") {
        uselabels[which(uselabels %in% names(x_labels))] <- x_labels
      }
      p <-
        p + scale_x_continuous(
          expand = c(.1, .1),
          breaks = unique(nodes$x),
          labels = uselabels,
          position = "top"
        ) +
        theme(axis.line.x = element_line(color = "black"))
      
    } else {
      p <- p +
        theme(axis.text.x = element_blank())
    }
    p
  }
