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
#' @param sortBy Field name (as character string) by which to sort the
#' table. Typically an information criterion (e.g., "AIC" or "BIC") is used to
#' sort the table. Defaults to "AICC". Set to NULL by default, so the table is
#' ordered by increasing number of classes.
#' @param ... Arguments passed to \code{\link{SummaryTable}}.
#' @return An object of class data.frame.
#' @author Caspar J. van Lissa
#' @note This function is partially a wrapper around SummaryTable, with
#' enhancements for summarizing mixture models.
#' @seealso \code{\link{SummaryTable}}
#' @export
#' @keywords mixture mplus
#' @examples
#' \dontrun{
#' res <- createMixtures(classes = 1:2, filename_stem = "iris", rdata = iris,
#'                OUTPUT = "tech11 tech14;",
#'                run = 1L)
#' mixtureSummaryTable(res)
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
                                ),
                                sortBy = NULL,
                                ...) {
  if(all(sapply(modelList, function(x){grepl("mixture", tolower(x$ANALYSIS))}))){
    if(all(sapply(modelList, function(x){ is.null(x[["results"]])}))){
      return(cat("These mixture models have not yet been evaluated. Add `run = 1L` to your function call to do so."))
    }
  }
  modelList <- tryCatch(mplus_as_list(modelList), error = function(e){
    stop("mixtureSummaryTable requires a list of mixture models as its first argument.")
  })
  
  # Remove models which are not type "mixture"
  modelList <- check_mixtures(modelList)
  
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
  summarytable_keepCols <- unique(keepCols[which(!keepCols %in% c("min_N", "max_N", "min_prob", "max_prob"))])
  summarytable_keepCols <- c(summarytable_keepCols, paste0(summarytable_keepCols, "_Mean"))
  if (length(summarytable_keepCols > 0)) {
    cl <- match.call()
    cl[[1L]] <- quote(SummaryTable)
    cl["sortBy"] <- list(NULL)
    if(is.null(cl[["type"]])) cl$type <- "none"
    cl[["modelList"]] <- modelList
    cl[["keepCols"]] <- summarytable_keepCols
    sumtab <- eval.parent(cl)
    model_summaries <- cbind(
        model_summaries,
        sumtab)
  }
  if(!is.null(sortBy)){
    if(sortBy %in% names(model_summaries)){
      model_summaries <- model_summaries[order(model_summaries[[sortBy]]), ]
    }
  }
  
  if(any(!keepCols %in% names(model_summaries) & paste0(keepCols, "_Mean") %in% names(model_summaries))){
    warning("Returned mean value of: ", paste0(keepCols[!keepCols %in% names(model_summaries) & paste0(keepCols, "_Mean") %in% names(model_summaries)], collapse = ", "))
    keepCols[!keepCols %in% names(model_summaries) & paste0(keepCols, "_Mean") %in% names(model_summaries)] <- paste0(keepCols[!keepCols %in% names(model_summaries) & paste0(keepCols, "_Mean") %in% names(model_summaries)], "_Mean")
  }
  model_summaries <-
    model_summaries[, keepCols[which(keepCols %in% names(model_summaries))],
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
      "The best loglikelihood value was not replicated in some models. The solution may not be trustworthy due to local maxima. Increase the number of random starts. The problematic models were row number(s): ",
      paste(not_replicated, collapse = ", "),
      call. = FALSE
    )
  }
  not_terminated <- which(sapply(modelList, function(x) {
    length(grep(
      "THE MODEL ESTIMATION DID NOT TERMINATE NORMALLY",
      paste(unlist(x$errors), collapse = "")
    ))
  }) > 0)
  if (length(not_terminated) > 0) {
    warning(
      "Model estimation did not terminate normally due to an error in the computation. Change your model and/or starting values. The problematic models were row number(s): ",
      paste(not_terminated, collapse = ", "),
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
#' res <- createMixtures(classes = 1:2, filename_stem = "cars",
#'                       model_overall = "wt ON drat;",
#'                       model_class_specific = "wt;  qsec;",
#'                       rdata = mtcars,
#'                       usevariables = c("wt", "qsec", "drat"),
#'                       OUTPUT = "standardized",
#'                       run = 1L)
#' plotMixtures(res, rawdata = TRUE)
#' }
#' \dontrun{
#' plotMixtures(res, variables = "wt")
#' }
#' \dontrun{
#' plotMixtures(res, coefficients = "stdyx.standardized")
#' }
# modelList,
# variables = NULL,
# coefficients = c("unstandardized",
#                  "stdyx.standardized",
#                  "stdy.standardized",
#                  "stdy.standardized"),
# parameter = c("Means", "Intercepts"),
# ci = .95,
# bw = FALSE,
# rawdata = FALSE,
# alpha_range = c(0, .1)
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
  modelList <- tryCatch(mplus_as_list(modelList), error = function(e){
    stop(
      "plotMixtures requires an object of class 'mplus.model' or a list of mplus.models as its first argument."
    )
  })
    
  # Remove models which are not type "mixture"
  modelList <- check_mixtures(modelList)
  
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
            names(x)[-grep("^CPROB", names(x))]
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
#' @param time_scale Numeric vector. In case some of the loadings of the growth
#' model are freely
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
#' mydat <- read.csv(
#' system.file("extdata", "ex8.2.csv", package = "MplusAutomation"))
#' res <- createMixtures(classes = 1:3, filename_stem = "ex8.2",
#'                       model_overall = 
#'                                     "i s | V1@0 V2@1 V3@2 V4@3;  i s on V5;",
#'                       rdata = mydat,
#'                       OUTPUT = "tech11 tech14;", 
#'                       usevariables = c("V1", "V2", "V3", "V4", "V5"),
#'                       run = 1L)
#' plotGrowthMixtures(res, estimated = TRUE, rawdata = TRUE, 
#'                    time_scale = c(0, 1, 2, 3))
#' plotGrowthMixtures(res, estimated = FALSE, rawdata = TRUE, poly = TRUE)
#'
#' res <- createMixtures(classes = 1:3, filename_stem = "ex8.2_free",
#'                       model_overall = "i s | V1@0 V2* V3* V4@3;  i s on V5;",
#'                       rdata = mydat,
#'                       OUTPUT = "tech11 tech14;",
#'                       usevariables = c("V1", "V2", "V3", "V4", "V5"),
#'                       run = 1L)
#' plotMixtureDensities(res, variables = c("V1", "V2", "V3", "V4"))
#' plotGrowthMixtures(res, estimated = TRUE, rawdata = TRUE,
#'                    bw = TRUE, time_scale = c(0, 1, 2, 3),
#'                    alpha_range = c(0, .05))
#'
#' plotGrowthMixtures(res, estimated = FALSE, rawdata = TRUE,
#'                    poly = TRUE, bw = TRUE, time_scale = c(0, 1, 2, 3))
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
    modelList <- tryCatch(mplus_as_list(modelList), error = function(e){
      stop(
        "plotGrowthMixtures requires an object of class 'mplus.model' or a list of mplus.models as its first argument."
      )
    })
    
    # Remove models which are not type "mixture"
    modelList <- check_mixtures(modelList)
    
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
      any(grepl("\\|$", x$parameters[[coefficients]]$paramHeader))|
      any(grepl("\\w+\\|(\\s?\\b\\w+\\b\\s?){2,}", x$input$model))
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
    }, Title = sapply(1:length(modelList), function(x){
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
            names(x)[-grep("^CPROB", names(x))]
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
#' results <- createMixtures(classes = 1:3, filename_stem = "iris",
#'                           rdata = iris, run = 1L)
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
    cl <- match.call()
    modelList <- mplus_as_list(modelList)
    modelList <- as.tidyLPA_model.list(modelList)
    # If no variables have been specified, use all variables
    var_names <- names(modelList[[1]]$dff)
    var_names <- var_names[1:min((which(var_names == "CPROB1")-1), length(var_names))]
    var_names <- var_names[-grep("^(model_number|classes_number|CPROB\\d+|Class)$", var_names)]
    if (is.null(variables)) {
      variables <- var_names
    } else {
      variables <- variables[which((variables) %in% (var_names))]
    }
    if (!length(variables))
      stop("No valid variables provided.")
    
    cl[["variables"]] <- variables
    cl[["x"]] <- modelList
    cl_dens <- cl
    cl_dens[[1L]] <- str2lang("MplusAutomation:::.extract_density_data")
    cl_dens <- cl_dens[c(1, which(names(cl_dens) %in% c("x", "variables")))]
    cl[["x"]] <- eval.parent(cl_dens)
    cl[["x"]] <- cl[["x"]][!is.na(cl[["x"]]$Value), ]
    cl[[1L]] <- str2lang("MplusAutomation:::plot_density_default")
    cl <- cl[c(1, which(names(cl) %in% c("x", "variables", "bw", "conditional", "alpha", "facet_labels")))]
    eval.parent(cl)
  }

#' Create syntax for a batch of mixture models
#'
#' Dynamically creates syntax for a batch of mixture models, with intelligent
#' defaults. This function is a wrapper around \code{mplusObject} and
#' \code{mplusModeler}, and the respective arguments of those functions can be
#' passed on using \code{...}. For instance, passing the argument
#' \code{run = 1L} means that the models will be evaluated and returned.
#' 
#' In the arguments \code{model_class_specific} and \code{SAVEDATA}, the
#' character string \dQuote{\{C\}} is substituted with the correct class number.
#' The character string \dQuote{\{filename_stem\}} is substituted with the
#' filename stem, for example, to name savedata in line with the input files.
#'
#' In all arguments to \code{mplusObject}, a double space (\dQuote{  }) is
#' replaced with a newline character. This can be used to obtain nicely
#' formatted Mplus syntax.
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
#' @param quiet optional. If \code{TRUE}, show status messages in the console.
#' @param ... Additional arguments, passed to \link{mplusObject}, such as syntax
#' for other Mplus options.
#' @return None, unless the argument \code{run = 1L} is specified. In that case,
#' a list with elements of class \code{mplusObject} is returned. Otherwise, this
#' function is used for its side effects (generating syntax).
#' @author Caspar J. van Lissa
#' @seealso \code{\link{mplusObject}}, \code{\link{mplusModeler}}
#' @export
#' @keywords mixture models mplus
#' @examples
#' \dontrun{
#' createMixtures(classes = 1:3, filename_stem = "iris", rdata = iris)
#' }
#' \dontrun{
#' mydat <- read.csv(
#' system.file("extdata", "ex8.13.csv", package = "MplusAutomation"))
#' createMixtures(
#' classes = 2,
#' filename_stem = "dating",
#' model_overall = "c2 ON c1;",
#' model_class_specific = c(
#' "[u11$1] (a{C});  [u12$1] (b{C});  [u13$1] (c{C});  [u14$1] (d{C});  [u15$1] (e{C});",
#' "[u21$1] (a{C});  [u22$1] (b{C});  [u23$1] (c{C});  [u24$1] (d{C});  [u25$1] (e{C});"
#' ),
#' rdata = mydat,
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
                           quiet = TRUE,
                           ...) {
  dots <- list(...)
  cl <- match.call()
  cl[c("classes", "filename_stem", "model_overall", "model_class_specific")] <- NULL
  if (hasArg("MODEL")) {
    warning(
      "MODEL argument was dropped: createMixtures constructs its own MODEL argument from model_overall and model_class_specific."
    )
    cl$MODEL <- NULL
  }
  if (!is.null(SAVEDATA)) {
    cl$SAVEDATA <- gsub("\\{filename_stem\\}",
                        filename_stem, SAVEDATA)
  } else {
    cl$SAVEDATA <- NULL
  }
  if (!hasArg(usevariables) & !hasArg("DEFINE")) {
    if(isFALSE(quiet)) message("No usevariables provided, or variables defined. All variables in rdata were used.")
    cl[["usevariables"]] <- names(rdata)
  }
  if (any(sapply(usevariables, nchar) > 8)) {
    warning(
      "Some variable names exceed 8 characters and will be truncated by Mplus. This can cause problems when plotting mixture models. Please rename variables."
    )
  }
  if (any(sapply(usevariables, grepl, "\\."))) {
    cl[["usevariables"]] <- gsub("\\.", "_", usevariables)
    warning("Some variable names contain periods ('.'). These were replaced with underscores.")
  }
  
  cl[["MODEL"]] <-
    paste(c("%OVERALL%\n", model_overall, "\n\n"), collapse = "")
  if (hasArg("ANALYSIS")) {
    cl[["ANALYSIS"]] <-
      paste0("TYPE = mixture;\n", dots[["ANALYSIS"]])
  } else {
    cl[["ANALYSIS"]] <- "TYPE = mixture;\n"
  }
  
  char_args <- which(sapply(cl, is.character))
  cl[char_args] <-
    lapply(cl[char_args], function(x) {
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
  cl_mplusobject <- cl[c(1, which(names(cl) %in% c("TITLE", "DATA", "VARIABLE", "DEFINE", "MONTECARLO", "MODELPOPULATION", 
                                                  "MODELMISSING", "ANALYSIS", "MODEL", "MODELINDIRECT", "MODELCONSTRAINT", 
                                                  "MODELTEST", "MODELPRIORS", "OUTPUT", "SAVEDATA", "PLOT", "usevariables", 
                                                  "rdata", "autov", "imputed", "quiet")))]
  cl_mplusobject[[1]] <- quote(mplusObject)
  if(is.null(cl_mplusobject[["OUTPUT"]])) cl_mplusobject$OUTPUT <- OUTPUT
  if(is.null(cl_mplusobject[["quiet"]])) cl_mplusobject$quiet <- quiet
  base_object <- eval.parent(cl_mplusobject)
  
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
  
  # Evaluate models
  # Create mplusModeler call
  cl_mplusmodeler <- cl[c(1, which(names(cl) %in% c("run", "check", "varwarnings", 
                                                    "Mplus_command", "writeData", "hashfilename", "killOnFail", "quiet")))]
  cl_mplusmodeler[[1]] <- quote(mplusModeler)
  
  if(!"run" %in% names(cl_mplusmodeler)) cl_mplusmodeler$run <- 1L
  if(!"check" %in% names(cl_mplusmodeler)) cl_mplusmodeler[["check"]] <- FALSE
  if(!"varwarnings" %in% names(cl_mplusmodeler)) cl_mplusmodeler[["varwarnings"]] <- TRUE
  if(!"Mplus_command" %in% names(cl_mplusmodeler)) cl_mplusmodeler[["Mplus_command"]] <- "Mplus"
  if(!"writeData" %in% names(cl_mplusmodeler)) cl_mplusmodeler[["writeData"]] <- "ifmissing"
  if(!"hashfilename" %in% names(cl_mplusmodeler)) cl_mplusmodeler[["hashfilename"]] <- TRUE
  
  invisible(suppressMessages({
    out <- lapply(1:n_classes, function(class) {
      cl_mplusmodeler[["object"]] <- input_list[[class]]
      cl_mplusmodeler[["dataout"]] <- if (is.null(filename_stem)) {
        "data.dat"
      } else {
        paste(c("data_", filename_stem, ".dat"), collapse = "")
      }
      cl_mplusmodeler[["modelout"]] <- if (is.null(filename_stem)) {
        paste0(filename_stem, "_class.inp")
      } else {
        paste(c(filename_stem, "_", class, "_class.inp"), collapse = "")
      }
      eval.parent(cl_mplusmodeler)
    })
    class(out) <- c("mixture.list", "model.list", class(out))
    names(out) <- paste0(paste(filename_stem, classes, "class", sep = "_"), ".out")
    if(length(out) == 1) out <- out[[1]]
  }))
  return(out)
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
#' mydat <- read.csv(
#' system.file("extdata", "ex8.13.csv", package = "MplusAutomation"))
#' createMixtures(
#' classes = 2,
#' filename_stem = "dating",
#' model_overall = "c2 ON c1;",
#' model_class_specific = c(
#' "[u11$1] (a{C});  [u12$1] (b{C});  [u13$1] (c{C});  [u14$1] (d{C});  [u15$1] (e{C});",
#' "[u21$1] (a{C});  [u22$1] (b{C});  [u23$1] (c{C});  [u24$1] (d{C});  [u25$1] (e{C});"
#' ),
#' rdata = mydat,
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
    mplusModel <- tryCatch(one_mplus_model(mplusModel), error = function(e){
      stop("plotLTA can only plot a single model. Please extract the desired model from the list.")
    })
    
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
    
    if (length(node_labels) > 1 | !(node_labels[1] %in% c("variable.class", "class"))) {
      nodes$nodeID[na.omit(match(names(node_labels), nodes$nodeID))] <-
        node_labels[which(names(node_labels) %in% nodes$nodeID)]
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
      if (length(x_labels) > 1 | !x_labels[1] == "variable") {
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


check_mixtures <- function(modelList){
  # Check if all models in the list are mixture models
  mixtures <- sapply(modelList, function(x) {
    !is.null(x$input$analysis[["type"]])
  })
  mixtures[mixtures] <- sapply(modelList[mixtures], function(x) {
    grepl("mixture", tolower(x$input$analysis$type))
  })
  if (!any(mixtures))
    stop("mixtureSummaryTable requires a list of mixture models as its first argument.")
  if (any(!mixtures))
    warning(
      "Some output files were excluded because they are not mixture models; specifically: ",
      paste(names(mixtures)[which(!mixtures)], collapse = ", ")
    )
  
  # Remove models which are not type "mixture"
  modelList[which(mixtures)]
}

mplus_as_list <- function(x){
  out <- switch(class(x)[1],
         mplus.model.list = x,
         mplus.model = list(Model_1 = x),
         mplusObject = list(Model_1 = x$results),
         mixture.list = lapply(x, `[[`, "results"),
         model.list = lapply(x, `[[`, "results"),
         list = {
           if(all(sapply(x, inherits, what = "mplusObject"))){
             lapply(x, `[[`, "results")
           } else {
             if(all(sapply(x, inherits, what = "mplus.model"))){
               x
             } else {
               stop("Not a list of Mplus models.")
             } 
           }
           
         },
         stop("Not a list of Mplus models.")
         )
  if(is.null(names(out))){
    nms <- sapply(1:length(out), function(i){
      if(!is.null(out[[i]][["input"]][["title"]])){
        out[[i]][["input"]][["title"]]
      } else {
        paste0("Model ", i)
      }
    })
    names(out) <- nms
  } else {
    if(any(names(out) == "")){
      names(out)[which(names(out) == "")] <- paste0("Model ", which(names(out) == ""))
    }
  }
  out
}


one_mplus_model <- function(x){
  out <- switch(class(x)[1],
                mplus.model = x,
                mplusObject = x$results,
                model.list = {
                  if(length(x) == 1){
                    x[[1]][["results"]]
                  } else {
                    stop("Not a single Mplus model.")
                  }},
                list = {
                  if(length(x) == 1){
                    if(inherits(x, "mplusObject")){
                      x[["results"]]
                    } else {
                      if(inherits(x, "mplus.model")){
                        x
                      } else {
                        stop("Not a single Mplus model.")
                      }
                    }
                  }
                },
                mplus.model.list = {
                  if(length(x) == 1){
                    x
                  } else {
                    stop("Not a single Mplus model.")
                  }
                },
                stop("Not a single Mplus model.")
  )
  out
}


as.tidyLPA_model.list <- function(modelList) {
  # Remove models which are not type "mixture"
  modelList <- check_mixtures(modelList)
  
  # Check if all models were run on the same dataset
  if (length(unique(sapply(modelList, function(x) {
    x$input$data$file
  }))) > 1) {
    stop("Models were not all run on the same data file.")
  }
  # Check if any models have missing columns (result of nonconvergence)
  missing_cols <-
    sapply(modelList, function(x) {
      length(names(x$parameters[["unstandardized"]]))
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
      is.null(x$parameters[["unstandardized"]])
    }))
  if (length(missing_coefficients > 0)) {
    warning(
      "Some models were missing the unstandardized coefficients. Please request these coefficients from Mplus.",
      call. = FALSE,
      immediate. = TRUE
    )
    modelList <- modelList[-missing_coefficients]
  }
  if (length(modelList) < 1)
    stop("No models left to convert to tidyLPA.", call. = FALSE)
  
  # Try to figure out what kind of model it was
  # lapply(modelList, function(x){
  #     class_specific <- which(grepl("\\d%", x$input$model))
  # })
  model_numbers <-
    paste0(as.numeric(factor(sapply(modelList, function(x) {
      end_first_class <- grep("#2%", x$input$model)
      end_first_class <-
        ifelse(length(end_first_class) == 0,
               length(x$input$model),
               end_first_class - 1)
      paste(x$input$model[1:end_first_class], collapse = "")
    }))))
  
  out_list <- lapply(modelList, function(x) {
    this_class <- nrow(x$class_counts$modelEstimated)
    this_model <- 99
    
    out <- list(model = x)
    out$fit <-
      c(Model = this_model,
        Classes = this_class,
        calc_fit(out$model))
    out$estimates <- tryCatch(estimates(out$model), error = function(e){ stop("No valid parameters in this model.", call. = FALSE)})
    out$estimates$Model <- this_model
    out$estimates$Classes <- this_class
    
    out$dff <- out$model$savedata
    if(!is.null(out[["dff"]])){
      out$dff$model_number <- this_model
      out$dff$classes_number <- this_class
      out$dff <-
        out$dff[, c((ncol(out$dff) - 1), ncol(out$dff), 1:(ncol(out$dff) - 2))]
      if(names(out$dff)[length(names(out$dff))] == "C"){
        names(out$dff)[length(names(out$dff))] <- "Class"
      }
    }
    
    #if(simplify) out$model <- NULL
    class(out) <-
      c("tidyProfile.mplus", "tidyProfile", "list")
    out
  })
  
  class(out_list) <- c("tidyLPA", "list")
  names(out_list) <-
    paste("model_",
          model_numbers,
          "_class_",
          sapply(out_list, function(x) {
            x$fit["Classes"]
          }),
          sep = "")
  out_list
}

calc_fit <- function(model){
  modsums <- model[["summaries"]]
  names(modsums) <- gsub("_Mean$", "", names(modsums))
  ll <- modsums$LL
  parameters <- modsums$Parameters
  n <- modsums$Observations
  fits <- c(ifelse(is.null(modsums$Entropy), 1, modsums$Entropy),
            tryCatch(range(diag(model$class_counts$classificationProbs.mostLikely)),
                     warning = function(x) {
                       c(NA, NA)
                     }),
            range(model$class_counts$mostLikely$proportion),
            ifelse(is.null(modsums$BLRT_2xLLDiff), NA, modsums$BLRT_2xLLDiff),
            ifelse(is.null(modsums$BLRT_PValue), NA, modsums$BLRT_PValue)
  )
  
  fits <- c(
    LogLik = ll,
    AIC = -2*ll + 2*parameters,
    AWE = -2*(ll + fits[1]) + 2*parameters*(3/2 + log(n)),
    BIC = -2*ll + parameters * log(n),
    CAIC = -2*ll + parameters * (log(n)+1),
    CLC = -2*ll + 2*fits[1],
    KIC = -2*ll + 3*(parameters + 1),
    SABIC = -2*ll + parameters * log(((n+2)/24)),
    ICL = NA,
    fits
  )
  names(fits) <- c("LogLik", "AIC", "AWE", "BIC", "CAIC", "CLC", "KIC", "SABIC", "ICL", "Entropy", "prob_min", "prob_max", "n_min", "n_max", "BLRT_val", "BLRT_p")
  fits
}



# Functions from tidyLPA --------------------------------------------------

plot_density_default <-
  function(x,
           variables = NULL,
           bw = FALSE,
           conditional = FALSE,
           alpha = .2,
           facet_labels = NULL) {
    plot_df <- x
    if(!inherits(plot_df[["Title"]], "factor")){
      plot_df[["Title"]] <- factor(plot_df[["Title"]])
    }
    # Plot figure
    Args <- as.list(match.call()[-1])
    Args <- Args[which(names(Args) %in% c("variables", "bw", "conditional", "alpha"))]
    Args <- c(Args, list(plot_df = plot_df))
    density_plot <- do.call(.plot_density_fun, Args)
    # Relabel facets
    label_facets <- c(levels(plot_df$Variable), levels(plot_df$Title))
    names(label_facets) <- label_facets
    if(!is.null(facet_labels)){
      label_facets[which(tolower(names(label_facets)) %in% tolower(names(facet_labels)))] <- facet_labels[which(tolower(names(facet_labels)) %in% tolower(names(label_facets)))]
    }
    # Facet the plot
    if (length(unique(plot_df$Title)) > 1) {
      if (length(variables) > 1) {
        
        density_plot <- density_plot +
          facet_grid(Title ~ Variable, labeller = labeller(Title = label_facets, Variable = label_facets), scales = "free_x")
        
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
      theme_bw()
    
    suppressWarnings(print(density_plot))
    return(invisible(density_plot))
  }

.extract_density_data <- function(x,
                                  variables = NULL, longform = TRUE){
  if(inherits(x, "tidyProfile")){
    x <- list(x)
  }
  x[sapply(x, function(i){is.null(i[["dff"]])})] <- NULL
  # Check if all variables (except CPROBs) are identical across models
  plot_df <- lapply(x, function(x)
    as.data.frame(x$dff))
  
  
  plot_df <-
    lapply(plot_df, function(x) {
      x <- x[, which(names(x) %in% c(grep("^CPROB", names(x), value = TRUE), variables))]
      names(x) <- gsub("^CPROB", "Probability.", names(x))
      data.frame(x, Probability.Total = 1)
    })
  
  for (i in names(plot_df)) {
    plot_df[[i]][, grep("^Probability", names(plot_df[[i]]))] <-
      lapply(plot_df[[i]][grep("^Probability", names(plot_df[[i]]))], function(x) {
        x / length(x)
      })
  }
  
  plot_df <- lapply(plot_df, function(x) {
    reshape(
      x,
      direction = "long",
      varying =
        grep("^Probability", names(x), value = TRUE),
      timevar = "Class",
      idvar = "ID"
    )
  })
  
  if(length(plot_df) > 1){
    plot_df <-
      do.call(rbind, lapply(names(plot_df), function(x) {
        data.frame(Title = gsub("_", " ", x), plot_df[[x]])
      }))
  } else {
    plot_df <- data.frame(Title = "", plot_df[[1]])
  }
  
  if(longform){
    variable_names <-
      which(!(
        names(plot_df) %in% c("Title", "Class", "Probability", "ID")
      ))
    
    names(plot_df)[variable_names] <-
      sapply(names(plot_df)[variable_names], function(x) {
        paste(c(
          "Value_____",
          toupper(substring(x, 1, 1)),
          tolower(substring(x, 2))
        ), collapse = "")
      })
    
    plot_df <- reshape(
      plot_df,
      direction = "long",
      varying =
        grep("^Value", names(plot_df), value = TRUE),
      sep = "_____",
      timevar = "Variable"
    )[, c("Title", "Variable", "Value", "Class", "Probability")]
    
    plot_df$Variable <- factor(plot_df$Variable)
  }
  
  plot_df$Class <- factor(plot_df$Class)
  plot_df$Class <-
    ordered(plot_df$Class, levels = c("Total", levels(plot_df$Class)[-length(levels(plot_df$Class))]))
  plot_df
}

.plot_density_fun <- function(plot_df, variables, bw = FALSE, conditional = FALSE, alpha = .2){
  if (conditional) {
    if (bw) {
      plot_df <- plot_df[-which(plot_df$Class == "Total"),]
      density_plot <-
        ggplot(plot_df,
               aes_string(x = "Value", y = "..count..", fill = "Class", weight = "Probability")) +
        geom_density(position = "fill") + scale_fill_grey(start = 0.2, end = 0.8)
    } else {
      plot_df <- plot_df[-which(plot_df$Class == "Total"),]
      density_plot <-
        ggplot(plot_df,
               aes_string(x = "Value", y = "..count..", fill = "Class", weight = "Probability")) +
        scale_fill_manual(values = get_palette(length(levels(plot_df$Class))-1)) +
        geom_density(position = "fill")
    }
  } else {
    densities <- .get_dens_for_plot(plot_df)
    densities$alpha <- alpha
    densities$alpha[densities$Class == "Total"] <- 0
    densities$Class <- ordered(densities$Class, levels = c(levels(plot_df$Class)[-1][order(as.numeric(levels(plot_df$Class)[-1]))], levels(plot_df$Class)[1]))
    if (bw) {
      density_plot <-
        ggplot(densities,
               aes_string(x = "x",
                          y = "y",
                          linetype = "Class"
                          #size = "size"
               )) + labs(x = "Value", y = "density")
      density_plot <- density_plot +
        geom_path()+
        scale_linetype_manual(values = c(2:length(levels(plot_df$Class)), 1))+
        scale_x_continuous(expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0))
    } else{
      density_plot <-
        ggplot(densities,
               aes_string(x = "x",
                          y = "y",
                          fill = "Class",
                          colour = "Class",
                          alpha = "alpha"#,
                          #size = "size"
               )) + labs(x = "Value", y = "density")
      class_colors <- c(get_palette(length(levels(plot_df$Class))-1), "#000000")
      density_plot <- density_plot +
        scale_colour_manual(values = class_colors)+
        scale_fill_manual(values = class_colors) +
        scale_alpha_continuous(range = c(0, alpha), guide = FALSE)+
        scale_size_continuous(range = c(.5, 1), guide = FALSE)+
        geom_area(position = "identity")+
        scale_x_continuous(expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0))
      
    }
  }
  density_plot
}

#' @importFrom stats density
.get_dens_for_plot <- function(plot_df){
  vars <- unique(plot_df[["Variable"]])
  titles <- unique(plot_df[["Title"]])
  if(is.null(vars)) vars <- ""
  if(is.null(titles)) titles <- ""
  if(length(titles) < 2 ){
    if(length(vars) < 2){
      densities <- lapply(unique(plot_df$Class), function(thisclass){
        thedf <- plot_df[plot_df$Class == thisclass, ]
        thep <- thedf$Probability
        data.frame(Title = titles,
                   Variable = vars,
                   Class = thisclass,
                   suppressWarnings(density(thedf$Value, weights = thep))[c("x", "y")])
      })
      do.call(rbind, densities)
    } else {
      do.call(rbind, lapply(vars, function(thisvar){
        .get_dens_for_plot(plot_df[plot_df$Variable == thisvar, ])
      }))
    }
  } else {
    do.call(rbind, lapply(titles, function(thistit){
      .get_dens_for_plot(plot_df[plot_df$Title == thistit, ])
    }))
  }
  
}

estimates <- function(model){
  # Select means, variances, covariances of class-specific parameters; drop est_se
  df <- suppressWarnings(subset(model$parameters[["unstandardized"]], grepl("(^Means$|^Intercepts$|^Variances$|\\.WITH$)", model$parameters[["unstandardized"]]$paramHeader) & !is.na(as.numeric(model$parameters[["unstandardized"]]$LatentClass)), select = -5))
  df <- df[!df$pval == "999", ]
  # Extract original variable names
  
  varnames <- strsplit(model$input$variable$names, " ")[[1]]
  # Match original var names to names in $param column
  param_match <- sapply(df$param, pmatch, toupper(varnames))
  # Replace names in $param column
  df$param[!is.na(param_match)] <- varnames[param_match[!is.na(param_match)]]
  # Match original var names to names in $paramHeader column (remove any suffixes starting with ., like .WITH)
  param_match <- sapply(gsub("\\..*$", "", df$paramHeader), pmatch, toupper(varnames))
  # Replace these one at a time (necessary because of .WITH etc)
  if(any(!is.na(param_match))){
    param_match <- na.omit(param_match)
    for(i in 1:length(param_match)){
      if(is.na(param_match[i])) next
      df$paramHeader[i] <- gsub(names(param_match)[i], varnames[param_match[i]], df$paramHeader[i])
    }
  }
  if(!any(varnames %in% df$param)){
    varnames <- unique(model$parameters[["unstandardized"]]$param[model$parameters[["unstandardized"]]$paramHeader == "Means"])
  }
  df <- subset(df, df$param %in% varnames)
  if(is.null(df)){return(NULL)}
  covariances <- grepl(".WITH$", df$paramHeader)
  df$param[covariances] <- paste(df$paramHeader[covariances], df$param[covariances], sep = ".")
  df$paramHeader[covariances] <- "Covariances"
  df$LatentClass <- as.integer(df$LatentClass)
  names(df) <- c("Category", "Parameter", "Estimate", "se", "p", "Class")
  df[!df$p == 999, ]
}


get_palette <- function(x){
  if(x < 10){
    switch(max(x-2, 1),
           c("#E41A1C", "#377EB8", "#4DAF4A"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF"),
           c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
    )[1:x]
  } else {
    colrs <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    c(get_palette(9), sample(colrs, (x-9)))
  }
}


# Functions for selecting output elements ---------------------------------

create_all_output_fun <- function(filename = "R/mixtures.R"){
  txt <- readLines(filename)
  out <- txt[1:grep("^# Automatically generated functions below here$", txt)]
  # Prepare lists
  fun_list <- c("input", "warn_err", "data_summary", "sampstat", "covariance_coverage", "summaries",
                "invariance_testing", "parameters", "class_counts", "indirect", "mod_indices", "residuals",
                "savedata", "bparameters", "tech1", "tech3", "tech4", "tech7", "tech8",
                "tech9", "tech10", "tech12", "tech15", "fac_score_stats", "lcCondMeans", "gh5")
  
  # Get templates
  template <- txt[grepl("^#A?[XY]", txt)]
  template <- mapply(function(st, en){
    template[st:en]
  }, st = grep("@export", template), en = c((grep("@export", template)[-1]-1), length(template)))
  
  # Replace COND and NAME with different conditionals
  out <- list()
  for(i in 1:length(fun_list)){
    out <- c(out,
             lapply(template, function(x){
               gsub("NAME", fun_list[i], x)}))
  }

  template <- unlist(out)
  
  template <- gsub(pattern = "^#X ", replacement = "", template)
  template <- gsub(pattern = "^#Y ", replacement = "#' ", template)
  out <- c(txt[1:grep("^# Automatically generated functions below here$", txt)], template)
  writeLines(out, filename)
}

#' @title Extract Mplus results
#' @description This function allows users to extract elements of Mplus output
#' by name from different types of objects returned by \code{MplusAutomation}.
#' @param x Object from which to extract results.
#' @param element Which element of the results to extract.
#' @param simplify Logical; should the result be simplified to a vector, matrix
#' or higher dimensional array if possible? See \code{\link{sapply}}. Defaults
#' to \code{FALSE}.
#' @param ... Additional arguments passed to and from functions.
#' @return An atomic vector or matrix or list of the same length as X
#' (of length n for replicate). If simplification occurs,
#' the output type is determined from the highest type of the return values in
#' the hierarchy NULL < raw < logical < integer < double < complex < character <
#' list < expression, after coercion of pairlists to lists.
#' @examples
#' \dontrun{
#'  test <- mplusObject(MODEL = "mpg ON wt hp;
#'  wt WITH hp;", rdata = mtcars)
#'  res <- mplusModeler(test, modelout = "model1.inp", run = 1L)
#'  get_results(res, "summaries")
#'  unlink(res$results$input$data$file)
#'  unlink("model1.inp")
#'  unlink("model1.out")
#' }
#' @rdname get_results
#' @export
get_results <- function(x, element, simplify = FALSE, ...){
  tryCatch({
    x <- mplus_as_list(x)
    list_length <- length(x)
    out <- sapply(x, function(this_element){tryCatch(this_element[[element]], error = function(e){ NULL })}, simplify = simplify)
    if(simplify & inherits(out, "list")){
      if(all(sapply(out, inherits, c("data.frame", "matrix"))) & !is.null(names(out))){
        out <- lapply(1:length(out), function(x){
          cbind(out[[x]], Model = names(out)[x])
        })
        
        allNms <- unique(unlist(lapply(out, names)))
        
        out <- do.call(rbind,
                       c(lapply(out,
                                function(x) data.frame(c(x, sapply(setdiff(allNms, names(x)),
                                                                   function(y) NA)))),
                         make.row.names=FALSE))
      }
      
    }
    if(inherits(out, "list") & list_length == 1){
      out <- out[[1]]
    }
    return(out)
  }, error = function(e){
    return(NULL)
  })
}

# Template function
#Y @export
#Y @rdname get_results
#Y @examples
#Y out <- get_NAME(res)
#X get_NAME <- function(x, simplify = FALSE, ...){
#X   cl <- match.call()
#X   cl[["element"]] = "NAME"
#X   cl[[1L]] <- quote(get_results)
#X   eval.parent(cl)
#X }

# End template

# Automatically generated functions below here
#' @export
#' @rdname get_results
#' @examples
#' out <- get_input(res)
get_input <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "input"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_warn_err(res)
get_warn_err <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "warn_err"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_data_summary(res)
get_data_summary <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "data_summary"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_sampstat(res)
get_sampstat <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "sampstat"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_covariance_coverage(res)
get_covariance_coverage <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "covariance_coverage"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_summaries(res)
get_summaries <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "summaries"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_invariance_testing(res)
get_invariance_testing <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "invariance_testing"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_parameters(res)
get_parameters <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "parameters"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_class_counts(res)
get_class_counts <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "class_counts"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_indirect(res)
get_indirect <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "indirect"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_mod_indices(res)
get_mod_indices <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "mod_indices"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_residuals(res)
get_residuals <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "residuals"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_savedata(res)
get_savedata <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "savedata"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_bparameters(res)
get_bparameters <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "bparameters"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_tech1(res)
get_tech1 <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "tech1"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_tech3(res)
get_tech3 <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "tech3"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_tech4(res)
get_tech4 <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "tech4"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_tech7(res)
get_tech7 <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "tech7"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_tech8(res)
get_tech8 <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "tech8"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_tech9(res)
get_tech9 <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "tech9"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_tech10(res)
get_tech10 <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "tech10"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_tech12(res)
get_tech12 <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "tech12"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_tech15(res)
get_tech15 <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "tech15"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_fac_score_stats(res)
get_fac_score_stats <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "fac_score_stats"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_lcCondMeans(res)
get_lcCondMeans <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "lcCondMeans"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}
#' @export
#' @rdname get_results
#' @examples
#' out <- get_gh5(res)
get_gh5 <- function(x, simplify = FALSE, ...){
  cl <- match.call()
  cl[["element"]] = "gh5"
  cl[[1L]] <- quote(get_results)
  eval.parent(cl)
}