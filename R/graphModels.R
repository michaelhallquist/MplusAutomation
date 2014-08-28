#' Plot the samples for each MCMC chain as a function of iterations
#'
#' Displays a traceplot of the MCMC draws from the poster distribution of each parameter estimate for a Bayesian Mplus model.
#' This function requires that 1) PLOT: TYPE=PLOT2; be included in the Mplus input file, 2) a gh5 file be present corresponding
#' to the Mplus output file (and containing a bayesian_data section), and 3) that the rhdf5 package be installed to allow
#' the gh5 file to be imported.
#'
#' A multi-panel plot is drawn to the screen and the user is prompted to display the next plot if more than rows x columns estimates are
#' in the model.
#'
#' @note Trace and density plots can also be obtained using the coda package and the bparameters
#'   element of the mplus.model object. This requires that the posterior draws
#'   be saved using SAVEDATA: BPARAMETERS syntax. See example below.
#'
#' @param mplus.model An Mplus model extracted by the \code{readModels} function.
#' @param rows Number of rows to display per plot.
#' @param cols Optional. Number of columns to display per plot.
#' @param parameters_only Optional. If TRUE, only the unstandardized parameter estimates from the MCMC
#'   draws will be displayed (as opposed to standardized estimates, r-square estimates, etc.).
#'   The unstandardized estimates all begin with "Parameter" in the Mplus gh5 output.
#' @return No value is returned by this function.
#'   Called for the side effect of displaying an MCMC chains traceplot.
#' @author Joseph Glass, Michael Hallquist
#' @export
#' @seealso \code{\link{plot.mcmc}}
#' @examples
#' \dontrun{
#'   myModel <- readModels("BayesModel_WithGH5MCMC.out")
#'   mplus.traceplot(myModel, rows=2, cols=3)
#'
#'   #alternative using the coda package
#'   library(coda)
#'   plot(myModel$bparameters$valid_draw)
#' }
#' @keywords interface
mplus.traceplot <- function(mplus.model, rows=4, cols=4, parameters_only=TRUE) {
  #uses gh5 output, requires PLOT: TYPE=PLOT2
  
  if (!inherits(mplus.model, "mplus.model")) stop("mplus.traceplot function requires an mplus.model object (from readModels).")

  #if(!(suppressWarnings(require(rhdf5)) && suppressWarnings(require(lattice)))) stop("mplus.traceplot requires rhdf5 and lattice packages")
  if(!suppressWarnings(require(rhdf5))) {
    stop(paste(c("mplus.traceplot requires the rhdf5 package, which is not installed.\n",
                "To install, in an R session, type:\n",
                "  source(\"http://bioconductor.org/biocLite.R\")\n",
                "  biocLite(\"rhdf5\")\n")))
  }

	if (length(mplus.model$gh5) <= 0) stop("No data in gh5 element of Mplus model.")

  if (!"bayesian_data" %in% names(mplus.model$gh5)) stop("No bayesian_data element of gh5 file. Requires PLOT: TYPE=PLOT2; in Mplus input.")

  #expected structure
  # $bayesian_data - list of 1
  #  $parameters_autocorr - list of 3
  #   $statements - chr [1:302]
  #
  #   $parameters - num [1:2, 1:162200, 1:302]
  #                      ^^^  ^^^^^^^^  ^^^^^
  #                    chain  iteration parameter
  #
  #   $autocorrelation - num [1:2, 1:302, 1:30]
  #                           ^^^  ^^^^^  ^^^^
  #                          chain param  value

  #UPDATE OCT2013: NEW expected structure
  #not sure if this reflects an internal change in Mplus or shift to rhdf5 package
  # $bayesian_data - list of 2
  #  $parameters_autocorr - list of 4
  #   $statements - chr [1:302]
  #
  #   $parameters - num [1:302, 1:162200, 1:2]
  #                      ^^^^^  ^^^^^^^^  ^^^
  #                  parameter  iteration chain
  #
  #   $autocorrelation - num [1:30, 1:302, 1:2]
  #                           ^^^^  ^^^^^  ^^^^
  #                          value  param  chain
  
  
#  setwd(analysisDirectory)
#  jpeg(file="plot%d.jpg", width=8.5, height=11, units = "in", res=120)
#  col <- c("red", "blue", "green", "orange")

  parameters_autocorr <- mplus.model$gh5$bayesian_data$parameters_autocorr

  #to keep code below the same as previous versions, use aperm to rearrange as chain, iteration, parameter/value
  parameters_autocorr$parameters <- aperm(parameters_autocorr$parameters, c(3,2,1))
  parameters_autocorr$autocorrelation <- aperm(parameters_autocorr$autocorrelation, c(3,2,1))
  
  #whether to restrict to parameters, as opposed to STD, STDY, STDYX, R-SQUARE, etc.
  if (parameters_only) {
    pkeep <- grep("^Parameter", parameters_autocorr$statements)
    if (length(pkeep) > 0) {
      parameters_autocorr$statements <- parameters_autocorr$statements[pkeep]
      parameters_autocorr$parameters <- parameters_autocorr$parameters[,,pkeep, drop=FALSE]
      parameters_autocorr$autocorrelation <- parameters_autocorr$autocorrelation[,pkeep,, drop=FALSE]
    }
  }

  param.dim <- dim(parameters_autocorr$parameters)

  n.chains <- param.dim[1]
  n.iterations <- param.dim[2]
  n.parameters <- param.dim[3]

  #col <- colors()[1:n.chains]
  #Use paired palette from color brewer. Supports up to 10 qualitative colors.
  #cb.paired <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")
  cb.set1 <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999")
  col <- cb.set1[1:n.chains]

  oldAsk <- par("ask")
  par(ask=TRUE)

  par(mfrow=c(rows,cols), mar=c(5,5,3,2)+.1)
  for (i in 1:n.parameters){
    #cat("Range: ", range(parameters.autocorr$parameters[1:n.chains,,i]), "\n")

    plot(
        #x
        c(0, n.iterations),
        #y
        range(parameters_autocorr$parameters[1:n.chains,,i]),
        #options
        ylab="Estimate", xlab="Iteration", main=paste(strwrap(parameters_autocorr$statements[i], width=30), collapse="\n"),
        xaxs="i", cex.main=0.8, cex.lab=1, cex.axis=1, type="n"
    )
    for (j in 1:n.chains){
      lines (1:n.iterations, parameters_autocorr$parameters[j,1:n.iterations,i], lwd=.5, col=col[j])
    }
  }
  par(ask=oldAsk)
#  dev.off()
}





#######
#EXPERIMENTAL CODE FOR GRAPHING MODELS USING GRAPHVIZ

#test <- extractModelParameters("C:\\Program Files\\Mplus\\Mplus Examples\\User's Guide Examples\\ex5.1.out")

#' Add a node to a GRAPHVIZ model
#'
#' To do: add details
#'
#' @param dotModel Basic model
#' @param name
#' @param role The role of a variable (can be multiple)
#' @param type The type of variable
#' @return the dotModel with added node
#' @keywords internal
addNode <- function(dotModel, name, role, type) {
  if (!inherits(dotModel, "list")) stop("dotModel parameter must be a list")

  if (is.null(dotModel[[name]])) dotModel[[name]] <- list(role=role, type=type)
  else {
    #okay to convert something we thought was observed to latent (but not the other way
    if (dotModel[[name]]$type == "observed" && type == "latent") dotModel[[name]]$type <- type

    #append the role if it's not already present (vars can have many roles)
    if (!role %in% dotModel[[name]]$role) dotModel[[name]]$role <- c(dotModel[[name]]$role, role)
  }

  return(dotModel)
}

#' Connect two nodes
#'
#' To do: add details
#'
#' @param dotModel The basic model
#' @param node1 The starting node
#' @param node2 The ending node
#' @param connectionType The type of connection to add between nodes 1 and 2
#' @return NULL
#' @keywords internal
connectNodes <- function(dotModel, node1, node2, connectionType) {

}

#' Create a graphic model from Mplus
#'
#' To do: add details
#'
#' @param The model
#' @return a dot model
#' @keywords internal
graphModel <- function(model) {
  # require(plyr)
  if (!inherits(model, "data.frame")) stop("Parameter model must be a data.frame")

  byOnWith <- grep("\\.(BY|ON|WITH)$", model$paramHeader, perl=TRUE)

  #create a df with name1, connectiontype, name2
  dotModel <- list(nodes=list(), connections=list())
  connections <- a_ply(model, 1, function(row) {
        splitHeader <- strsplit(row$paramHeader, ".", fixed=TRUE)[[1]]
        varName1 <- paste(splitHeader[-length(splitHeader)], collapse=".")
        connectType <- splitHeader[length(splitHeader)]
        varName2 <- row$param

        if (connectType == "ON") {
          dotModel$nodes <<- addNode(dotModel$nodes, name=varName1, role="outcome", type="observed")
          dotModel$nodes <<- addNode(dotModel$nodes, name=varName2, role="predictor", type="observed")
          dotModel$connections <<- connectNodes(dotModel$connections, varName1, varName2, "<-")

        }
        else if (connectType == "WITH") {
          dotModel <<- addNode(dotModel$nodes, name=varName1, role="covariance", type="observed")
          dotModel <<- addNode(dotModel$nodes, name=varName2, role="covariance", type="observed")
          dotModel$connections <<- connectNodes(dotModel$connections, varName1, varName2, "<->")
        }
        else if (connectType == "BY") {
          dotModel <<- addNode(dotModel$nodes, name=varName1, role="factor", type="latent")
          dotModel <<- addNode(dotModel$nodes, name=varName2, role="indicator", type="observed")
          dotModel$connections <<- connectNodes(dotModel$connections, varName1, varName2, "->")
        }
      })

#	varTypes <- ldply(strsplit(byOnWith, ".", fixed=TRUE), function(element) {
#				varName <- paste(element[-length(element)], collapse=".")
#				if (element[length(element)] == "BY") return(data.frame(name=varName, type="latent"))
#				else return(data.frame(name=varName, type="observed"))
#			})
#
#
#	latentVars <- na.omit(unique(latentVars))

#	print(latentVars)
#	dotModel <- addNodes(list(), data.frame(name=latentVars, type="latent", stringsAsFactors=FALSE))

  return(dotModel)
}
