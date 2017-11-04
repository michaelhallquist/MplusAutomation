##R code to generate external Monte Carlo analyses in Mplus
basedir <- file.path(getMainDir(), "scatter_sim")
setwd(basedir)
library(MplusAutomation)
library(foreach)
library(doSNOW)

##force to 200 replications only (otherwise will be full lenth of fmm_sim, 1000)
replications <- 1:200

######
###RUN EXTERNAL MONTE CARLO ANALYSES THROUGH MPLUS
######

##use Mplus modeler to get the job done
##workdir <- "/Users/michael/mplus_workdir"
##workdir <- tempdir()
srcdir <- "/Volumes/ExternalHD/mplus_scatter"
##1) write out mplus datasets and list
##2) run external monte carlo analysis for FMM and CFA
##3) read in and save results

getSyntax <- function(fc, nc, nv, ss, sep, np, dataSection, propMeans=NULL, means=NULL, output=NULL, savedata=NULL, processors=1) {
  syntax <- paste0(
    "TITLE:\n",
    "  FitClass = ", fc, ", NClass = ", nc, ", NVar = ", nv, ", NCases = ", ss, ", LCSep = ", sep, ", NoisePct = ", np, "\n",
    dataSection,
    "VARIABLE:\n",
    "  NAMES ARE ", paste0("x", 1:nv, collapse=" "), " trueclas;\n",
    "  USEVARIABLES ARE ", paste0("x", 1:nv, collapse=" "), ";\n",
    "  CLASSES=c(", fc, ");\n",
    "ANALYSIS:\n",
    "  STARTS = 350 35;\n",
    "  TYPE = MIXTURE;\n",
    "  ESTIMATOR=MLR;\n",
    "  PROCESSORS=", processors, " 1;\n",
    "MODEL:\n",
    "  %OVERALL%\n",
    "  ", paste0("x1-x", nv, "*1"), ";\n",
    propMeans,
    "\n",
    means,
    output,
    savedata
    )
  return(syntax)
}


##Load a list of all models to be run
##Then permute the run order and spawn several workers

load(file.path(basedir, "data", "fmmMasterList_13May2013.RData"))

njobs <- 6
setDefaultClusterOptions(master="localhost", port=10290) #move away from 10187 to avoid collisions
clusterobj <- makeSOCKcluster(njobs)
registerDoSNOW(clusterobj)

#for (m in mlist) {
fmm <- foreach(m=iter(mlist), .inorder=FALSE, .multicombine=TRUE, .packages=c("MplusAutomation")) %dopar% {
  if (file.exists(m$fname_resultsRData)) {
    cat("Already processed replications:", m$fname_resultsRData, "\n")
    return(NULL) #next
  }
  
  ##verify that replications exist
  if (!file.exists(m$fname_sourceReps)) {
    cat("Cannot open replications file: ", m$fname_sourceReps, "\n")
    return(NULL) #next
  }

  if (m$fc!=3) {
    #only run FMM3 for now (to finish up)
    return(NULL) #next
  }
  
  
  load(file=m$fname_sourceReps)

  ##setup model syntax
  if (m$nc==3) {
    means <- list(
      "%c#1%\n",
      paste0("  [x1-x", m$nv, "*", -m$sep, "];\n"),
      "%c#2%\n",
      paste0("  [x1-x", m$nv, "*0];\n"),
      "%c#3%\n",
      paste0("  [x1-x", m$nv, "*", m$sep, "];\n"),
      "%c#4%\n", #for misspecified models above 3 true classes
      paste0("  [x1-x", m$nv, "*0];\n"),
      "%c#5%\n",
      paste0("  [x1-x", m$nv, "*0];\n")
      )
    propMeans <- list(
      "  !default to equal sizes\n",
      "  [c#1*0];\n",
      "  [c#2*0];\n",
      "  [c#3*0];\n",
      "  [c#4*0];\n"
      )
  } else if (m$nc==5) {
    means <- list(
      "%c#1%\n",
      paste0("  [x1-x", m$nv, "*", -2*m$sep, "];\n"),
      "%c#2%\n",
      paste0("  [x1-x", m$nv, "*", -m$sep, "];\n"),
      "%c#3%\n",
      paste0("  [x1-x", m$nv, "*0];\n"),
      "%c#4%\n",
      paste0("  [x1-x", m$nv, "*", m$sep, "];\n"),
      "%c#5%\n",
      paste0("  [x1-x", m$nv, "*", 2*m$sep, "];\n")                    
      )
    propMeans <- list(
      "  !default to equal sizes\n",
      "  [c#1*0];\n",
      "  [c#2*0];\n",
      "  [c#3*0];\n",
      "  [c#4*0];\n"
      )              
  }

  ##restrict means and proportions to the number of classes being fit (as opposed to true structure)
  if (m$fc == 2) {
    means <- do.call(paste0, means[1:4])
    propMeans <- do.call(paste0, propMeans[1:2])
  } else if (m$fc == 3) {
    means <- do.call(paste0, means[1:6])
    propMeans <- do.call(paste0, propMeans[1:3])
  } else if (m$fc == 4) {
    means <- do.call(paste0, means[1:8])
    propMeans <- do.call(paste0, propMeans[1:4])
  } else if (m$fc == 5) {
    means <- do.call(paste0, means[1:10])
    propMeans <- do.call(paste0, propMeans[1:5])
  } else { stop("Cannot fit ", m$fc, "classes.") }
  
  ##define data section for external Monte Carlo
  dataSection <- paste0(
    "DATA:\n",
    "  FILE=external_montecarlo_list.dat;\n",
    "  TYPE=MONTECARLO;\n"
    )
  outputSection <- paste0(
    "OUTPUT:\n",
    "  TECH1 TECH9;\n"
    )   

  ##write Monte Carlo syntax to file
  cat(getSyntax(m$fc, m$nc, m$nv, m$ss, m$sep, m$np, dataSection=dataSection, propMeans=propMeans, means=means, output=outputSection), file=file.path(tempdir(), "external_montecarlo.inp"))
  
  ##write each replication dataset and the list of datsets to file
  datList <- c()
  for (i in 1:min(length(fmm_sim), max(replications))) {
    dat <- cbind(fmm_sim[[i]]$X, fmm_sim[[i]]$id)
    write.table(dat, file=file.path(tempdir(), paste0("external_montecarlo_rep", sprintf("%04d", i), ".dat")), row.names=FALSE, col.names=FALSE)
    datList <- c(datList, paste0("external_montecarlo_rep", sprintf("%04d", i), ".dat"))
  }
  write.table(datList, file=file.path(tempdir(), "external_montecarlo_list.dat"), row.names=FALSE, col.names=FALSE, quote=FALSE)

  ##run external monte carlo
  runModels(tempdir(), recursive=FALSE, logFile=NULL)

  ##read in monte carlo results
  monteCarloCombined <- readModels(file.path(tempdir(), "external_montecarlo.out"))

  ##move input and output files to results directory
  file.rename(
    file.path(tempdir(), "external_montecarlo.inp"), 
    paste0("data/external_montecarlo_results/fmm", m$fc, "_n", m$ss, "_p", gsub(".", "_", m$np*100, fixed=TRUE), "_s", gsub(".", "_", m$sep, fixed=TRUE), "_c", m$nc, "_v", m$nv, ".inp")
    )
  
  file.rename(
    file.path(tempdir(), "external_montecarlo.out"), 
    paste0("data/external_montecarlo_results/fmm", m$fc, "_n", m$ss, "_p", gsub(".", "_", m$np*100, fixed=TRUE), "_s", gsub(".", "_", m$sep, fixed=TRUE), "_c", m$nc, "_v", m$nv, ".out")
    )

  ##now run each model individually and collate the results
  ##save class probabilities for each run
  savedata <- paste0("SAVEDATA:\n",
                     "  FILE=cprobs.dat;\n",
                     "  SAVE=CPROBABILITIES;\n")
  
  outputSection <- paste0(
    "OUTPUT:\n",
    "  TECH1 TECH7 TECH12;\n"
    )
  
  repResults <- list()
  for (i in 1:min(length(fmm_sim), max(replications))) {
    dataSection <- paste0(
      "DATA:\n",
      "  FILE=external_montecarlo_rep", sprintf("%04d", i), ".dat;\n"
      )

    cat(getSyntax(m$fc, m$nc, m$nv, m$ss, m$sep, m$np, dataSection=dataSection, propMeans=propMeans, means=means, output=outputSection, savedata=savedata), file=file.path(tempdir(), "individual_rep.inp"))
    runModels(tempdir(), recursive=FALSE, logFile=NULL)
    repResults[[i]] <- readModels(file.path(tempdir(), "individual_rep.out"))
    
  }
  
  ##add simulation parameters to object for clarity
  attr(repResults, "f") <- m$fc  #number of classes fit
  attr(repResults, "n") <- m$ss  #sample size
  attr(repResults, "p") <- m$np  #noise proportion
  attr(repResults, "s") <- m$sep #latent separation
  attr(repResults, "c") <- m$nc  #number of true classes
  attr(repResults, "v") <- m$nv  #number of variables
  save(repResults, monteCarloCombined, file=m$fname_resultsRData, compress="xz", compression_level=9)
  
  ##cleanup
  unlink(c(
    file.path(tempdir(), "external_montecarlo_rep*.dat"),
    file.path(tempdir(), "*.tst"), #sometimes left behind by Mplus
    file.path(tempdir(), "external_montecarlo_rep*.dat"),
    file.path(tempdir(), "external_montecarlo_list.dat"),
    file.path(tempdir(), "individual_rep.inp"),
    file.path(tempdir(), "individual_rep.out"),
    file.path(tempdir(), "cprobs.dat")
    ))          

  return(m$fname_sourceReps)
}

stopCluster(clusterobj)
