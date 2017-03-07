#simulate replications from a 3-class latent class model
#varying sample size, mean indicator separation, and noise proportion

setwd("/Volumes/HD/scatter_sim")
srcdir <- "/Volumes/HD/scatter_sim"

source("code/mixsim_generation_functions.R") #extensions of MixSim functions for data simulation

sampsize <- c(45, 90, 180, 360, 720, 1440)
separation <- c(0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0)
noiseProp <- c(0, .025, .05, .10, .15, .20, .25, .30)
replications <- 1:1000
nc <- 3 #number of classes used to simulate data from latent class model
nv <- 5 #number of variables in latent class model
fitClasses <- 2:5 #number of classes in LPMs to be fit

library(plyr); library(foreach); library(doSNOW); library(MASS)

njobs <- 12 #number of cores to use for parallel data generation
clusterobj <- makeSOCKcluster(njobs); registerDoSNOW(clusterobj)

for (ss in sampsize) {
  for (sep in separation) {
    for (np in noiseProp) {
      dirTarget <- paste0(srcdir, "/n", ss, "/p", gsub(".", "_", np*100, fixed=TRUE), "/s", gsub(".", "_", sep, fixed=TRUE))
      fileTarget <- paste0(dirTarget, "/c", nc, "_v", nv, ".RData")
      
      #do not re-run simulation if data already exist
      if (file.exists(fileTarget)) { cat("Skip, file exists:", fileTarget, "\n"); next }
      
      fmm_sim <- foreach(r=iter(replications), .inorder=FALSE, .multicombine=TRUE, .packages=c("MASS", "plyr")) %dopar% {
        fmm_pi <- rep(1/nc, nc) #equal class sizes
        fmm_mu <- rbind(c1=rep(-sep, nv), c2=rep(0, nv), c3=rep(sep, nv)) #setup latent mean separation
        
        fmm_s <- array(data=NA_real_, dim=c(nv, nv, nc), dimnames=list(v=paste0("x", 1:nv), v=paste0("x", 1:nv), c=paste0("c", 1:nc)))
        fmm_s <- aaply(fmm_s, 3, function(cmat) {
          diag(cmat) <- 1 #variance of 1 for each indicator
          cmat[upper.tri(cmat)] <- cmat[lower.tri(cmat)] <- 0 #no correlation among indicators
          return(cmat)
        })
        
        fmm_s <- aperm(fmm_s, c(2,3,1)) #rearrange to maintain variables x variables x classes structure
        
        sim <- simdata(n=ss, Pi=fmm_pi, Mu=fmm_mu, S=fmm_s, n.out=0) #draw mixture replication using mean and covariance structure
        id <- sim$id #track the class membership vector
        
        #get IQR-based outliers for each variable
        IQRoutlier <- apply(sim$X, 2, function(vec) {
          iqr <- IQR(vec); q25 <- quantile(vec, .25); q75 <- quantile(vec, .75)
          ohigh <- q75 + iqr; olow <- q25 - iqr
          return(c(olow, ohigh))
        })
        
        #use union of outlier values across variables to define noise range
        noise.range <- c(min(IQRoutlier[1,]), max(IQRoutlier[2,]))
        n.outliers <- ceiling(ss*np)
        alpha <- .01 #ellipsoid bounds for outlier definition
        
        if (n.outliers > 0) {
          #now simulate noise (outside of 99% ellipsoidal bounds)
          outliers <- genOutliers(n.out=n.outliers, Pi=fmm_pi, Mu=fmm_mu, S=fmm_s, alpha=alpha, max.out=1e06, int=noise.range)
          
          sim <- rbind(sim$X, outliers$X.out)
          id <- c(id, rep(0, n.outliers)) #noise observations have a class value of 0
        }
        
        return(list(X=as.data.frame(sim), id=id, rep=r, #return a list containing simulated data, input, etc.
                    input=list(Pi=fmm_pi, Mu=fmm_mu, S=fmm_s, n=ss, n.outliers=n.outliers, alpha=alpha, noise.range=noise.range)
        ))
      }
      
      dir.create(dirTarget, recursive=TRUE, showWarnings=FALSE) #verify that directory is in place
      save(fmm_sim, file=fileTarget, compress="xz", compression_level=9) #output replications for this cell
    }
  }
}

stopCluster(clusterobj) #terminate parallel processing cluster

##Build a list of all models to be run
##For each cell in the simulation design, add elements to the cfalist and lpm list objects
##that store a queue of models to be run for confirmatory factor and latent class models,
##respectively. For latent class models, fit the data from 2-5 classes (where they were
##simulated from a 3-class model)

lcalist <- list()
cfalist <- list()
for (ss in sampsize) {
  for (sep in separation) {
    for (np in noiseProp) {
      fname_sourceReps <- paste0(srcdir, "/n", ss, "/p", gsub(".", "_", np*100, fixed=TRUE), "/s", gsub(".", "_", sep, fixed=TRUE), "/c", nc, "_v", nv, ".RData")
      fname_resultsRData <- paste0("data/external_montecarlo_results/cfa_n", ss, "_p", gsub(".", "_", np*100, fixed=TRUE), "_s", gsub(".", "_", sep, fixed=TRUE), "_c", nc, "_v", nv, ".RData")
      mname <- paste0("cfa_n", ss, "_p", gsub(".", "_", np*100, fixed=TRUE), "_s", gsub(".", "_", sep, fixed=TRUE), "_c", nc, "_v", nv) #model name
      
      cfalist[[mname]] <- list(nc=nc, nv=nv, ss=ss, sep=sep, np=np, fname_sourceReps=fname_sourceReps, fname_resultsRData=fname_resultsRData)
      
      for (fc in fitClasses) {
        fname_sourceReps <- paste0(srcdir, "/n", ss, "/p", gsub(".", "_", np*100, fixed=TRUE), "/s", gsub(".", "_", sep, fixed=TRUE), "/c", nc, "_v", nv, ".RData")
        fname_resultsRData <- paste0("data/external_montecarlo_results/fmm", fc, "_n", ss, "_p", gsub(".", "_", np*100, fixed=TRUE), "_s", gsub(".", "_", sep, fixed=TRUE), "_c", nc, "_v", nv, ".RData")
        mname <- paste0("fmm", fc, "_n", ss, "_p", gsub(".", "_", np*100, fixed=TRUE), "_s", gsub(".", "_", sep, fixed=TRUE), "_c", nc, "_v", nv) #model name
        
        lcalist[[mname]] <- list(nc=nc, nv=nv, ss=ss, sep=sep, np=np, fc=fc, fname_sourceReps=fname_sourceReps, fname_resultsRData=fname_resultsRData)
        
      }
    }
  }
}

save(lcalist, file=file.path(basedir, "data", "lcaMasterList.RData"))
save(cfalist, file=file.path(basedir, "data", "cfaMasterList.RData"))
