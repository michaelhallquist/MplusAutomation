setwd(file.path(getMainDir(), "scatter_sim", "output"))# "data", "external_montecarlo_results"))
rdatFiles <- list.files(pattern="fmm3.*c3_v5\\.RData")
library(ggplot2)
library(plyr)
library(reshape2)
library(grid)

#get a plot of a single dataset just to illustrate the data
#load("/Volumes/ExternalHD/mplus_scatter/n720/p10/s2/c3_v5.RData")
load("C:/Users/mnh5174/Downloads/n720_p15_s2_c3_v5.RData")
png("n720_15pct_example.png", width=12, height=8, units="in", res=240)
df <- cbind(fmm_sim[[1]]$X, id=factor(fmm_sim[[1]]$id, levels=c(1,2,3,0), labels=c("Class 1", "Class 2", "Class 3", "Noise"), ordered=TRUE))
df.melt <- melt(df, id.vars="id")
g_noise <- ggplot(df.melt, aes(x=value, fill=id)) + geom_histogram(bins=20) + 
    #scale_color_brewer("Latent Class", palette="Set1") +
    scale_fill_brewer("Latent Class", palette="Set1") + 
    facet_wrap(~variable) + theme_bw(base_size=16) + ylab("Count") + xlab("Value") +
    theme(legend.key.size=unit(1.5, "lines")) + scale_x_continuous(breaks=c(-5,0,5))
plot(g_noise)
dev.off()

#load("/Volumes/ExternalHD/mplus_scatter/n720/p0/s2/c3_v5.RData")
load("C:/Users/mnh5174/Downloads/n720_p0_s2_c3_v5.RData")
png("n720_0pct_example.png", width=12, height=8, units="in", res=240)
df <- cbind(fmm_sim[[1]]$X)
df$id <- factor(df$id, levels=c(1,2,3), labels=c("Class 1", "Class 2", "Class 3"), ordered=TRUE)
names(df) <- c("x1", "x2", "x3", "x4", "x5", "id")
df.melt <- melt(df, id.vars="id")

g_clean <- ggplot(df.melt, aes(x=value, color=id, fill=id)) + geom_histogram(bins=20) + 
    scale_color_brewer("Latent Class", palette="Set1") +
    scale_fill_brewer("Latent Class", palette="Set1") + 
    facet_wrap(~variable) + theme_bw(base_size=16) + ylab("Count") + xlab("Value") +
    theme(legend.key.size=unit(1.5, "lines")) + scale_x_continuous(breaks=c(-5,0,5))
plot(g_clean)
dev.off()

pdf("fig2_lpm_structure_p15.pdf", width=7, height=5)
plot(g_noise)
dev.off()

#two panel figure for publication
library(cowplot)
pdf("fig2_lpm_structure.pdf", width=9, height=5)
plot_grid(g_clean + guides(fill=FALSE, color=FALSE), g_noise, labels = c("a)", "b)"),
		rel_widths=c(1, 1.4))
dev.off()

allCoverage <- c()
badCoverage <- .90
for (f in rdatFiles) {
  load(f)
  
  allAICC <- do.call(rbind.fill, sapply(repResults, "[", "summaries"))$AICC
  allEntropy <- do.call(rbind.fill, sapply(repResults, "[", "summaries"))$Entropy
  avgEntropy <- mean(allEntropy, na.rm=TRUE)
  AICC_Mean <- mean(allAICC, na.rm=TRUE)
  AICC_SD <- sd(allAICC, na.rm=TRUE)
  params <- monteCarloCombined$parameters$unstandardized
  propBadCoverage <- with(params, sum(as.numeric(params[paramHeader=="Means", "cover_95"] < badCoverage))/nrow(params[paramHeader=="Means",]))
  avgMCoverage <- with(params, mean(params[paramHeader=="Means", "cover_95"]))
  avgVCoverage <- with(params, mean(params[paramHeader=="Variances", "cover_95"]))
  avgM_MSE <- with(params, mean(params[paramHeader=="Means", "mse"]))
  avgV_MSE <- with(params, mean(params[paramHeader=="Variances", "mse"]))
  pconverged <- sum(!is.na(allAICC))/length(allAICC)

#  AICC_Mean <- with(monteCarloCombined$summaries, AIC_Mean + (2*Parameters*(Parameters+1))/(attr(repResults, "n")-Parameters-1))
  simDetails <- as.data.frame(attributes(repResults))
  if (! "f" %in% names(simDetails)) simDetails$f <- 3 #initial runs did not vary fitted # latent classes
  simDetails$model <- "FMM"
  #propGoodCoverage
  allCoverage <- rbind(allCoverage,
#      data.frame(simDetails, pbadcov=propBadCoverage, monteCarloCombined$summaries[c("AIC_Mean", "AIC_SD", "BIC_Mean", "BIC_SD")],
#              AICC_Mean=AICC_Mean
#      )
      data.frame(simDetails, monteCarloCombined$summaries[c("AIC_Mean", "AIC_SD", "BIC_Mean", "BIC_SD")],
          AICC_Mean=AICC_Mean, AICC_SD=AICC_SD, pconverged=pconverged, avgMCoverage=avgMCoverage,
          avgVCoverage=avgVCoverage, avgM_MSE=avgM_MSE, avgV_MSE=avgV_MSE, avgEntropy=avgEntropy
      )
  )
}

allCoverage.fmm <- allCoverage

#ggplot(allCoverage, aes(x=factor(p), y=AICC_Mean, color=factor(s), shape=factor(n))) + geom_point(size=3) + theme_bw(base_size=24)
#
#allCoverage.melt <- melt(subset(allCoverage, select=-c(BIC_SD, AIC_SD, pbadcov, c, v)), id.vars=c("n", "p", "s"))
#
#ggplot(allCoverage.melt, aes(x=factor(p), y=value, color=factor(s), shape=factor(n))) + geom_point(size=3) + theme_bw(base_size=24) + facet_wrap(~variable)

#cfa
cfaFiles <- list.files(pattern="cfa.*c3_v5\\.RData")
for (f in cfaFiles) {
  load(f)

  params <- monteCarloCombined$parameters$unstandardized
  allAICC <- do.call(rbind.fill, sapply(repResults, "[", "summaries"))$AICC
  AICC_Mean <- mean(allAICC, na.rm=TRUE)
  AICC_SD <- sd(allAICC, na.rm=TRUE)
  pconverged <- sum(!is.na(allAICC))/length(allAICC)
  avgMCoverage <- with(params, mean(params[paramHeader=="Means", "cover_95"]))
  avgVCoverage <- with(params, mean(params[paramHeader=="Variances", "cover_95"]))
  avgM_MSE <- with(params, mean(params[paramHeader=="Means", "mse"]))
  avgV_MSE <- with(params, mean(params[paramHeader=="Variances", "mse"]))
  
#  AICC_Mean <- with(monteCarloCombined$summaries, AIC_Mean + (2*Parameters*(Parameters+1))/(attr(repResults, "n")-Parameters-1))
  simDetails <- as.data.frame(attributes(repResults))
  if (! "f" %in% names(simDetails)) simDetails$f <- 3 #initial runs did not vary fitted # latent classes
  simDetails$model <- "CFA"
  #propGoodCoverage
  allCoverage <- rbind(allCoverage,
#      data.frame(simDetails, pbadcov=propBadCoverage, monteCarloCombined$summaries[c("AIC_Mean", "AIC_SD", "BIC_Mean", "BIC_SD")],
#          AICC_Mean=AICC_Mean
#      )
      data.frame(simDetails, monteCarloCombined$summaries[c("AIC_Mean", "AIC_SD", "BIC_Mean", "BIC_SD")],
          AICC_Mean=AICC_Mean, AICC_SD=AICC_SD, pconverged=pconverged, avgMCoverage=avgMCoverage,
          avgVCoverage=avgVCoverage, avgM_MSE=avgM_MSE, avgV_MSE=avgV_MSE, avgEntropy=0
      )
  )
}


allCoverage.diff <- ddply(allCoverage, .(n, p, s, c, v), function(subdf) {
      if (nrow(subdf) < 2) {
#        cat("ZIP\n")
        return(NULL)
      } else {
        diffDF <- data.frame(
            AICdiff=subdf[which(subdf$model == "CFA"),"AIC_Mean"] - subdf[which(subdf$model == "FMM"),"AIC_Mean"],
            BICdiff=subdf[which(subdf$model == "CFA"),"BIC_Mean"] - subdf[which(subdf$model == "FMM"),"BIC_Mean"],
            AICCdiff=subdf[which(subdf$model == "CFA"),"AICC_Mean"] - subdf[which(subdf$model == "FMM"),"AICC_Mean"] 
        )
        return(diffDF)
        
      }
      
    })

#selection of c3 v5 should be handled above by file filter
#allCoverage.diff <- subset(allCoverage.diff, c==3 & v==5)
#allCoverage.fmm <- subset(allCoverage, model="FMM" & c==3 & v==5)

diff.melt <- melt(subset(allCoverage.diff, select=-c(c,v)), id.vars=c("n", "p", "s"))
diff.melt$n <- ordered(diff.melt$n)
diff.melt$s <- ordered(diff.melt$s)
diff.melt$p <- ordered(diff.melt$p*100)
diff.melt$variable <- factor(diff.melt$variable, levels=c("BICdiff", "AICdiff", "AICCdiff"), labels=c("BIC", "AIC", "AICC"))

setwd(file.path(getMainDir(), "scatter_sim", "output"))
save(allCoverage, allCoverage.diff, diff.melt, file=file.path(getMainDir(), "scatter_sim", "data", "modelSelectionDiffs.RData"))

#ggplot(diff.melt, aes(x=factor(p), y=value, color=factor(s), shape=factor(n))) + geom_point(size=3) + theme_bw(base_size=24) + facet_wrap(~variable)
#png("initial_diffs.png", width=14, height=11, units="in", res=300)
#ggplot(diff.melt, aes(x=p, y=value, color=s)) + geom_point(size=3) + theme_bw(base_size=24) + facet_grid(n~variable, scales="free_y")
#dev.off()

#plots of 
for (ss in unique(diff.melt$n)) {
  thisDF <- subset(diff.melt, n==ss) #gdata::drop.levels(
  png(paste0("initial_diffs_", ss, ".png"), width=10, height=7.5, units="in", res=200)
  ymax <- max(thisDF$value)
  ymin <- min(thisDF$value)
  lowerrectDF <- ddply(thisDF, .(s), function(subdf) {
        data.frame(xmin=0.5, xmax=8.5, ymin=min(subdf$value), ymax=-10)
      })
  lowerrectDF <- subset(lowerrectDF, ymin < ymax) #only retain lower DF where there are models where CFA bests FMM
  upperrectDF <- ddply(thisDF, .(s), function(subdf) {
        data.frame(xmin=0.5, xmax=8.5, ymin=10, ymax=max(subdf$value))
      })
  upperrectDF <- subset(upperrectDF, ymax > ymin) #only retain upper DF where there are models where FMM bests CFA
  
  p <- ggplot(thisDF, aes(x=p, y=value, group=1)) + geom_point(size=3) + geom_line() + theme_bw(base_size=15) + 
      facet_grid(s~variable, scales="free_y") + ggtitle(paste0("N = ", ss)) + geom_hline(yintercept=0, size=2, alpha=0.6) + geom_hline(yintercept=c(-10,10), alpha=0.3, size=1.2) +
      geom_rect(data=upperrectDF, aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.2) + 
      geom_rect(data=lowerrectDF, aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="blue", alpha=0.2) +
      theme(panel.margin = unit(0.4, "lines")) + ylab(expression(paste(Delta, "IC (CFA - FMM)"))) + xlab("\n% Noise Observations")
  print(p)
  dev.off()
}

##MplusAutomation publication plot
ss=180
library(ggplot2); library(plyr)

thisDF <- subset(diff.melt, n==ss & s %in% c("0.5", "1", "1.5", "2") & variable %in% c("BIC", "AICC")) #gdata::drop.levels(
levels(thisDF$s) <- paste("d =", levels(thisDF$s))
ymax <- max(thisDF$value); ymin <- min(thisDF$value)
lowerrectDF <- ddply(thisDF, .(s), function(subdf) {
			data.frame(xmin=0.5, xmax=8.5, ymin=min(subdf$value) - 2, ymax=-10)
		})
lowerrectDF <- subset(lowerrectDF, ymin < ymax) #only retain lower DF where there are models where CFA bests FMM
upperrectDF <- ddply(thisDF, .(s), function(subdf) {
			data.frame(xmin=0.5, xmax=8.5, ymin=10, ymax=max(subdf$value) + 2)
		})
upperrectDF <- subset(upperrectDF, ymax > ymin) #only retain upper DF where there are models where FMM bests CFA

pdf("fig3_IC_LCA_CFA.pdf", width=8, height=8)
p <- ggplot(thisDF, aes(x=p, y=value, group=1)) + geom_point(size=3) + geom_line() + theme_bw(base_size=15) + 
		facet_grid(s~variable, scales="free_y") + geom_hline(yintercept=0, size=1.5, alpha=0.6) + geom_hline(yintercept=c(-10,10), alpha=0.3, size=1.2) +
		geom_rect(data=upperrectDF, aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.2) + 
		geom_rect(data=lowerrectDF, aes(x=NULL, y=NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="blue", alpha=0.2) +
		theme(panel.spacing = unit(0.4, "lines")) + ylab(expression(paste("Mean ", Delta, "IC (CFA - LCA)"))) + xlab("\n% Noise Observations")
plot(p)
dev.off()

#plot convergence (restrict to c3 v5)
#converge.melt <- melt(subset(allCoverage.fmm, c==3 & v==5, select=c(n,p,s,pconverged)), id.vars=c("n", "p", "s"))
#for (ss in unique(converge.melt$n)) {
#  thisDF <- subset(converge.melt, n==ss) #gdata::drop.levels(
#  png(paste0("converge_", ss, ".png"), width=14, height=11, units="in", res=300)
#  p <- ggplot(thisDF, aes(x=p, y=value, group=1)) + geom_point(size=3) + geom_line() + theme_bw(base_size=16) + facet_grid(s~variable, scales="free_y") + ggtitle(paste0("N = ", ss)) + ylab("proportion converged") +
#      scale_y_continuous(breaks=c(0,0.5,1.0))
#  print(p)
#  dev.off()
#}

#plot mse
mse.melt <- melt(subset(allCoverage.fmm, select=c(n,p,s,avgM_MSE)), id.vars=c("n", "p", "s"))
for (ss in unique(mse.melt$n)) {
  thisDF <- subset(mse.melt, n==ss) #gdata::drop.levels(
  png(paste0("meanMSE_", ss, ".png"), width=14, height=11, units="in", res=300)
  p <- ggplot(thisDF, aes(x=p, y=value, group=1)) + geom_point(size=3) + geom_line() + theme_bw(base_size=24) + facet_grid(s~variable) + ggtitle(paste0("N = ", ss)) + ylab("proportion converged")
  print(p)
  dev.off()
}

#plot avg coverage

cover.melt <- melt(subset(allCoverage.fmm, select=c(n,p,s,avgMCoverage)), id.vars=c("n", "p", "s"))
cover.melt$n <- ordered(cover.melt$n)
cover.melt$s <- ordered(cover.melt$s)
cover.melt$p <- ordered(cover.melt$p*100)

for (ss in unique(cover.melt$n)) {
  thisDF <- subset(cover.melt, n==ss) #gdata::drop.levels(
  png(paste0("meanCoverage_", ss, ".png"), width=10, height=7.5, units="in", res=200)
  p <- ggplot(thisDF, aes(x=p, y=value, group=1)) + geom_point(size=3) + geom_line() + theme_bw(base_size=15) + facet_grid(s~.) + 
      ggtitle(paste0("N = ", ss)) + ylab("Average parameter coverage of latent means") +
      xlab("\n% Noise Observations") + scale_y_continuous(breaks=c(.25, .5, .75))
  print(p)
  dev.off()
}

cover.melt <- melt(subset(allCoverage.fmm, select=c(n,p,s,avgMCoverage)), id.vars=c("n", "p", "s"))
cover.melt$n <- ordered(cover.melt$n)
cover.melt$s <- ordered(cover.melt$s)
cover.melt$p <- ordered(cover.melt$p*100)

png(paste0("meanCoverageAll.png"), width=10, height=7.5, units="in", res=200)
p <- ggplot(cover.melt, aes(x=p, y=value, group=1)) + geom_point(size=3) + geom_line() + theme_bw(base_size=15) + facet_grid(s~n) + 
    ggtitle("Average Parameter Coverage") + ylab("Average parameter coverage of latent means") +
    xlab("\n% Noise Observations") + scale_y_continuous(breaks=c(.25, .5, .75)) + geom_hline(yintercept=0.8) + scale_x_discrete(breaks=c("0", "10", "20", "30"))
print(p)
dev.off()



#plot avg entropy
entropy.melt <- melt(subset(allCoverage.fmm, select=c(n,p,s,avgEntropy)), id.vars=c("n", "p", "s"))
entropy.melt$n <- ordered(entropy.melt$n)
entropy.melt$s <- ordered(entropy.melt$s)
entropy.melt$p <- ordered(entropy.melt$p*100)

png(paste0("meanEntropyAll.png"), width=10, height=7.5, units="in", res=200)
p <- ggplot(entropy.melt, aes(x=p, y=value, group=1)) + geom_point(size=3) + geom_line() + theme_bw(base_size=15) + facet_grid(s~n) + 
    ggtitle("Average Entropy") + ylab("Average entropy") +
    xlab("\n% Noise Observations") + scale_y_continuous(breaks=c(.25, .5, .75)) + scale_x_discrete(breaks=c("0", "10", "20", "30"))
print(p)
dev.off()


for (ss in unique(entropy.melt$n)) {
  thisDF <- subset(entropy.melt, n==ss) #gdata::drop.levels(
  png(paste0("meanEntropy_", ss, ".png"), width=14, height=11, units="in", res=300)
  p <- ggplot(thisDF, aes(x=p, y=value, group=1)) + geom_point(size=3) + geom_line() + theme_bw(base_size=24) + facet_grid(s~variable) + ggtitle(paste0("N = ", ss)) + ylab("proportion converged")
  print(p)
  dev.off()
}








