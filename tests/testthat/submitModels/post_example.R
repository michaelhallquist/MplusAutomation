# simple script to show post-processing for Mplus
mplusdir <- Sys.getenv("MPLUSDIR")
mplusinp <- Sys.getenv("MPLUSINP")
if (nchar(mplusinp) > 0) cat("MPLUSINP from Sys.getenv() is: ", mplusinp, "\n")

library(MplusAutomation)

# parse output file and save to disk as rds object
m <- readModels(file.path(mplusdir, sub(".inp", ".out", mplusinp, fixed = TRUE)))

saveRDS(m, file.path(mplusdir, sub(".inp", ".rds", mplusinp, fixed=TRUE)))

