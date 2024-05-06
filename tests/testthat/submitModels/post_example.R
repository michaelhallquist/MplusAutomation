# simple script to show post-processing for Mplus

cat("Running post_example.R\n")

mplusdir <- Sys.getenv("MPLUSDIR")
if (nchar(mplusdir) > 0) cat("MPLUSDIR from Sys.getenv() is: ", mplusdir, "\n")

mplusinp <- Sys.getenv("MPLUSINP")
if (nchar(mplusinp) > 0) cat("MPLUSINP from Sys.getenv() is: ", mplusinp, "\n")

library(MplusAutomation)

cat("Using readModels and printing parameters.\n")
m <- readModels(file.path(mplusdir, sub(".inp", ".out", mplusinp, fixed = TRUE)))

print(m$parameters$unstandardized)

