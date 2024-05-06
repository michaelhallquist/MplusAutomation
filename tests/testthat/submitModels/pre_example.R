# simple script to show pre-processing for Mplus

cat("Running pre_example.R\n")

mplusdir <- Sys.getenv("MPLUSDIR")
if (nchar(mplusdir) > 0) cat("MPLUSDIR from Sys.getenv() is: ", mplusdir, "\n")

mplusinp <- Sys.getenv("MPLUSINP")
if (nchar(mplusinp) > 0) cat("MPLUSINP from Sys.getenv() is: ", mplusinp, "\n")

cat("Creating an arbitrary pre file pre.txt to show how this script works.\n")
writeLines(c("PRE FILE", as.character(Sys.time())), con = file.path(mplusdir, "pre.txt"))