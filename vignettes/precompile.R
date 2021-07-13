## This runs knitr on .Rmd.orig files in the vignette folder that rely on Mplus
## and converts them into precompiled .Rmd files so that CRAN etc. can knit them into
## package vignettes without needing Mplus installed

## this script should be run from the top level MplusAutomation package directory
## as the working directory, otherwise the various paths and code may break

## the steps are:
## 1. knit the .Rmd.orig -> .Rmd file
## 2. find any figures generated
## 3. rename the figures so that they are moved from the top level package dir
##    into the vignettes directory

## More detailed are in the vignettes/README.md file in the vignettes folder

library(knitr)

## knit CFA - Continuous vignette
knit("vignettes/cfacont.Rmd.orig", "vignettes/cfacont.Rmd")
cfacont.figures <- list.files(pattern = "^cfacont.*")
if (length(cfacont.figures)) {
  file.rename(cfacont.figures, sprintf("vignettes/%s", cfacont.figures))
}
