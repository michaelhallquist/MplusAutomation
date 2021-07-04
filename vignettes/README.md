# README for MplusAutomation Vignettes

Writing vignettes for *MplusAutomation* is complicated by the 
need for commercial Mplus to be installed, which will not be available
for any continuous integration testing nor on CRAN.
This README outlines the process and steps needed to create successful vignettes.

The work flow for vignettes requiring Mplus to run is as follows.


1.  Decide on the vignette short name, say "cfacont" for Confirmatory Factor Analysis - Continuous.
2.  Create a file `shortname.Rmd.orig`, continuing with the example that would be `cfacont.Rmd.orig`. 
    This will be an `Rmarkdown` document but is our original source document that can be run 
    on machines with Mplus installed, but not on CRAN, etc.
3.  Name each `R` chunk in `shortname.Rmd.orig` based on this convention: `shortname-chunkname`.
    For example, `cfacont-model` would be the label for a chunk "model" in the vignette "cfacont".
	It is a good idea to name all chunks. However, it is **required** for chunks with figures 
	for the figure parsing to work correctly.
4.  Edit the file `vignettes/precompile.R` to for the new vignette. `precompile.R` is a script 
    that is explicitly added to `.Rbuildignore` so its never in the CRAN build. `precompile.R` 
	does the precompilation of our `shortname.Rmd.orig` vignettes into `shortname.Rmd` vignettes 
	that look like a `.Rmd` file but have already had all the code run so that Mplus is no longer 
	required. The code in `precompile.R` for a new vignette should look like this:
	`knit("vignettes/shortname.Rmd.orig", "vignettes/shortname.Rmd")`
5.  Any figures generated need to be moved into the `vignettes` folder from the top level package 
    directory. This can be done automatically using the `file.rename()` function to move them 
	and the `list.files()` function to find them all. For this to work smoothly, two things are required.
	**First** all the chunks with figures must be named consistently with the `shortname` of the 
	vignette. **Second**, the `shortname` must be unique, both from other vignettes and from any 
	package level files or directories. For example, because at the top level, there is a DESCRIPTION 
	file, that would not be a good `shortname` nor would README, NEWS, vignettes (a directory name), 
	etc.
6.  Write your vignette in the file `shortname.Rmd.orig`. In addition to the `Rmarkdown` YAML header 
    information required for the vignette and vignette index entry, `R` code is needed 
	to set the chunk options so that `fig.path = ""`. Otherwise, by default figures go into 
	a `figures` subfolder, which will break moving the figures to their correct, final location.
	The first `R` code chunk should include the following code: 
	`knitr::opts_chunk$set(fig.path = "")` and so that users do not see it, this chunk should have 
	the option `include = FALSE`.
7.  From `R` make sure your working directory is in the top level, *MplusAutomation* package directory.
    From there, you can source/run `precompile.R`. This will precompile the vignette and should 
	move all the figures to the right place. 
8.  Check that a new file `shortname.Rmd` has been generated. Check that if you have figures in the 
    vignette these are **not** in the top level package directory anymore and **are** in the 
	`vignettes` directory.
9.  Commit changes via `git`. Make sure that changed / modified files look like what you expect.

