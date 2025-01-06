# test mplusObject

m <- mplusModel(inp_file="/Users/hallquist/Data_Analysis/r_packages/MplusAutomation/tests/testthat/mplus_ug/8.11/ch8/ex8.6r3step.inp")

m <- mplusModel(inp_file="/Users/hallquist/Data_Analysis/Momentum/momentum_collab/code/ema/analysis/s1_s2_factors/07_fscores_RLla_p24_v2_fixloadings_exp.inp")

m <- mplusModel(inp_file="/Users/hallquist/Data_Analysis/Momentum/momentum_collab/code/ema/analysis/s1_s2_factors/07_fscores_RLla_p24_v2_fixloadings_exp.inp", read=FALSE)

orig <- readLines("/Users/hallquist/Data_Analysis/Momentum/momentum_collab/code/ema/analysis/s1_s2_factors/07_fscores_RLla_p24_v2_fixloadings_exp.inp")
syn <- parseMplusSyntax(orig)
recon <- mplusInpToString(syn)

syn2 <- parseMplusSyntax(recon)
recon2 <- mplusInpToString(syn2)

m <- mplusModel(syntax=orig)
identical(m$syntax, trimws(orig))

paste(m$syntax, collapse=" ")
paste(trimws(orig), collapse=" ")

trimws(orig)
m$syntax
m$write_inp()

# simple regression 
syn <- "
TITLE:	this is an example of a simple linear
	regression for a continuous observed
	dependent variable with two covariates
DATA:	FILE IS ex3.1.dat;
VARIABLE:	NAMES ARE y1 x1 x3;
MODEL:	y1 ON x1 x3;
"

m <- mplusModel(inp_file="/Users/hallquist/Data_Analysis/r_packages/MplusAutomation/tests/testthat/submitModels/ex3.1.inp")

dd <- m$data

nobj <- mplusModel(syntax=syn, data=dd, inp_file="/Users/hallquist/Data_Analysis/r_packages/MplusAutomation/tests/testthat/ex3.1_mplusmodel.inp")
nobj$run()


m <- mplusModel(inp_file="/Users/hallquist/Data_Analysis/Miscellaneous/Rens_GMM_Constraints/mcex8.1.inp")
m$run()
