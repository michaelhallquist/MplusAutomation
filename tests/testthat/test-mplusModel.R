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
trimws(orig)
m$syntax
m$write_inp()
