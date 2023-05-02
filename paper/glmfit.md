dat <- rdsRead()

uniglmN.R fits a univariate response (to N only)
multiglm.R fits a multivariate response (to NPK, no interactions)
interglm.R fits multivariate with all three two-way interactions

######################################################################

## not implemented

quadN.R fits a quadratic response to N only
multiPK.R fits multivariate with one two-way interaction

monster.R is for testing machinery

######################################################################

saveVars(mod, dat)
