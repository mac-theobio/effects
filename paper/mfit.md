dat <- rdsRead()

uniN.R fits a univariate response (to N only)
quadN.R fits a quadratic response to N only
multi.R fits a multivariate response (to NPK)
multiPK.R fits multivariate with one two-way interactions
inter.R fits multivariate with three two-way interactions

saveVars(mod, dat)
