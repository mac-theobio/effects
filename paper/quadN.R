library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- lm(mass~poly(nitro, 2, raw=FALSE), data=dat)

summary(mod)

saveVars(mod, dat)

