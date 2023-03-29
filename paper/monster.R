library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- lm(mass~poly(nitro, 2, raw=FALSE)*P*K, data=dat)

summary(mod)

saveVars(mod, dat)

