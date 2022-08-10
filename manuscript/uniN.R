library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- lm(mass~nitro, data=dat)

summary(mod)

saveVars(mod, dat)
