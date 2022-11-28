library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- lm(mass~poly(nitro, 2), data=dat)

summary(mod)

saveVars(mod, dat)

