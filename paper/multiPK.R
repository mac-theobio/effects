library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- lm(mass~nitro+phos*pot, data=dat)

summary(mod)

saveVars(mod, dat)
