library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- lm(mass~nitro+phos, data=dat)

summary(mod)

saveVars(mod, dat)
