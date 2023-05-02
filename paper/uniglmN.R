library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- glm(robustProp~nitro, data=dat, weights=numTest)

summary(mod)

saveVars(mod, dat)
