
library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- glm(robustProp~nitro*phos*pot, data=dat, weights=numTest, family=binomial)

summary(mod)

saveVars(mod, dat)
