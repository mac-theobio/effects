library(shellpipes)

dat <- rdsRead()

summary(dat)

mod <- lm(mass~nitro, data=dat)

summary(mod)

saveVars(mod, dat)
veff <- as.data.frame(varpred(mod, "nitro", steps=200))
vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE, steps=200))
