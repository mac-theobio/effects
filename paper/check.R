library(varpred)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()


anchor <- mean(dat$nitro)

vpred <- varpred(mod, "nitro", isolate=FALSE, steps=200, isolate.value=anchor)
plot(vpred)
veff <- varpred(mod, "nitro", steps=200, isolate.value=anchor)
plot(veff)

