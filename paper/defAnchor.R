library(varpred)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()

## summary(mod)
## summary(dat)

vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE, steps=200))
veff <- as.data.frame(varpred(mod, "nitro", steps=200))
iveff <- as.data.frame(varpred(mod, "nitro", steps=200, input_vars=TRUE))

saveEnvironment()

