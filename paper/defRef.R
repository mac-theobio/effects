library(varpred)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()

## summary(mod)
## summary(dat)

steps <- 200
vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE, steps=steps))
veff <- as.data.frame(varpred(mod, "nitro", steps=steps))
iveff <- as.data.frame(varpred(mod, "nitro", steps=steps, input_vars=TRUE))

saveEnvironment()

