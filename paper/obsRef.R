library(varpred)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()

## summary(mod)
## summary(dat)

steps <- 200

veff <- as.data.frame(varpred(mod, "nitro", steps=steps
	, bias.adjust="observed"
))

vpred <- as.data.frame(varpred(mod, "nitro", steps=steps
	, bias.adjust="observed"
	## , isolate=FALSE
))

iveff <- as.data.frame(varpred(mod, "nitro", steps=steps
	, bias.adjust="observed"
	, input_vars=TRUE
))

saveEnvironment()

