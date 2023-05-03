library(varpred)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()
startGraphics()

## summary(mod)
## summary(dat)

steps <- nrow(dat)

veff <- varpred(mod, "nitro", steps=steps
	, bias.adjust="observed"
	, modelname="obs-eff"
)

vpred <- varpred(mod, "nitro", steps=steps
	, bias.adjust="observed"
	, isolate=FALSE
	, modelname="obs-pred"
)

mveff <- varpred(mod, "nitro", steps=steps
	, bias.adjust="none"
	, isolate = TRUE
	, modelname="mean-eff"
)

mvpred <- varpred(mod, "nitro", steps=steps
	, isolate=FALSE
	, bias.adjust="none"
	, modelname="mean-pred"
)

preds <- combinevarpred(list(veff, vpred, mveff, mvpred), plotit=TRUE)
print(preds)

saveEnvironment()

