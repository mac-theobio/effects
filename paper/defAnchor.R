library(varpred)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()

## summary(mod)

vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE, steps=200))
veff <- as.data.frame(varpred(mod, "nitro", steps=200))
iveff <- as.data.frame(varpred(mod, "nitro", steps=200, input_centering=TRUE)) 
eveff <- as.data.frame(emmeans(mod, specs=~nitro, at=list(nitro=veff$nitro)))

print(
	full_join(iveff, eveff, by="nitro")
)

saveEnvironment()

quit()

vall <- combinepreds(list(vpred, veff)) ## drop data.frame, add modelnames (or try list names??)
