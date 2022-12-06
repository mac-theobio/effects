library(varpred)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()

vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE, steps=200))
veff <- as.data.frame(varpred(mod, "nitro", steps=200))
## iveff <- as.data.frame(varpred(mod, "nitro", steps=200, input_centering=TRUE)) 
iveff <- as.data.frame(varpred(mod, "nitro", steps=200, emmeans_centered=TRUE)) 
eveff <- as.data.frame(emmeans(mod, specs=~nitro, at=list(nitro=veff$nitro)))

print(
	full_join(vpred, eveff, by="nitro")
)

saveEnvironment()

quit()

vall <- combinepreds(list(vpred, veff)) ## drop data.frame, add modelnames (or try list names??)
