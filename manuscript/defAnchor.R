library(varpred)

library(shellpipes)
loadEnvironments()

vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE, steps=200))
veff <- as.data.frame(varpred(mod, "nitro", steps=200))

saveEnvironment()

quit()

vall <- combinepreds(list(vpred, veff)) ## drop data.frame, add modelnames (or try list names??)
