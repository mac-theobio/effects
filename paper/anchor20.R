library(varpred)

library(shellpipes)
loadEnvironments()

anchor <- 20

vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE, steps=200, isolate.value=anchor))
veff <- as.data.frame(varpred(mod, "nitro", steps=200, isolate.value=anchor))

saveEnvironment()
