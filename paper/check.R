library(varpred)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()

## summary(mod)
## summary(dat)

mm <- colMeans(model.matrix(mod))
bb <- coefficients(mod)

print(matrix(mm, nrow=1) %*% bb)
mean(dat$mass)

veff <- varpred(mod, c("nitro")
	, x.var="nitro"
	, returnall=TRUE
)
mean(veff$pred$fit)
plot(veff)
veff <- varpred(mod, "nitro", bias.adjust="observed")
mean(veff$pred$fit)
quit()

vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE, steps=200))
veff <- as.data.frame(varpred(mod, "nitro", steps=200))
iveff <- as.data.frame(varpred(mod, "nitro", steps=200, input_vars=TRUE))

saveEnvironment()

