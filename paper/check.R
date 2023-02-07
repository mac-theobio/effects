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

veff <- varpred(mod, c("nitro", "phos", "pot")
	, x.var="nitro"
	, at=list(nitro=dat$nitro, phos=dat$phos, pot=dat$pot)
	, returnall=TRUE
)
mean(veff$pred$fit)
colMeans(veff$raw$model.matrix)
colMeans(model.matrix(mod))
plot(veff)
quit()

vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE, steps=200))
veff <- as.data.frame(varpred(mod, "nitro", steps=200))
iveff <- as.data.frame(varpred(mod, "nitro", steps=200, input_vars=TRUE))

saveEnvironment()

