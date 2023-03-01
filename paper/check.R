library(varpred)
library(ggplot2)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()

summary(mod)

mean_biomass <- mean(dat$mass)
mean_nitro=mean(dat$nitro)

vpred <- varpred(mod, "nitro")
print(plot(vpred) 
	+ geom_point(aes(x=mean_nitro, y=mean_biomass, size=3))
	+ geom_hline(yintercept=mean(vpred$preds$fit), colour="red")
)
print(mean_biomass)
print(mean(vpred$preds$fit))


