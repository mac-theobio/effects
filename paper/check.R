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
plot(vpred) + geom_point(aes(x=mean_nitro, y=mean_biomass, size=3))
print(vpred)

vpred <- varpred(mod, "nitro", isolate=FALSE)
plot(vpred)
print(vpred)

names(mod)
names(mod$model)
