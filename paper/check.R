library(varpred)
library(dplyr)
library(emmeans)

library(shellpipes)
loadEnvironments()

vpred <- varpred(mod, "nitro")
print(vpred)

names(mod)
names(mod$model)
