library(vareffects)
library(magrittr)
library(ggplot2); theme_set(theme_bw())

library(shellpipes)
loadEnvironments()
startGraphics()

objects()

print(pipeStar())

pred <- varpred(m, "x", isolate=FALSE, modelname="pred")
eff <- varpred(m , "x", modelname="eff")
zero <- varpred(m, "x", isolate.value=0, modelname="zero")

all <- list(pred, eff, zero) %>% combinevarpred

plot(all)
