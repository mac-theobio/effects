library(dplyr)
library(vareffects)
library(ggplot2); theme_set(theme_bw())

library(shellpipes)
loadEnvironments()
startGraphics()

## m <- glm(z ~ x+y, data=sim_df_mediate, family="binomial")
m <- lm(z ~ x+y, data=sim_df_mediate)

def <- varpred(m , "x", modelname="def")
pred <- varpred(m, "x", isolate=FALSE, modelname="pred")
zero <- varpred(m, "x", isolate.value=-1, modelname="zero")

all <- list(def, pred, zero) %>% combinevarpred

plot(all)

