library(vareffects)
library(dplyr)
library(ggplot2); theme_set(theme_bw())

library(shellpipes)
loadEnvironments()
startGraphics()

objects()

print(pipeStar())

b <- bind_rows(NULL
	, pred=as.data.frame(varpred(m, "x", isolate=FALSE))
	, eff=as.data.frame(varpred(m, "x"))
	, zero=as.data.frame(varpred(m, "x", isolate.value=0))
	, .id="method"
)

summary(b)

pred <- varpred(m, "x", isolate=FALSE, modelname="pred")
eff <- varpred(m , "x", modelname="eff")
zero <- varpred(m, "x", isolate.value=0, modelname="zero")

all <- list(pred, eff, zero) %>% combinevarpred

colfun <- function(n){return(hcl.colors(n))}

## Why can't I add a colour scale? I have tried many things
## I can't even add a regular scale thing
print(plot(all)
	## + scale_colour_viridis_d(option="magma")
	## + scale_color_manual(palette=colfun)
	## + scale_color_manual(values = hcl.colors(6, "Harmonic")) 
)

