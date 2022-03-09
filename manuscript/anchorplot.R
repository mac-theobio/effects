library(vareffects)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(ggpubfigs)

library(shellpipes)
loadEnvironments()
startGraphics()

tau = 360

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
zero

all <- list(pred, eff, zero) %>% combinevarpred
summary(all)

print(plot(all)
	+ scale_color_manual(
		values = c("black", friendly_pal("contrast_three")[2:3])
	)
)

