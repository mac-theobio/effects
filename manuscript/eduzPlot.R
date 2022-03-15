library(vareffects)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=16))
## library(ggpubfigs)

library(shellpipes)
startGraphics()

loadEnvironments()

vzero <- as.data.frame(varpred(mod, "edu", isolate.value=0))
vpzero <- as.data.frame(varpred(mod, "edu", isolate=FALSE, isolate.value=0))

(pred
	+ geom_line(data=vpzero, lty=2, color="blue", aes(y=lwr))
	+ geom_line(data=vpzero, lty=2, color="blue", aes(y=upr))
) %>% teeGG(desc="pzero")

(eff
	+ geom_line(data=vzero, lty=2, color="blue", aes(y=lwr))
	+ geom_line(data=vzero, lty=2, color="blue", aes(y=upr))
) %>% teeGG(desc="zero")

(plain
	+ geom_line(data=vzero, lty=2, color="blue", aes(y=lwr))
	+ geom_line(data=vzero, lty=2, color="blue", aes(y=upr))
) %>% teeGG(desc="zeroonly")

