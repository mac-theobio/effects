library(vareffects)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=16))

library(shellpipes)
startGraphics()

loadEnvironments()

points <- (ggplot(dat)
	+ aes(nitro, mass)
	+ geom_point()
	+ xlab("Nitrogen")
	+ ylab("Biomass")
)

veff <- as.data.frame(varpred(mod, "nitro", steps=200))
vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE, steps=200))

plain <- (points
	+ geom_line(data=vpred, aes(y=fit))
)

pred <- (plain
	+ geom_line(data=vpred, lty=2, aes(y=lwr))
	+ geom_line(data=vpred, lty=2, aes(y=upr))
)
teeGG(pred, desc="pred")

eff <- (plain
	+ geom_line(data=veff, lty=2, aes(y=lwr))
	+ geom_line(data=veff, lty=2, aes(y=upr))
)
teeGG(eff, desc="eff")

predeff <- (pred
	+ geom_line(data=veff, lty=2, aes(y=lwr))
	+ geom_line(data=veff, lty=2, aes(y=upr))
)
teeGG(predeff, desc="predeff")

saveEnvironment()
