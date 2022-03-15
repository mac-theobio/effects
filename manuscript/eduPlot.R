library(vareffects)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=16))
## library(ggpubfigs)

library(shellpipes)
startGraphics()

loadEnvironments()

points <- (ggplot(dat)
	+ aes(edu, con)
	+ geom_point()
	+ xlab("Education")
	+ ylab("Consumption")
)

veff <- as.data.frame(varpred(mod, "edu"))
vpred <- as.data.frame(varpred(mod, "edu", isolate=FALSE))

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
