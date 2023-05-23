
library(varpred)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=16))

library(shellpipes)
startGraphics()

loadEnvironments()

vpred <- as.data.frame(varpred(mod, "nitro", isolate=FALSE))
veff <- as.data.frame(varpred(mod, "nitro"))

points <- (ggplot(binned_df)
	+ aes(nitro, status)
	+ geom_point()
	+ xlab("Nitrogen")
	+ ylab("Status")
)

plain <- (points
	+ geom_line(data=vpred, aes(y=fit))
)

pred <- (plain
	+ geom_line(data=vpred, lty=2, aes(y=lwr))
	+ geom_line(data=vpred, lty=2, aes(y=upr))
)
teeGG(pred, desc="pred")

eff <- (plain
	+ geom_line(data=veff, lty=3, aes(y=lwr))
	+ geom_line(data=veff, lty=3, aes(y=upr))
)
teeGG(eff, desc="eff")

predeff <- (pred
	+ geom_line(data=veff, lty=3, aes(y=lwr))
	+ geom_line(data=veff, lty=3, aes(y=upr))
)
teeGG(predeff, desc="predeff")

saveEnvironment()

