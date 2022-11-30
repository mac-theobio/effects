
library(varpred)
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

plain <- (points
	+ geom_line(data=vpred, aes(y=fit))
)

eff <- (plain
	+ geom_line(data=veff, lty=3, aes(y=lwr))
	+ geom_line(data=veff, lty=3, aes(y=upr))
)
teeGG(eff, desc="eff")

saveEnvironment()

