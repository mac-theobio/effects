library(varpred)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=16))

library(shellpipes)
startGraphics()

loadEnvironments()

inColor = "blue"

points <- (ggplot(dat)
	+ aes(nitro, mass)
	+ geom_point()
	+ xlab("Nitrogen")
	+ ylab("Biomass")
	+ geom_point(aes(mean(nitro), mean(mass)), size=4)
)

vpoints <- (points
	+ geom_line(data=veff, aes(y=fit))
	+ geom_line(data=veff, lty=3, aes(y=lwr))
	+ geom_line(data=veff, lty=3, aes(y=upr))
)

comp <- (vpoints
	+ geom_line(data=iveff, aes(y=fit), color=inColor)
	+ geom_line(data=iveff, lty=3, aes(y=lwr), color=inColor)
	+ geom_line(data=iveff, lty=3, aes(y=upr), color=inColor)
)

print(comp)
