
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

comp <- (points
	+ geom_line(data=veff, aes(y=fit))
	+ geom_line(data=iveff, aes(y=fit)) 
)

print(comp)
