library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=16))

library(shellpipes)
startGraphics()

dat <- rdsRead()

print(ggplot(dat)
	+ aes(nitro, robustProp)
	+ geom_point()
	+ xlab("Nitrogen")
	+ ylab("prop. robust")
	+ geom_point(aes(mean(nitro), mean(robustProp)), size=4, color="grey")
)
