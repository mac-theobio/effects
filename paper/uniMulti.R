
library(ggplot2); theme_set(theme_bw(base_size=14))
library(ggpubr)

library(shellpipes)
startGraphics(height=4)

comb <- ggarrange(
	getEnvObj("predeff", "uniN") + ggtitle("Univariate")
	, getEnvObj("eff", "multi") + ggtitle("Multivariate")
		+ rremove("ylab")
	, ncol=2
)

print(comb)
