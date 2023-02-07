library(ggplot2); theme_set(theme_bw(base_size=14))
library(ggpubr)

library(shellpipes)
startGraphics(height=4)

comb <- ggarrange(
	getEnvObj("predeff", "def") + ggtitle("Default")
	, getEnvObj("predeff", "mean") + ggtitle("Mean anchor")
		+ rremove("ylab")
	, ncol=2
)

print(comb)
