library(ggplot2); theme_set(theme_bw(base_size=16))
library(ggpubr)

library(shellpipes)
startGraphics(height=5)

comb <- ggarrange(
	getEnvObj("predeff", "def") + ggtitle("Default")
	, getEnvObj("predeff", "anchor") + ggtitle("Fixed anchor")
		+ rremove("ylab")
	, ncol=2
)

print(comb)
