library(shellpipes)
library(vareffects); varefftheme()
library(ggplot2)
library(ggthemes)
library(patchwork)

loadEnvironments()
startGraphics()

p1 <- (justify_ci_plots
	+ labs(title="A) Predictions and effects")
	+ theme(legend.position="right")
)

p2 <- (veff_combined_plot
	+ labs(title="B) Anchors")
	+ theme(legend.position="right")
)

p <- p1/p2

teeGG(p)
