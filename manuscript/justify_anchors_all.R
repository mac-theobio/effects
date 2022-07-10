library(shellpipes)
library(vareffects); varefftheme()
library(ggplot2)
library(ggthemes)
library(patchwork)

loadEnvironments()
startGraphics()

p1 <- (justify_ci_plots
	+ labs(title="A) Predictions and effects")
	+ theme(legend.position=c(0.25, 0.85))
)

p2 <- (veff_combined_plot
	+ labs(title="B) Anchors", y="")
	+ theme(legend.position=c(0.31, 0.88))
)

p <- p1 + p2

teeGG(p)
