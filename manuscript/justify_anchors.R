library(shellpipes)
library(vareffects); varefftheme()
library(ggplot2)
library(dplyr)
library(ggthemes)

loadEnvironments()
startGraphics()

## Isolated with CIs (zero-anchored)
quants <- seq(0,1,length.out=100)
x2_focal <- quantile(justify_sim_df$x2, quants, names=FALSE)
justify_center_anchored <- varpred(justify_mod, "x2", at=list(x2=x2_focal), modelname="center-anchored")
justify_zero_anchored <- varpred(justify_mod, "x2", at=list(x2=x2_focal), isolate.value=min(x2_focal), modelname="min-anchored")

## Combined
veff_combined_plot <- (combinevarpred(list(justify_center_anchored, justify_zero_anchored), plotit=TRUE)
	+ labs(x="x2", y="Predicted y")
	+ scale_colour_colorblind()
	+ scale_linetype_manual(values=c(1,1))
	+ theme(legend.position="bottom")
)
teeGG(veff_combined_plot, desc="combined")

