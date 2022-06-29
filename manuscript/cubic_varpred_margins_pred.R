library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(margins)

loadEnvironments()
startGraphics()

## marginal effect
meffect_df <- cplot(mod_cubic, "age", what="effect", draw=FALSE)
meffect_df <- data.frame(meffect_df)
meffect_df$model <- "B) Marginal effect"
meffect_plot <- (ggplot(meffect_df, aes(x=xvals))
	+ geom_line(aes(y=yvals))
	+ geom_line(aes(y=lower), lty=2)
	+ geom_line(aes(y=upper), lty=2)
	+ labs(x="age", y="ME of age", title="B) Marginal effect")
)

## varpred
varpred_df <- varpred(mod_cubic, "age", steps=500, modelname="A) Prediction and effects")
varpred_plot <- (plot(varpred_df)
	+ labs(y="Predicted household size", title="A) Prediction and effects")
)

## Combine all prediction for faceting
pred_plots <- ggarrange(varpred_plot
	, meffect_plot
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)

teeGG(pred_plots)
