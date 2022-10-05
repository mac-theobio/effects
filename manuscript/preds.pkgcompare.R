library(shellpipes)
library(ggplot2)
library(ggthemes)
library(varpred); varpredtheme()

loadEnvironments()

rrbind <- function(x){do.call("rbind", x)}
preds_df <- (list(varpred_mc, emmeans_iv, effects_iv)
	|> rrbind()
)

preds_plot <- (ggplot(preds_df, aes(x=age, colour=model))
	+ geom_line(aes(y=fit))
	+ geom_line(aes(y=lwr, linetype=model))
	+ geom_line(aes(y=upr, linetype=model))
	+ scale_color_colorblind()
	+ labs(x="Age", y="Household size", colour="Model", linetype="Model")
)
teeGG(preds_plot, desc="simple")

(preds_plot
	+ geom_point(data=bin_df, aes(x=age, y=hhsize), colour="grey")
) |> teeGG(desc="data")
	
