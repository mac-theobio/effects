library(shellpipes)
library(ggplot2)
library(ggthemes)
library(varpred); varpredtheme()

loadEnvironments()

rrbind <- function(x){do.call("rbind", x)}
preds_df <- (list(varpred_mc, emmeans_iv)
	|> rrbind()
)

preds_plot <- (ggplot(preds_df, aes(x=age, colour=model))
	+ geom_line(aes(y=fit))
	+ geom_line(aes(y=lwr))
	+ geom_line(aes(y=upr))
	+ scale_color_colorblind()
)
preds_plot
