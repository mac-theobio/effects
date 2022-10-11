library(shellpipes)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(varpred); varpredtheme()

loadEnvironments()

col_limits <- c("data mean", "varpred", "emmeans", "Effect")

rrbind <- function(x){do.call("rbind", x)}
preds_df <- (list(varpred_mc, emmeans_iv, effects_iv)
	|> rrbind()
)

## Average predictions to compare with data mean
pred_mean <- (preds_df
	%>% group_by(model)
	%>% summarise(fit=mean(fit))
)

preds_plot <- (ggplot(preds_df, aes(x=age, colour=model))
	+ geom_line(aes(y=fit))
	+ geom_line(aes(y=lwr, linetype=model))
	+ geom_line(aes(y=upr, linetype=model))
	+ geom_hline(data=pred_mean, aes(yintercept=fit, colour=model, linetype=model))
	+ geom_hline(data=hhsize_mean, aes(yintercept=fit, colour=model), linetype=3)
	+ scale_color_colorblind(limits=col_limits)
 	+ scale_linetype_discrete(limits=col_limits)
	+ labs(x="Age", y="Household size", colour="Model", linetype="Model")
	+ guides(linetype="none")
)
teeGG(preds_plot, desc="simple")

(preds_plot
	+ geom_point(data=bin_df, aes(x=age, y=hhsize), colour="grey")
) |> teeGG(desc="data")
	
