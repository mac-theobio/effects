library(shellpipes)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(varpred); varpredtheme()

loadEnvironments()

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
	+ geom_hline(data=pred_mean, aes(yintercept=fit, colour=model))
	+ geom_hline(data=hhsize_mean, aes(yintercept=fit, colour=model, linetype=model))
	+ scale_color_colorblind()
	+ labs(x="Age", y="Household size", colour="Model", linetype="Model")
)
teeGG(preds_plot, desc="simple")

(preds_plot
	+ geom_point(data=bin_df, aes(x=age, y=hhsize), colour="grey")
) |> teeGG(desc="data")
	
