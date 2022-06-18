library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggthemes)
library(ggplot2)
library(dplyr)

loadEnvironments()
startGraphics()

## Not mediated

### Not corrected
pred_notmediated_none <- varpred(mod_notmediated
	, "x"
	, bias.adjust="none"
	, modelname="Mean-based"
)
pred_notmediated_none_mean <- getmeans(pred_notmediated_none, what="estimate")

### Bias corrected
pred_notmediated_pop <- varpred(mod_notmediated
	, "x"
	, bias.adjust="population"
	, modelname="Whole-sample-based"
)
pred_notmediated_pop_mean <- getmeans(pred_notmediated_pop, what="estimate")

summary(pred_notmediated_pop)
summary(pred_notmediated_pop$preds)

### Binned obs
binned_df <- binfun(mod_notmediated, focal="x", bins=50, groups=NULL)

summary(binned_df)

### Combine all predictions
vlist <- list(pred_notmediated_none, pred_notmediated_pop)
col_limits <- c("Observed mean", "Mean-based", "Whole-sample-based")

pred_notmediated_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
	+ geom_hline(data=pred_notmediated_none_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=pred_notmediated_pop_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=observed_df_med, aes(yintercept=z, colour=model, lty=model))
	+ geom_vline(data=observed_df_med, aes(xintercept=x), lty=2, col="black")
	+ geom_point(data=binned_df, aes(x=x, y=z), colour="grey")
	+ scale_color_colorblind(limits=col_limits)
 	+ scale_linetype_discrete(limits=col_limits)
	+ labs(colour="Method", linetype="Method", title="A) Non-mediated", y="Predicted probability")
	+ theme(legend.position="bottom")
)

## Mediated

### Not corrected
pred_mediated_none <- varpred(mod_mediated
	, "x"
	, bias.adjust="none"
	, modelname="Mean-based"
)
pred_mediated_none_mean <- getmeans(pred_mediated_none, what="estimate")

### Bias corrected
pred_mediated_pop <- varpred(mod_mediated
	, "x"
	, bias.adjust="population"
	, modelname="Whole-sample-based"
)
pred_mediated_pop_mean <- getmeans(pred_mediated_pop, what="estimate")

### Binned obs
binned_df <- binfun(mod_mediated, focal="x", bins=50, groups=NULL)

### Combine all predictions
vlist <- list(pred_mediated_none, pred_mediated_pop)

pred_mediated_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
	+ geom_hline(data=pred_mediated_none_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=pred_mediated_pop_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=observed_df_med, aes(yintercept=z, colour=model, lty=model))
	+ geom_vline(data=observed_df_med, aes(xintercept=x), lty=2, col="black")
	+ geom_point(data=binned_df, aes(x=x, y=z), colour="grey")
	+ scale_color_colorblind(limits=col_limits)
 	+ scale_linetype_discrete(limits=col_limits)
	+ labs(colour="Method", linetype="Method", title="B) Mediated", y="Predicted probability")
	+ theme(legend.position="bottom")
)

pred_mediated_plots <- ggarrange(pred_notmediated_plots
	, pred_mediated_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_mediated_plots)

teeGG(pred_mediated_plots)
