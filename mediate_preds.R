library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

loadEnvironments()
startGraphics()

## Not mediated

### Not corrected
pred_notmediated_none <- varpred(mod_notmediated
	, "x"
	, bias.adjust="none"
	, modelname="none"
)
pred_notmediated_none_mean <- getmeans(pred_notmediated_none, what="estimate")

### Bias corrected
pred_notmediated_pop <- varpred(mod_notmediated
	, "x"
	, bias.adjust="population"
	, modelname="bias corrected"
)
pred_notmediated_pop_mean <- getmeans(pred_notmediated_pop, what="estimate")

### Binned obs
binned_df <- binfun(mod_notmediated, focal="x", bins=50, groups=NULL)

### Combine all predictions
vlist <- list(pred_notmediated_none, pred_notmediated_pop)

pred_notmediated_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
	+ geom_hline(data=pred_notmediated_none_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=pred_notmediated_pop_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=observed_df_med, aes(yintercept=z, colour="observed", lty="observed"))
	+ geom_vline(data=observed_df_med, aes(xintercept=x), lty=2, col="grey")
#	+ geom_point(data=binned_df, aes(x=x, y=z), colour="grey")
	+ scale_colour_manual(breaks = c("observed", "none", "bias corrected")
		, values=c("observed"="red", "none"="blue", "bias corrected"="black")
	)
	+ scale_linetype_manual(values=c("observed"=2, "none"=3, "bias corrected"=4))
	+ labs(colour="model", linetype="model", title="Not mediated")
	+ theme(legend.position="bottom")
)

## Mediated

### Not corrected
pred_mediated_none <- varpred(mod_mediated
	, "x"
	, bias.adjust="none"
	, modelname="none"
)
pred_mediated_none_mean <- getmeans(pred_mediated_none, what="estimate")

### Bias corrected
pred_mediated_pop <- varpred(mod_mediated
	, "x"
	, bias.adjust="population"
	, modelname="bias corrected"
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
	+ geom_hline(data=observed_df_med, aes(yintercept=z, colour="observed", lty="observed"))
	+ geom_vline(data=observed_df_med, aes(xintercept=x), lty=2, col="grey")
#	+ geom_point(data=binned_df, aes(x=x, y=z), colour="grey")
	+ scale_colour_manual(breaks = c("observed", "none", "bias corrected")
		, values=c("observed"="red", "none"="blue", "bias corrected"="black")
	)
	+ scale_linetype_manual(values=c("observed"=2, "none"=3, "bias corrected"=4))
	+ labs(colour="model", linetype="model", title="Mediated")
	+ theme(legend.position="bottom")
)

pred_mediate_plots <- ggarrange(pred_notmediated_plots
	, pred_mediated_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_mediate_plots)

saveVars(pred_mediate_plots)

