library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggthemes)
library(ggplot2)
library(dplyr)

loadEnvironments()
startGraphics()

## Age

### Not corrected
pred_age_none <- varpred(glm_mod
	, "age"
	, bias.adjust="none"
	, modelname="Mean-based"
)
pred_age_none_mean <- getmeans(pred_age_none, what="estimate")

### Bias corrected
pred_age_pop <- varpred(glm_mod
	, "age"
	, bias.adjust="population"
	, modelname="Observed-value-based"
)
pred_age_pop_mean <- getmeans(pred_age_pop, what="estimate")

### Binned obs
binned_df <- binfun(glm_mod, focal="age", bins=50, groups=NULL)
col_limits <- c("Observed mean", "Mean-based", "Observed-value-based")

### Combine all predictions
vlist <- list(pred_age_none, pred_age_pop)

pred_age_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
	+ geom_hline(data=true_prop_df, aes(yintercept=status, colour=model,lty=model))
	+ geom_vline(aes(xintercept=mean(age)), lty=2)
	+ geom_hline(data=pred_age_none_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=pred_age_pop_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_point(data=binned_df, aes(x=age, y=status), colour="grey")
	+ scale_color_colorblind(limits=col_limits)
 	+ scale_linetype_discrete(limits=col_limits)
	+ labs(y="Probability of improved water quality", colour="Method", linetype="Method", title="A) Small effect size")
	+ theme(legend.position="bottom")
)

## Wealth index

### Not corrected
pred_wealthindex_none <- varpred(glm_mod
	, "wealthindex"
	, bias.adjust="none"
	, modelname="Mean-based"
)
pred_wealthindex_none_mean <- getmeans(pred_wealthindex_none, what="estimate")

### Bias corrected
pred_wealthindex_pop <- varpred(glm_mod
	, "wealthindex"
	, bias.adjust="population"
	, modelname="Observed-value-based"
)
pred_wealthindex_pop_mean <- getmeans(pred_wealthindex_pop, what="estimate")

### Binned obs
binned_df <- binfun(glm_mod, focal="wealthindex", bins=50, groups=NULL)

### Combine all predictions
vlist <- list(pred_wealthindex_none, pred_wealthindex_pop)

pred_wealthindex_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
	+ geom_vline(aes(xintercept=mean(wealthindex)), lty=2)
	+ geom_hline(data=pred_wealthindex_none_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=pred_wealthindex_pop_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=true_prop_df, aes(yintercept=status, colour=model,lty=model))
	+ geom_point(data=binned_df, aes(x=wealthindex, y=status), colour="grey")
	+ scale_color_colorblind(limits=col_limits)
 	+ scale_linetype_discrete(limits=col_limits)
	+ labs(colour="Method", linetype="Method", title="B) Large effect size")
	+ theme(legend.position="bottom")
)

pred_bin_plots <- ggarrange(pred_age_plots
	, pred_wealthindex_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_bin_plots)

teeGG(pred_bin_plots)
