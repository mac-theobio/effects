library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

loadEnvironments()
startGraphics()

## Age

### Not corrected
pred_age_none <- varpred(glm_mod
	, "age"
	, bias.adjust="none"
	, modelname="Mean-anchored"
)
pred_age_none_mean <- getmeans(pred_age_none, what="estimate")

### Bias corrected
pred_age_pop <- varpred(glm_mod
	, "age"
	, bias.adjust="population"
	, modelname="Population-based"
)
pred_age_pop_mean <- getmeans(pred_age_pop, what="estimate")

### Binned obs
binned_df <- binfun(glm_mod, focal="age", bins=50, groups=NULL)

### Combine all predictions
vlist <- list(pred_age_none, pred_age_pop)

pred_age_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
	+ geom_vline(aes(xintercept=mean(age)), lty=2)
	+ geom_hline(data=pred_age_none_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=pred_age_pop_mean, aes(yintercept=fit, colour=model, lty=model))
	+ geom_hline(data=true_prop_df, aes(yintercept=status, colour=model,lty=model))
	+ geom_point(data=binned_df, aes(x=age, y=status), colour="grey")
	+ scale_colour_manual(breaks = c("Observed mean", "Mean-anchored", "Population-based")
		, values=c("Observed mean"="red", "Mean-anchored"="blue", "Population-based"="black")
	)
	+ scale_linetype_manual(values=c("Observed mean"=2, "Mean-anchored"=1, "Population-based"=1))
	+ labs(y="Probability of improved water quality", colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
)

## Wealth index

### Not corrected
pred_wealthindex_none <- varpred(glm_mod
	, "wealthindex"
	, bias.adjust="none"
	, modelname="Mean-anchored"
)
pred_wealthindex_none_mean <- getmeans(pred_wealthindex_none, what="estimate")

### Bias corrected
pred_wealthindex_pop <- varpred(glm_mod
	, "wealthindex"
	, bias.adjust="population"
	, modelname="Population-based"
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
	+ scale_colour_manual(breaks = c("Observed mean", "Mean-anchored", "Population-based")
		, values=c("Observed mean"="red", "Mean-anchored"="blue", "Population-based"="black")
	)
	+ scale_linetype_manual(values=c("Observed mean"=2, "Mean-anchored"=1, "Population-based"=1))
	+ labs(colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
)

pred_bin_plots <- ggarrange(pred_age_plots
	, pred_wealthindex_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_bin_plots)

## Figure 5
pdf("pred_bin_plots-figure5.pdf", height = 5.3)
print(pred_bin_plots)
dev.off()

