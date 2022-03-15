library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

loadEnvironments()
startGraphics()

## Age

points <- (ggplot(binned_df)
	+ aes(x=age, y=status)
	+ geom_point(colour="grey")
	+ xlab("income")
	+ ylab("Service probability")
)

uncorrected <- (points
	+ geom_line(data=pred_age_none, aes(y=fit))
)
teeGG(uncorrected, desc="uncorrected")

uncorrected_band <- (uncorrected
	+ geom_line(data=pred_age_none, aes(y=upr), lty=2)
	+ geom_line(data=pred_age_none, aes(y=lwr), lty=2)
)
teeGG(uncorrected_band, desc="uncorrected_band")

## This is the cool syntax, upgrade
(both <- uncorrected
	+ geom_line(data=pred_age_pop, aes(y=fit), color="blue")
) %>% teeGG(desc="both")

corrected <- (points
	+ geom_line(data=pred_age_pop, aes(y=fit), color="blue")
	+ geom_line(data=pred_age_pop, aes(y=upr), lty=2, color="blue")
	+ geom_line(data=pred_age_pop, aes(y=lwr), lty=2, color="blue")
)
teeGG(corrected, desc="corrected")

quit()

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
	+ scale_colour_manual(breaks = c("Observed mean", "Mean-based", "Population-based")
		, values=c("Observed mean"="red", "Mean-based"="blue", "Population-based"="black")
	)
	+ scale_linetype_manual(values=c("Observed mean"=2, "Mean-based"=1, "Population-based"=1))
	+ labs(y="Probability of improved water quality", colour="Method", linetype="Method")
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
	+ scale_colour_manual(breaks = c("Observed mean", "Mean-based", "Population-based")
		, values=c("Observed mean"="red", "Mean-based"="blue", "Population-based"="black")
	)
	+ scale_linetype_manual(values=c("Observed mean"=2, "Mean-based"=1, "Population-based"=1))
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
