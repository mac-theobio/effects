library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(emmeans)

loadEnvironments()
startGraphics()

## Focal polynomial

### Non-isolated CIs 
pred_age_non_cubic <- varpred(mod_cubic
	, "age"
	, isolate=FALSE
	, modelname="Non-isolated"
)

### Isolated CIs
pred_age_isolated_cubic <- varpred(mod_cubic
	, "age"
	, isolate=TRUE
	, modelname="Isolated"
)
pred_fit_mean <- getmeans(pred_age_isolated_cubic, what="estimate", modelname="Predicted mean")
print(pred_fit_mean)

### Combine
vlist <- list(pred_age_non_cubic, pred_age_isolated_cubic)

pred_age_cubic_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
#	+ geom_hline(data=pred_fit_mean, aes(yintercept=fit, colour=model), lty=1)
	+ geom_hline(data=true_prop_df, aes(yintercept=fit, colour=model), lty=2)
	+ scale_colour_manual(breaks = c("Observed mean", "Predicted mean", "Isolated", "Non-isolated")
		, values=c("Observed mean"="black", "Predicted mean"="red", "Isolated"="red", "Non-isolated"="blue")
	)
	+ scale_linetype_manual(values=c("Observed mean"=2, "Predicted mean"=1, "Isolated"=1, "Non-isolated"=3))
	+ labs(title="A) Focal polynomial", y="Predicted household size", linetype="Method", colour="Method")
	+ theme(legend.position="bottom")
)
print(pred_age_cubic_plots)


## Non-focal polynomial

### Non-isolated CIs 
pred_wealthindex_non_cubic <- varpred(mod_cubic
	, "wealthindex"
	, isolate=FALSE
	, modelname="Non-isolated"
)

### Isolated CIs
pred_wealthindex_isolated_cubic <- varpred(mod_cubic
	, "wealthindex"
	, isolate=TRUE
	, modelname="Isolated"
)
pred_fit_mean <- getmeans(pred_wealthindex_isolated_cubic, what="estimate", modelname="Predicted mean")
print(pred_fit_mean)

### Combine
vlist <- list(pred_wealthindex_non_cubic, pred_wealthindex_isolated_cubic)

pred_wealthindex_cubic_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ geom_hline(data=pred_fit_mean, aes(yintercept=fit, colour=model), lty=1)
	+ geom_hline(data=true_prop_df, aes(yintercept=fit, colour=model), lty=2)
	+ geom_vline(aes(xintercept=mean(wealthindex)), lty=2)
	+ scale_colour_manual(breaks = c("Observed mean", "Predicted mean", "Isolated", "Non-isolated")
		, values=c("Observed mean"="black", "Predicted mean"="red", "Isolated"="red", "Non-isolated"="blue")
	)
	+ scale_linetype_manual(values=c("Observed mean"=2, "Predicted mean"=1, "Isolated"=1, "Non-isolated"=3))
	+ labs(title="B) Focal non-polynomial", y="Predicted household size", linetype="Method", colour="Method")
	+ theme(legend.position="bottom")
)

## Combine all prediction for faceting
pred_cubic_plots <- ggarrange(pred_age_cubic_plots
	, pred_wealthindex_cubic_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_cubic_plots)

teeGG(pred_cubic_plots)


