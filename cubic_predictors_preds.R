library(shellpipes)
library(vareffects); varefftheme()
library(splines)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(emmeans)

loadEnvironments()
startGraphics()

## Focal polynomial

### All uncertainties 
pred_age_trad_cubic <- varpred(mod_cubic
	, "age"
	, isolate=TRUE
	, modelname="everything"
)

### Centered predictions
pred_age_centered_cubic <- varpred(mod_cubic
	, "age"
	, isolate=TRUE
	, isolate.value=0
	, modelname="isolated (mm)"
)
pred_age_mean <- mean(pred_age_centered_cubic$preds$fit)

### Combine
vlist <- list(pred_age_trad_cubic, pred_age_centered_cubic)

pred_age_cubic_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ geom_hline(data=true_prop_df, aes(yintercept=hhsize), colour="grey", lty=2)
	+ geom_hline(yintercept=pred_age_mean, colour="yellow", lty=2)
#	+ geom_vline(aes(xintercept=mean(age)), colour="grey", lty=2)
	+ scale_colour_manual(breaks = c("everything", "isolated (mm)")
		, values=c(everything="blue", "isolated (mm)"="black")
	)
	+ labs(title="a) Focal polynomial", y="Predictions", linetype="Method", colour="Method")
	+ theme(legend.position="bottom")
)
print(pred_age_cubic_plots)

## Non-focal polynomial

### All uncertainties 
pred_wealthindex_trad_cubic <- varpred(mod_cubic
	, "wealthindex"
	, isolate=FALSE
	, modelname="everything"
)

### Centered predictions
pred_wealthindex_centered_cubic <- varpred(mod_cubic
	, "wealthindex"
	, isolate=TRUE
	, isolate.value=0
	, modelname="isolated (mm)"
)
pred_wealthindex_mean <- mean(pred_wealthindex_centered_cubic$preds$fit)

### Combine
vlist <- list(pred_wealthindex_trad_cubic, pred_wealthindex_centered_cubic)

pred_wealthindex_cubic_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ geom_hline(data=true_prop_df, aes(yintercept=hhsize), colour="grey", lty=2)
	+ geom_hline(yintercept=pred_wealthindex_mean, colour="yellow", lty=2)
	+ geom_vline(aes(xintercept=mean(wealthindex)), colour="grey", lty=2)
	+ scale_colour_manual(breaks = c("everything", "isolated (mm)")
		, values=c(everything="blue", "isolated (mm)"="black")
	)
	+ labs(title="b) Non-focal is a polynomial", y="Predictions", linetype="Method", colour="Method")
	+ theme(legend.position="bottom")
)
print(pred_wealthindex_cubic_plots)

## Combine all prediction for faceting
pred_cubic_plots <- ggarrange(pred_age_cubic_plots
	, pred_wealthindex_cubic_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_cubic_plots)

quants <- seq(0, 1, length.out=100)
focal_levels <- quantile(sim_df_cubic$wealthindex, quants)
compare_cubic_plots <- (combinepreds(mod_cubic, focal="wealthindex"
		, funs=c("emmeans", "varpred")
		, at=list(wealthindex=focal_levels)
		, x.var="wealthindex"
		, isolate=TRUE
		, plotit=TRUE
	)
	+ geom_vline(aes(xintercept=mean(focal_levels)), lty=2, colour="grey")
	+ geom_hline(yintercept=pred_wealthindex_mean, colour="yellow", lty=1, alpha=0.5)
	+ labs(colour="Method", linetype="Method")
)
print(compare_cubic_plots)

saveVars(pred_cubic_plots)
