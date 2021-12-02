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
	, modelname="none"
)

### Centered predictions
pred_age_centered_cubic <- varpred(mod_cubic
	, "age"
	, isolate=TRUE
	, bias.adjust="population"
	, modelname="bias corrected"
)
pred_age_mean <- mean(pred_age_centered_cubic$preds$fit)

### Combine
vlist <- list(pred_age_trad_cubic, pred_age_centered_cubic)

pred_age_cubic_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ geom_hline(data=true_prop_df, aes(yintercept=hhsize), colour="grey", lty=1)
	+ geom_hline(yintercept=pred_age_mean, colour="yellow", lty=2)
#	+ geom_vline(aes(xintercept=mean(age)), colour="grey", lty=2)
	+ scale_colour_manual(breaks = c("none", "bias corrected")
		, values=c("none"="red", "bias corrected"="black")
	)
	+ scale_linetype_manual(values=c("none"="dotted", "bias corrected"="solid"))
	+ labs(title="a) Focal polynomial", y="Predictions", linetype="Method", colour="Method")
	+ theme(legend.position="bottom")
)
print(pred_age_cubic_plots)

## Non-focal polynomial

### All uncertainties 
pred_wealthindex_trad_cubic <- varpred(mod_cubic
	, "wealthindex"
	, isolate=TRUE
	, modelname="none"
)

### Centered predictions
pred_wealthindex_centered_cubic <- varpred(mod_cubic
	, "wealthindex"
	, isolate=TRUE
	, bias.adjust="population"
	, modelname="bias corrected"
)
pred_wealthindex_mean <- mean(pred_wealthindex_centered_cubic$preds$fit)

### Combine
vlist <- list(pred_wealthindex_trad_cubic, pred_wealthindex_centered_cubic)

pred_wealthindex_cubic_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
	)
	+ geom_hline(data=true_prop_df, aes(yintercept=hhsize), colour="grey", lty=1)
	+ geom_hline(yintercept=pred_wealthindex_mean, colour="yellow", lty=2)
	+ geom_vline(aes(xintercept=mean(wealthindex)), colour="grey", lty=2)
	+ scale_colour_manual(breaks = c("none", "bias corrected")
		, values=c(none="red", "bias corrected"="black")
	)
	+ scale_linetype_manual(values=c("none"="solid", "bias corrected"="solid"))
	+ labs(title="b) Non-focal is a polynomial", y="Predictions", linetype="Method", colour="Method")
	+ theme(legend.position="bottom")
)
print(pred_wealthindex_cubic_plots)

## Combine all prediction for faceting
pred_cubic_plots_adjust <- ggarrange(pred_age_cubic_plots
	, pred_wealthindex_cubic_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
print(pred_cubic_plots_adjust)

saveVars(pred_cubic_plots_adjust)
