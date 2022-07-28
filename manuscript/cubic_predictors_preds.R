library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(emmeans)

loadEnvironments()
startGraphics()

## Focal polynomial

### Predictions CIs 
pred_age_non_cubic <- varpred(mod_cubic
	, "age"
	, steps=500
	, isolate=FALSE
	, modelname="Predictions"
)

### Effects CIs
pred_age_isolated_cubic <- varpred(mod_cubic
	, "age"
	, steps=500
	, isolate=TRUE
	, modelname="Effects"
)
pred_fit_mean <- getmeans(pred_age_isolated_cubic, what="estimate", modelname="Predicted mean")
print(pred_fit_mean)

## Colour limits
col_limits <- c("Data mean", "Predicted mean", "Effects", "Predictions")

### Combine
vlist <- list(pred_age_non_cubic, pred_age_isolated_cubic)

pred_age_cubic_df <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=FALSE
		, addmarginals=FALSE
	)
)
pred_age_cubic_plots <- (ggplot(pred_age_cubic_df$preds, aes(x=age, y=fit))
	+ geom_line(aes(linetype=model, colour=model), size=1, alpha=0.5)
	+ geom_ribbon(aes(ymin=lwr, ymax=upr, fill=model), alpha=0.5)
 	+ geom_hline(data=true_prop_df, aes(yintercept=fit, colour=model, linetype="Data mean"))
 	+ geom_hline(data=pred_fit_mean, aes(yintercept=fit, colour=model, linetype=model))
 	+ scale_color_colorblind(limits=col_limits)
 	+ scale_fill_colorblind(limits=col_limits)
 	+ scale_linetype_discrete(limits=col_limits)
 	+ labs(title="A) Polynomial", y="Household size", linetype="Method", colour="Method", fill="Method")
)
print(pred_age_cubic_plots)


## Non-focal polynomial

### Predictions CIs 
pred_wealthindex_non_cubic <- varpred(mod_cubic
	, "wealthindex"
	, isolate=FALSE
	, modelname="Predictions"
)

### Effects CIs
pred_wealthindex_isolated_cubic <- varpred(mod_cubic
	, "wealthindex"
	, isolate=TRUE
	, modelname="Effects"
)
pred_fit_mean <- getmeans(pred_wealthindex_isolated_cubic, what="estimate", modelname="Predicted mean")
print(pred_fit_mean)

### Combine
vlist <- list(pred_wealthindex_non_cubic, pred_wealthindex_isolated_cubic)

pred_wealthindex_cubic_df <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=FALSE
		, addmarginals=FALSE
	)
)
pred_wealthindex_cubic_plots <- (ggplot(pred_wealthindex_cubic_df$preds, aes(x=wealthindex, y=fit))
	+ geom_line(aes(linetype=model, colour=model), size=1, alpha=0.5)
	+ geom_ribbon(aes(ymin=lwr, ymax=upr, fill=model), alpha=0.5)
 	+ geom_hline(data=true_prop_df, aes(yintercept=fit, colour=model, linetype="Data mean"))
	+ geom_vline(aes(xintercept=mean(wealthindex)), lty=2)
 	+ geom_hline(data=pred_fit_mean, aes(yintercept=fit, colour=model, linetype=model))
 	+ scale_color_colorblind(limits=col_limits)
 	+ scale_fill_colorblind(limits=col_limits)
 	+ scale_linetype_discrete(limits=col_limits)
 	+ labs(title="B) Simple (linear)", y="Household size", linetype="Method", colour="Method", fill="Method")
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

teeGG(pred_cubic_plots)


