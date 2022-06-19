library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggthemes)
library(ggplot2)
library(dplyr)

loadEnvironments()
startGraphics()

## Age

## Not corrected
### Predictions
pred_age_prediction <- varpred(glm_mod
	, "age"
	, isolate=FALSE
	, bias.adjust="none"
	, modelname="Predictions"
)

### Effects
pred_age_effect <- varpred(glm_mod
	, "age"
	, bias.adjust="none"
	, modelname="Effects"
)

### True predictions bias corrected
pred_age_pop_true <- varpred(glm_mod
	, "age"
	, true.beta=glm_sim_betas
	, bias.adjust="population"
	, modelname="Truth"
)$preds

### Binned obs
binned_df <- binfun(glm_mod, focal="age", bins=50, groups=NULL)

### Combine all predictions
vlist <- list(pred_age_prediction, pred_age_effect)
col_limits <- c("Effects", "Predictions")

pred_age <- comparevarpred(vlist=vlist
	, lnames=NULL
	, plotit=FALSE
	, addmarginals=FALSE
	, ci=TRUE
)
pred_age_plots <- (ggplot(pred_age$preds, aes(x=age, y=fit))
	+ geom_line(aes(colour=model, linetype=model), size=1)
	+ geom_ribbon(aes(ymin=lwr, ymax=upr, fill=model), alpha=0.5)
	+ geom_line(data=pred_age_pop_true, aes(x=age, y=fit), colour="red", lty=1, size=1)
	+ geom_point(data=binned_df, aes(x=age, y=status), colour="grey")
	+ scale_color_colorblind(limits=col_limits)
	+ scale_linetype_discrete(limits=col_limits)
	+ labs(y="Probability of improved water quality", fill="Method", colour="Method", linetype="Method", title="A) Mean-based")
)

## Corrected (Whole-sample)
### Predictions
pred_age_prediction_ws <- varpred(glm_mod
	, "age"
	, isolate=FALSE
	, bias.adjust="population"
	, modelname="Predictions"
)

### Effects
pred_age_effect_ws <- varpred(glm_mod
	, "age"
	, bias.adjust="population"
	, modelname="Effects"
)

### Combine all predictions
vlist <- list(pred_age_prediction_ws, pred_age_effect_ws)

pred_age_ws <- comparevarpred(vlist=vlist
	, lnames=NULL
	, plotit=FALSE
	, addmarginals=FALSE
	, ci=TRUE
)
pred_age_ws_plots <- (ggplot(pred_age_ws$preds, aes(x=age, y=fit))
	+ geom_line(aes(colour=model, linetype=model), size=1)
	+ geom_ribbon(aes(ymin=lwr, ymax=upr, fill=model), alpha=0.5)
	+ geom_line(data=pred_age_pop_true, aes(x=age, y=fit), colour="red", lty=1, size=1)
	+ geom_point(data=binned_df, aes(x=age, y=status), colour="grey")
	+ scale_color_colorblind(limits=col_limits)
	+ scale_linetype_discrete(limits=col_limits)
	+ labs(y="Probability of improved water quality", fill="Method", colour="Method", linetype="Method", title="B) Whole-sample-based")
)

pred_bin_plots <- ggarrange(pred_age_plots
	, pred_age_ws_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)

teeGG(pred_bin_plots)
