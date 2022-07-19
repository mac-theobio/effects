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
pred_age_pop_true_df <- pred_age_pop_true
pred_age_pop_true_df$.method <- "A) Mean-based"
pred_age_pop_true_df$lwr <- pred_age_pop_true_df$fit # Reset CI for the 'truth'
pred_age_pop_true_df$upr <- pred_age_pop_true_df$fit

### Binned obs
binned_df <- binfun(glm_mod, focal="age", bins=50, groups=NULL)

### Combine all predictions
vlist <- list(pred_age_prediction, pred_age_effect)
col_limits <- c("Truth", "Effects", "Predictions")

pred_age <- comparevarpred(vlist=vlist
	, lnames=NULL
	, plotit=FALSE
	, addmarginals=FALSE
	, ci=TRUE
)
pred_age_df <- pred_age$preds
pred_age_df$.method <- "A) Mean-based" 

## Corrected (Observed-value)
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
pred_age_ws_df <- pred_age_ws$preds
pred_age_ws_df$.method <- "B) Observed-value-based"
pred_age_pop_ws_true_df <- pred_age_pop_true_df
pred_age_pop_ws_true_df$.method <- "B) Observed-value-based"

## Compare the predictions
pred_df <- do.call("rbind"
	, list(pred_age_df, pred_age_ws_df, pred_age_pop_true_df, pred_age_pop_ws_true_df)
)
pred_age_plots <- (ggplot(pred_df, aes(x=age, y=fit))
	+ geom_line(aes(colour=model, linetype=model), size=1)
	+ geom_ribbon(aes(ymin=lwr, ymax=upr, fill=model), alpha=0.5)
	+ geom_point(data=binned_df, aes(x=age, y=status), colour="grey")
	+ scale_color_colorblind(limits=col_limits)
	+ scale_fill_colorblind(limits=col_limits)
	+ scale_linetype_discrete(limits=col_limits)
	+ labs(y="Probability of improved water quality", fill="Method", colour="Method", linetype="Method")
	+ facet_wrap(~.method)
	+ theme(strip.text.x = element_text(face="plain"))
)
teeGG(pred_age_plots)
