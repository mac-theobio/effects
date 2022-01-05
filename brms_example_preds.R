library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(brms)
library(emmeans)

loadEnvironments()
startGraphics()

## focal levels
x2_levels <- quantile(rstan_sim_df$x2, seq(0,1,length.out=100))

## No bias adjustment
brms_pred_none <- varpred(brms_mod
	, "x2"
	, at=list(x2=x2_levels)
	, isolate=FALSE
	, bias.adjust="none"
	, modelname="none"
)
est_prop_none <- data.frame(fit=mean(brms_pred_none$preds$fit), model="none")

## pop: not adjusted for re
brms_pred_pop <- varpred(brms_mod
	, "x2"
	, at=list(x2=x2_levels)
	, isolate=TRUE
	, bias.adjust="population"
	, modelname="bias corrected"
	, include.re=FALSE
)
est_prop_pop <- data.frame(fit=mean(brms_pred_pop$preds$fit), model="bias corrected")

## pop: include re
brms_pred_pop_re <- varpred(brms_mod
	, "x2"
	, at=list(x2=x2_levels)
	, isolate=TRUE
	, bias.adjust="population"
	, modelname="bias corrected (+re)"
	, include.re=TRUE
)
est_prop_pop_re <- data.frame(fit=mean(brms_pred_pop_re$preds$fit), model="bias corrected (+re)")

## Bins 
binned_df <- binfun(brms_mod, focal="x2", bins=50, groups=NULL)

## Combine preds
vlist <- list(brms_pred_none
	, brms_pred_pop
	, brms_pred_pop_re
)

## emmeans uses posterior predict method
pred_brms <- emmeans(brms_mod, specs=~x2, at=list(x2=x2_levels), type="response") 
pred_brms <- as.data.frame(pred_brms)
colnames(pred_brms) <- c("x2", "fit", "lwr", "upr")
pred_brms$model <- "postpred"
pred_brms$se <- NA


### Combine the predictions
pred_vpred <- combinevarpred(vlist, plotit=FALSE)
pred_vpred$preds <- do.call("rbind", list(pred_vpred$preds, pred_brms))
brms_plots <- (plot(pred_vpred, ci=FALSE)
	+ scale_colour_manual(breaks = c("none", "postpred", "bias corrected", "bias corrected (+re)", "observed")
		, values=c( "none"="red", "postpred"="orange", "bias corrected"="blue", "bias corrected (+re)"="black", "observed"="green")
	)
	+ geom_point(data = binned_df, aes(x=x2, y=y), col="grey")
	+ geom_hline(data=est_prop_none, aes(yintercept=fit, col=model, lty=model))
	+ geom_hline(data=est_prop_pop, aes(yintercept=fit, col=model, lty=model))
	+ geom_hline(data=est_prop_pop_re, aes(yintercept=fit, col=model, lty=model))
	+ geom_hline(yintercept=mean(rstan_sim_df$y), lyt=5, col="green")
	+ scale_linetype_manual(values=c("none"=4, "postpred"=3, "bias corrected"=2, "bias corrected (+re)"=1, "observed"=5))
	+ labs(colour="Method", linetype="Method")
)
print(brms_plots)

saveVars(brms_plots)

