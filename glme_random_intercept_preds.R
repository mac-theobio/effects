library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(emmeans)

loadEnvironments()
startGraphics()

## No bias adjustment
glme_pred_none <- varpred(glme_mod
	, "age"
	, isolate=TRUE
	, bias.adjust="none"
	, modelname="none"
)
est_prop_none <- data.frame(fit=mean(glme_pred_none$preds$fit), model="none")
est_focal_mean <- mean(glme_pred_none$preds$age)

## pop
glme_pred_pop <- varpred(glme_mod
	, "age"
	, isolate=TRUE
	, bias.adjust="population"
	, modelname="bias corrected"
	, include.re=TRUE
)
est_prop_pop <- data.frame(fit=mean(glme_pred_pop$preds$fit), model="bias corrected")

## Bins 
binned_df <- binfun(glme_mod, focal="age", bins=50, groups=NULL)

## Combine preds
vlist <- list(glme_pred_none
	, glme_pred_pop
)

glme_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=TRUE
	)
	+ geom_point(data=binned_df, aes(x=age, y=status), colour="grey")
	+ geom_hline(data=true_prop_df, aes(yintercept=status), lty=1, col="grey")
	+ geom_hline(data=est_prop_none, aes(yintercept=fit, colour=model), lty=2, col="red")
	+ geom_hline(data=est_prop_pop, aes(yintercept=fit, colour=model), lty=2, col="black")
	+ geom_vline(aes(xintercept=mean(age)), lty=2, col="grey")
	+ scale_colour_manual(breaks = c("none", "bias corrected")
		, values=c("none"="red", "bias corrected"="black")
	)
	+ scale_linetype_manual(values=c("none"="solid", "bias corrected"="solid"))
	+ labs(y="Probability of\n improved water", colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
)
glme_plots

### Compare with emmeans
#age_at <- quantile(glme_sim_df$age, seq(0,1,length.out=100))
#glme_varpred_emmeans_plots <- combinepreds(glme_mod, c("emmeans", "varpred")
#	, focal = "age"
#	, x.var = "age"
#	, at=list(age=age_at)
#	, xlevel=list(age=age_at)
#	, nesting=NULL
#	, type="response"
#	, weights="proportional"
#	, plotit = TRUE
#	, ci=FALSE
#)
#glme_varpred_emmeans_plots

saveVars(glme_plots)

