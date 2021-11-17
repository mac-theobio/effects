library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(emmeans)

commandEnvironments()
makeGraphics()

## No bias adjustment
glme_pred_none <- varpred(glme_mod
	, "age"
	, isolate=FALSE
	, bias.adjust="none"
	, modelname="none"
)
est_prop_none <- data.frame(fit=mean(glme_pred_none$preds$fit), model="none")
est_focal_mean <- mean(glme_pred_none$preds$age)

## Delta-lp
glme_pred_delta_lp <- varpred(glme_mod
	, "age"
	, isolate=FALSE
	, bias.adjust="delta"
	, sigma="lp"
	, modelname="delta-lp"
)
est_prop_delta_lp <- data.frame(fit=mean(glme_pred_delta_lp$preds$fit), model="delta-lp")

## Delta-mod
glme_pred_delta_mod <- varpred(glme_mod
	, "age"
	, isolate=FALSE
	, bias.adjust="delta"
	, sigma="mod"
	, modelname="delta-mod"
)
est_prop_delta_mod <- data.frame(fit=mean(glme_pred_delta_mod$preds$fit), model="delta-mod")

## mcculloch-lp
glme_pred_mc_lp <- varpred(glme_mod
	, "age"
	, isolate=FALSE
	, bias.adjust="mcculloch"
	, sigma="lp"
	, modelname="mc-lp"
)
est_prop_mc_lp <- data.frame(fit=mean(glme_pred_mc_lp$preds$fit), model="mc-lp")

## mcculloch-mod
glme_pred_mc_mod <- varpred(glme_mod
	, "age"
	, isolate=FALSE
	, bias.adjust="mcculloch"
	, sigma="mod"
	, modelname="mc-mod"
)
est_prop_mc_mod <- data.frame(fit=mean(glme_pred_mc_mod$preds$fit), model="mc-mod")

## diggle-lp
glme_pred_dig_lp <- varpred(glme_mod
	, "age"
	, isolate=FALSE
	, bias.adjust="diggle"
	, sigma="lp"
	, modelname="dig-lp"
)
est_prop_dig_lp <- data.frame(fit=mean(glme_pred_dig_lp$preds$fit), model="dig-lp")

## diggle-mod
glme_pred_dig_mod <- varpred(glme_mod
	, "age"
	, isolate=FALSE
	, bias.adjust="diggle"
	, sigma="mod"
	, modelname="dig-mod"
)
est_prop_dig_mod <- data.frame(fit=mean(glme_pred_dig_mod$preds$fit), model="dig-mod")

## pop
glme_pred_pop <- varpred(glme_mod
	, "age"
	, isolate=FALSE
	, bias.adjust="population"
	, modelname="pop"
	, include.re=TRUE
)
est_prop_pop <- data.frame(fit=mean(glme_pred_pop$preds$fit), model="pop")

## Bins 
binned_df <- binfun(glme_mod, focal="age", bins=50, groups=NULL)

## Combine preds
vlist <- list(glme_pred_none
#	, glme_pred_delta_lp
#	, glme_pred_delta_mod
#	, glme_pred_mc_lp
#	, glme_pred_mc_mod
#	, glme_pred_dig_lp
#	, glme_pred_dig_mod
	, glme_pred_pop
)

glme_plots <- (comparevarpred(vlist=vlist
		, lnames=NULL
		, plotit=TRUE
		, addmarginals=FALSE
		, ci=FALSE
	)
	+ geom_point(data=binned_df, aes(x=age, y=status), colour="grey")
	+ geom_hline(data=true_prop_df, aes(yintercept=status), lty=1, col="black")
	+ geom_hline(data=est_prop_none, aes(yintercept=fit, colour=model), lty=2)
#	+ geom_hline(data=est_prop_delta_lp, aes(yintercept=fit, colour=model), lty=2)
#	+ geom_hline(data=est_prop_delta_mod, aes(yintercept=fit, colour=model), lty=2)
#	+ geom_hline(data=est_prop_mc_lp, aes(yintercept=fit, colour=model), lty=2)
#	+ geom_hline(data=est_prop_mc_mod, aes(yintercept=fit, colour=model), lty=2)
#	+ geom_hline(data=est_prop_dig_lp, aes(yintercept=fit, colour=model), lty=2)
#	+ geom_hline(data=est_prop_dig_mod, aes(yintercept=fit, colour=model), lty=2)
	+ geom_hline(data=est_prop_pop, aes(yintercept=fit, colour=model), lty=2)
	+ geom_vline(xintercept=est_focal_mean, col="black")
	+ labs(y="Predictions", colour="Method")
	+ theme(legend.position="bottom")
)
glme_plots

## Compare with emmeans
age_at <- quantile(glme_sim_df$age, seq(0,1,length.out=100))
glme_varpred_emmeans_plots <- combinepreds(glme_mod, c("emmeans", "varpred")
	, focal = "age"
	, x.var = "age"
	, at=list(age=age_at)
	, xlevel=list(age=age_at)
	, nesting=NULL
	, type="response"
	, weights="proportional"
	, plotit = TRUE
	, ci=FALSE
)
glme_varpred_emmeans_plots

saveEnvironment()

