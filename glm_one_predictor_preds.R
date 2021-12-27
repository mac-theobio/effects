library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(emmeans)

loadEnvironments()
startGraphics()

## No bias adjustment
glm_pred_none <- varpred(glm_mod
	, "age"
	, isolate=TRUE
	, bias.adjust="none"
	, modelname="none"
)
glm_pred_none_mean <- mean(glm_pred_none$preds$fit)

## Bias adjusted
glm_pred_pop <- varpred(glm_mod
	, "age"
	, steps=nrow(glm_sim_df)
	, isolate=TRUE
	, bias.adjust="population"
	, modelname="bias corrected"
)
glm_pred_none_pop <- mean(glm_pred_pop$preds$fit)

## Bins 
binned_df <- binfun(glm_mod, focal="age", bins=50, groups=NULL)

## Combine the predictions
vlist <- list(glm_pred_none, glm_pred_pop)

glm_plots <- (combinevarpred(vlist, plotit=TRUE, ci=FALSE)
	+ geom_hline(aes(yintercept=glm_pred_none_mean, colour="none", lty="none"))
	+ geom_hline(aes(yintercept=glm_pred_none_pop, colour="bias corrected", lty="bias corrected"))
	+ geom_vline(aes(xintercept=mean(age)), lty=2, col="grey")
	+ geom_point(data=binned_df, aes(x=age, y=status), colour="blue")
	+ geom_hline(data=true_prop_df, aes(yintercept=status), lty=3, col="blue")
	+ labs(y="Probability of improved water quality", colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
)

print(glm_plots)

## Compare with emmeans
age_at <- quantile(glm_sim_df$age, seq(0,1,length.out=100))
glm_varpred_emmeans_plots <- combinepreds(glm_mod, c("emmeans", "varpred")
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
glm_varpred_emmeans_plots

saveVars(glm_plots, glm_pred_none_mean, glm_pred_none_pop)

