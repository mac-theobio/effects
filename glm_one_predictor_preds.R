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
)

## Bins 
binned_df <- binfun(glm_mod, focal="age", bins=50, groups=NULL)

glm_plots <- (plot(glm_pred_none)
	+ geom_point(data=binned_df, aes(x=age, y=status), colour="red")
	+ geom_hline(data=true_prop_df, aes(yintercept=status), lty=2, col="red")
	+ geom_hline(aes(yintercept=mean(fit)), lty=1, col="black")
	+ geom_vline(aes(xintercept=mean(age)), lty=2, col="grey")
	+ labs(y="Probability of improved water quality", colour="Method", linetype="Method")
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

saveVars(glm_plots)

