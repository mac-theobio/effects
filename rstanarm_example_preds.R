library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(rstanarm)

loadEnvironments()
startGraphics()

## No bias adjustment
rstan_pred_none <- varpred(rstan_mod
	, "x3"
	, isolate=FALSE
	, bias.adjust="none"
	, modelname="none"
)
est_prop_none <- data.frame(fit=mean(rstan_pred_none$preds$fit), model="none")
est_focal_mean <- mean(rstan_pred_none$preds$x3)

## pop: not adjusted for re
rstan_pred_pop <- varpred(rstan_mod
	, "x3"
	, isolate=TRUE
	, bias.adjust="population"
	, modelname="bias corrected"
	, include.re=FALSE
)
est_prop_pop <- data.frame(fit=mean(rstan_pred_pop$preds$fit), model="bias corrected")

## pop: include re
rstan_pred_pop_re <- varpred(rstan_mod
	, "x3"
	, isolate=TRUE
	, bias.adjust="population"
	, modelname="bias corrected (+re)"
	, include.re=TRUE
)
est_prop_pop_re <- data.frame(fit=mean(rstan_pred_pop_re$preds$fit), model="bias corrected")

## Bins 
binned_df <- binfun(rstan_mod, focal="x3", bins=50, groups=NULL)

## Combine preds
vlist <- list(rstan_pred_none
	, rstan_pred_pop
	, rstan_pred_pop_re
)

## Posterior predict method
new_df <- (rstan_sim_df
	%>% mutate_at(c("x1", "x2"), mean)
	%>% select(c("x1", "x2", "x3"))
	%>% distinct(x1, x2, x3)
	%>% data.frame()
)
pred_rstan <- posterior_epred(rstan_mod, newdata = new_df, re.form = NA)
pred_rstan <- apply(pred_rstan,2, function(x)quantile(x, c(0.025, 0.5, 0.975)))
pred_rstan <- t(pred_rstan)
colnames(pred_rstan) <- c("lwr", "fit", "upr")
pred_rstan <- as.data.frame(pred_rstan)
pred_rstan$x3 <- new_df$x3
pred_rstan$model <- "postpred"
pred_rstan$se <- NA

## Observed means
observed_df <- (rstan_sim_df
	%>% group_by(x3)
	%>% summarise(fit=mean(y))
	%>% mutate(model="observed", lwr=fit, upr=fit,se=NA)
)

### Combine the predictions
pred_vpred <- combinevarpred(vlist, plotit=FALSE)
pred_vpred$preds <- do.call("rbind", list(pred_vpred$preds, observed_df, pred_rstan))
rstan_plots <- (plot(pred_vpred)
	+ scale_colour_manual(breaks = c("observed", "none", "postpred", "bias corrected", "bias corrected (+re)")
		, values=c("observed"="grey", "none"="red", "postpred"="orange", "bias corrected"="blue", "bias corrected (+re)"="black")
	)
	+ labs(colour="Method")
)
print(rstan_plots)

saveVars(rstan_plots)

