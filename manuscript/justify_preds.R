library(shellpipes)
library(vareffects); varefftheme()
library(ggthemes)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(emmeans)
library(effects)

loadEnvironments()
startGraphics()

numBins <- 10
quants <- seq(0,1,length.out=100)
compare_with_others <- FALSE # TRUE to compare with effects/emmeans

## No interactions
x1_focal <- quantile(justify_sim_df$x1, quants, names=FALSE)
binned_df <- binfun(justify_mod, focal="x1", bins=numBins, groups=NULL)
## Preds based on true parameter values
x1_truepred_df <- varpred(justify_mod, "x1", true.bata=justify_sim_betas, bias.adjust="population", modelname="observed")$preds

col_limits <- c("observed", "emmeans", "Effect", "varpred")
col_labels <- c("observed", "emmeans", "effects", "varpred")
simple_plot <- (combinepreds(justify_mod
		, c("varpred", "emmeans", "Effect")
		, focal="x1"
		, x.var="x1"
		, at=list(x1=x1_focal)
		, type="response"
		, nesting=NULL
		, ci=FALSE
	)
	+ geom_line(data=x1_truepred_df, aes(x=x1, y=fit, colour=model, linetype=model), linetype=2)
	+ geom_hline(data=true_prop_df, aes(yintercept=y, colour="observed", linetype="observed"))
	+ geom_vline(xintercept=mean(x1_focal), lty=2, col="grey")
	+ geom_point(data=binned_df, aes(x=x1, y=y), colour="grey")
	+ scale_color_colorblind(limits=col_limits 
		, labels=col_labels
	)
	+ scale_linetype_discrete(limits=col_limits 
		, labels=col_labels
	)
	+ labs(y="y", title="A) No interaction", colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
)

## With interactions
x1_focal <- quantile(justify_inter_sim_df$x1, quants, names=FALSE)
binned_df <- binfun(justify_inter_mod, focal="x1", bins=numBins, groups=NULL)
x1_truepred_df <- varpred(justify_inter_mod, "x1", true.beta=justify_inter_sim_betas, bias.adjust="population", modelname="observed")$preds

inter_plot <- (combinepreds(justify_inter_mod
		, c("varpred", "emmeans", "Effect")
		, focal="x1"
		, x.var="x1"
		, at=list(x1=x1_focal)
		, type="response"
		, nesting=NULL
		, ci=FALSE
	)
	+ geom_hline(data=true_prop_inter_df, aes(yintercept=y, colour="observed", linetype="observed"), alpha=0.7)
	+ geom_vline(xintercept=mean(x1_focal), lty=2, col="grey")
	+ geom_point(data=binned_df, aes(x=x1, y=y), colour="grey")
	+ geom_line(data=x1_truepred_df, aes(x=x1, y=fit, colour=model, linetype=model), lty=2)
	+ scale_color_colorblind(limits=col_limits 
		, labels=col_labels
	)
	+ scale_linetype_discrete(limits=col_limits 
		, labels=col_labels
	)
	+ labs(title="B) Interaction", colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
)

double_plot <- ggarrange(simple_plot
	, inter_plot + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
teeGG(double_plot, desc="inter")

## Isolated with CIs
### Preds based on true parameter values
x2_truepred_df <- varpred(justify_mod, "x2", true.beta=justify_sim_betas, bias.adjust="population", modelname="observed")$preds
x2_focal <- quantile(justify_sim_df$x2, quants, names=FALSE)
justify_ci_df <- (combinepreds(justify_mod
		, c("varpred", "emmeans", "Effect")
		, focal="x2"
		, x.var="x2"
		, at=list(x2=x2_focal)
		, type="response"
		, nesting=NULL
		, ci=TRUE
		, plotit=FALSE
	)
)
pred_means_df <- (justify_ci_df
	%>% group_by(model)
	%>% mutate(fit=mean(fit))
)

if (!compare_with_others) {
	pred_df <- varpred(justify_mod, "x2", isolate=FALSE, modelname="prediction")$preds
	justify_ci_df <- (justify_ci_df
		%>% filter(model=="varpred")
		%>% mutate(model="effect")
		%>% bind_rows(pred_df
			%>% rename(xvar=x2)
		)
	)
	pred_means_df <- (justify_ci_df
		%>% group_by(model)
		%>% mutate(fit=mean(fit))
	)
	col_limits <- c("observed", "prediction", "effect")
	col_labels <- c("observed", "prediction", "effect")
}

justify_ci_plots <- (ggplot(justify_ci_df, aes(x=xvar, colour=model, linetype=model))
	+ geom_line(aes(y=fit))
	+ geom_line(aes(y=lwr))
	+ geom_line(aes(y=upr))
 	+ geom_hline(data=true_prop_df, aes(yintercept=y, linetype="observed", colour="observed"))
 	+ geom_hline(data=pred_means_df, aes(yintercept=fit, linetype=model, colour=model))
	+ geom_vline(xintercept=mean(x2_focal), lty=2, col="grey")
 	+ scale_linetype_discrete(limits=col_limits#[4:2]
 		, labels=col_labels#[4:2]
 	)
 	+ scale_color_colorblind(limits=col_limits#[4:2] 
 		, labels=col_labels#[4:2]
 	)
	+ labs(colour="Method", linetype="Method", x="x2", y="y")
)


teeGG(justify_ci_plots, desc="isolate")

saveVars(justify_ci_plots)
