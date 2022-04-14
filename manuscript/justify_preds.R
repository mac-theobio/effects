library(shellpipes)
library(vareffects); varefftheme()
library(ggplot2)
library(dplyr)
library(ggpubr)
library(emmeans)
library(effects)

loadEnvironments()
startGraphics()

## No interactions
quants <- seq(0,1,length.out=100)
x1_focal <- quantile(justify_sim_df$x1, quants, names=FALSE)
binned_df <- binfun(justify_mod, focal="x1", bins=20, groups=NULL)
## Preds based on true parameter values
x1_truepred_df <- varpred(justify_mod, "x1", true.bata=justify_sim_betas, bias.adjust="population", modelname="observed")$preds

simple_plot <- (combinepreds(justify_mod
		, c("varpred", "emmeans", "Effect")
		, focal="x1"
		, x.var="x1"
		, at=list(x1=x1_focal)
		, type="response"
		, nesting=NULL
		, ci=FALSE
	)
	+ geom_hline(data=true_prop_df, aes(yintercept=y, colour="observed"), lty=4)
	+ geom_vline(xintercept=mean(x1_focal), lty=2, col="grey")
	+ geom_point(data=binned_df, aes(x=x1, y=y), colour="grey")
	+ geom_line(data=x1_truepred_df, aes(x=x1, y=fit), lty=4, colour="red")
	+ scale_colour_manual(breaks = c("observed", "emmeans", "Effect", "varpred")
		, values=c("observed"="red", "emmeans"="blue", "Effect"="green", "varpred"="black")
		, labels=c("observed", "emmeans", "effects", "varpred")
	)
	+ scale_linetype_manual(values=c("observed"=4, "emmeans"=2, "Effect"=3, "varpred"=1)
		, labels=c("observed", "emmeans", "effects", "varpred")
	)
	+ labs(title="No interaction", colour="Method", linetype="Method")
#	+ labs(title="A) Model 1", colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
)

## With interactions
quants <- seq(0,1,length.out=100)
x1_focal <- quantile(justify_inter_sim_df$x1, quants, names=FALSE)
binned_df <- binfun(justify_inter_mod, focal="x1", bins=20, groups=NULL)
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
	+ geom_hline(data=true_prop_inter_df, aes(yintercept=y, colour="observed"), lty=4)
	+ geom_vline(xintercept=mean(x1_focal), lty=2, col="grey")
	+ geom_point(data=binned_df, aes(x=x1, y=y), colour="grey")
	+ geom_line(data=x1_truepred_df, aes(x=x1, y=fit), lty=4, colour="red")
	+ scale_colour_manual(breaks = c("observed", "emmeans", "Effect", "varpred")
		, values=c("observed"="red", "emmeans"="blue", "Effect"="green", "varpred"="black")
		, labels=c("observed", "emmeans", "effects", "varpred")
	)
	+ scale_linetype_manual(values=c("observed"=4, "emmeans"=2, "Effect"=3, "varpred"=1)
		, labels=c("observed", "emmeans", "effects", "varpred")
	)
	+ labs(title="Interaction", colour="Method", linetype="Method")
#	+ labs(title="B) Model 2", colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
)

ggarrange(simple_plot
	, inter_plot + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
) %>% teeGG(desc="inter", height=4.0)

## Isolated with CIs
### Preds based on true parameter values
x2_truepred_df <- varpred(justify_mod, "x2", true.beta=justify_sim_betas, bias.adjust="population", modelname="observed")$preds
x2_focal <- quantile(justify_sim_df$x2, quants, names=FALSE)
justify_ci_plots <- (combinepreds(justify_mod
		, c("varpred", "emmeans", "Effect")
		, focal="x2"
		, x.var="x2"
		, at=list(x2=x2_focal)
		, type="response"
		, nesting=NULL
		, ci=TRUE
	)
	+ geom_hline(data=true_prop_df, aes(yintercept=y, colour="observed"), lty=4)
	+ geom_vline(xintercept=mean(x2_focal), lty=2, col="grey")
	+ geom_line(data=x2_truepred_df, aes(x=x2, y=fit), lty=4, colour="red")
	+ scale_colour_manual(breaks = c("observed", "emmeans", "Effect", "varpred")
		, values=c("observed"="red", "emmeans"="blue", "Effect"="green", "varpred"="black")
		, labels=c("observed", "emmeans", "effects", "varpred")
	)
	+ scale_linetype_manual(values=c("observed"=4, "emmeans"=2, "Effect"=3, "varpred"=1)
		, labels=c("observed", "emmeans", "effects", "varpred")
	)
	+ labs(colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
) %>% teeGG(desc="isolate", height=5.3)

print(justify_ci_plots)
