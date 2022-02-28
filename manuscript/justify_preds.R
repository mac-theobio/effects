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
x1_truepred_df <- truepredfun(justify_mod, "x1", true_betas=justify_sim_betas, modelname="observed")

justify_plots <- (combinepreds(justify_mod
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
	+ labs(title="A) Model 1", colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
)

## Add CIs
x2_focal <- quantile(justify_sim_df$x2, quants, names=FALSE)
x2_truepred_df <- truepredfun(justify_mod, "x2", true_betas=justify_sim_betas, modelname="observed")
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
)

### Figure 2
pdf("justify_ci_plots-figure2.pdf", height = 5.3)
print(justify_ci_plots)
dev.off()

## With interactions
quants <- seq(0,1,length.out=100)
x1_focal <- quantile(justify_inter_sim_df$x1, quants, names=FALSE)
binned_df <- binfun(justify_inter_mod, focal="x1", bins=20, groups=NULL)
x1_truepred_df <- truepredfun(justify_inter_mod, "x1", true_betas=justify_inter_sim_betas, modelname="observed")

justify_inter_plots <- (combinepreds(justify_inter_mod
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
	+ labs(title="B) Model 2", colour="Method", linetype="Method")
	+ theme(legend.position="bottom")
)
print(justify_inter_plots)

justify_plots <- ggarrange(justify_plots
	, justify_inter_plots + rremove("ylab")
	, common.legend=TRUE
	, legend="bottom"
	, ncol=2
)
justify_plots

## Figure 1
pdf("justify_plots-figure1.pdf", height = 5.3)
print(justify_plots)
dev.off()

