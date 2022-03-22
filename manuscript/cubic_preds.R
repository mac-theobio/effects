library(shellpipes)
library(vareffects)
library(emmeans)
library(effects)
library(dplyr)
library(ggsci)
library(ggplot2)

loadEnvironments()
startGraphics()

## Binned observations
bi_bin <- binfun(bi_df, "age", bins=100)
head(bi_bin)

multi_bin <- binfun(multi_df, "age", bins=100)
head(multi_bin)

## Predictions
### Bi-variate
bi_all <- list(
	varpred(bi_mod, "age", true.beta=true_beta_bi, bias.adjust="population", modelname="truth")
	, varpred(bi_mod, "age", bias.adjust="none", modelname="none")
	, varpred(bi_mod, "age", bias.adjust="population", modelname="sample")
)

### Compare with emmeans and effects
quants <- seq(0, 1, length.out=100)
age_focal <- quantile(bi_df$age, quants, names=FALSE)
#### emmeans
bi_em <- as.data.frame(emmip(bi_mod, ~age, at=list(age=age_focal), type="response", plotit=FALSE))
#### Effects
bi_ef <- as.data.frame(Effect("age", bi_mod, xlevels=list(age=age_focal)))

bi_plot <- (combinevarpred(bi_all, plotit=TRUE, ci=FALSE)
	+ geom_point(data = bi_bin, aes(x=age, y=hhsize), colour="black")
	+ geom_line(data=bi_ef, aes(x=age, y=fit, colour="effect", linetype="effect"))
	+ geom_line(data=bi_em, aes(x=age, y=yvar, colour="emmeans", linetype="emmeans"))
	+ scale_color_uchicago()
	+ theme_bw()
)
teeGG(bi_plot, desc="Bi-variate")

### Multi-variate
multi_all <- list(
	varpred(multi_mod, "age", true.beta=true_beta_multi, bias.adjust="population", modelname="truth")
	, varpred(multi_mod, "age", bias.adjust="none", modelname="none")
	, varpred(multi_mod, "age", bias.adjust="population", modelname="sample")
)
age_focal <- quantile(multi_df$age, quants, names=FALSE)
#### emmeans
multi_em <- as.data.frame(emmip(multi_mod, ~age, at=list(age=age_focal), type="response", plotit=FALSE))
#### Effects
multi_ef <- as.data.frame(Effect("age", multi_mod, xlevels=list(age=age_focal)))

multi_plot <- (combinevarpred(multi_all, plotit=TRUE, ci=FALSE)
	+ geom_point(data = multi_bin, aes(x=age, y=hhsize), colour="black")
	+ geom_line(data=multi_ef, aes(x=age, y=fit, colour="effect", linetype="effect"))
	+ geom_line(data=multi_em, aes(x=age, y=yvar, colour="emmeans", linetype="emmeans"))
	+ scale_color_uchicago()
	+ theme_bw()
)
teeGG(multi_plot, desc="Multi-variate")

## Higher order non-focal predictor
multi_bin <- binfun(multi_df, "wealthindex", bins=100)
multi_all <- list(
	varpred(multi_mod, "wealthindex", true.beta=true_beta_multi, bias.adjust="population", modelname="truth")
	, varpred(multi_mod, "wealthindex", bias.adjust="none", modelname="none")
	, varpred(multi_mod, "wealthindex", bias.adjust="population", modelname="sample")
)
wealthindex_focal <- quantile(multi_df$wealthindex, quants, names=FALSE)
#### emmeans
multi_em <- as.data.frame(emmip(multi_mod, ~wealthindex, at=list(wealthindex=wealthindex_focal), type="response", plotit=FALSE))
#### Effects
multi_ef <- as.data.frame(Effect("wealthindex", multi_mod, xlevels=list(wealthindex=wealthindex_focal)))

multi_plot <- (combinevarpred(multi_all, plotit=TRUE, ci=FALSE)
	+ geom_point(data = multi_bin, aes(x=wealthindex, y=hhsize), colour="black")
	+ geom_line(data=multi_ef, aes(x=wealthindex, y=fit, colour="effect", linetype="effect"))
	+ geom_line(data=multi_em, aes(x=wealthindex, y=yvar, colour="emmeans", linetype="emmeans"))
	+ scale_color_uchicago()
	+ theme_bw()
)
teeGG(multi_plot, desc="Non-focal (is polynomial)")
