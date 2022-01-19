library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)

loadEnvironments()
startGraphics()
 
set.seed(9991)

############################################################################################
# Global variables
nHH_obs <- 100
beta_age <- 0.1 # Focal predictor effect

############################################################################################
# Section: 2 Quantities of interest
## Figure 3
############################################################################################

## Coefficient estimate
qoi_df <- linearsim(nHH=1000, form=~1+x1*x2
	, betas=c(5, 1, 2, 0.5)
	, pgausian=list(p=2, fun=rnorm, mean=c(0.2,0), sd=1)
	, pcat=list(p=0)
	, link_scale = TRUE
)$data
colnames(qoi_df) <- c("age", "wealthindex", "hhsize")
qoi_mod1 <- lm(hhsize~age+wealthindex, qoi_df)
qoi_mod2 <- lm(hhsize~age*wealthindex, qoi_df)

## Prediction plot
qoi_age_pred <- varpred(qoi_mod1, "age")
qoi_age_pred_plot <- (plot(qoi_age_pred, ci=FALSE)
	+ geom_segment(aes(x=0,y=coef(qoi_mod1)[[1]],xend=1,yend=coef(qoi_mod1)[[1]]), colour="blue")
	+ geom_segment(aes(x=min(qoi_age_pred$preds$age), y=coef(qoi_mod1)[[1]]+coef(qoi_mod1)[[2]], xend=1, yend=coef(qoi_mod1)[[1]]+coef(qoi_mod1)[[2]]), linetype=2)
	+ geom_segment(aes(x=min(qoi_age_pred$preds$age), y=coef(qoi_mod1)[[1]], xend=0, yend=coef(qoi_mod1)[[1]]), linetype=2)
	+ geom_segment(aes(x=1,y=coef(qoi_mod1)[[1]],xend=1,yend=coef(qoi_mod1)[[1]]+coef(qoi_mod1)[[2]]), colour="blue")
	+ geom_segment(aes(x=1,y=min(qoi_age_pred$preds$fit),xend=1,yend=coef(qoi_mod1)[[1]]), linetype=2)
	+ geom_segment(aes(x=0,y=min(qoi_age_pred$preds$fit),xend=0,yend=coef(qoi_mod1)[[1]]), linetype=2)
	+ annotate('text', x = 0.5, y = 4.5, label = "Delta(age)==1",parse = TRUE,size=5, colour="red") 
	+ annotate('text', x = -1, y = 5.5, label = paste0("Delta(hhsize)==", round(coef(qoi_mod1)[[2]],3)),parse = TRUE,size=5, colour="red") 
	+ annotate('text', x = 0, y = 6.5, label = paste0("Marginal~effect(age)==", round(coef(qoi_mod1)[[2]],3)),parse = TRUE,size=5, colour="blue") 
	+ coord_cartesian(expand=FALSE)
	+ labs(y="Predicted household size")
)

## Figure 3
pdf("qoi_age_pred_plot-figure3.pdf", height = 5.3)
print(qoi_age_pred_plot)
dev.off()
