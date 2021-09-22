library(shellpipes)
library(vareffects); varefftheme()
library(ggplot2)

commandEnvironments()
makeGraphics()
 
set.seed(9991)

# Section: 2 Quantities of interest

## Coefficient estimate
qoi_df <- simplesim(N=1000, link_scale=TRUE)
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
	+ annotate('text', x = 0.5, y = 1, label = "Delta(x)==1",parse = TRUE,size=5, colour="red") 
	+ annotate('text', x = -1, y = 2, label = paste0("Delta(y)==", round(coef(qoi_mod1)[[2]],3)),parse = TRUE,size=5, colour="red") 
	+ annotate('text', x = 0, y = 4, label = paste0("Marginal~effect(x)==", round(coef(qoi_mod1)[[2]],3)),parse = TRUE,size=5, colour="blue") 
	+ coord_cartesian(expand=FALSE)
	+ labs(y="Predicted household size")
)

saveEnvironment()
